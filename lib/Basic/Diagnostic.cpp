//===-- Diagnostic.cpp - M2 Language Family Diagnostic Handling -*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
//  Implements the Diagnostic-related interfaces.
///
//===----------------------------------------------------------------------===//

#include "m2lang/Basic/Diagnostic.h"
#include "llvm/Support/raw_ostream.h"
#include <string>

using namespace m2lang;

namespace {
const char *DiagnosticText[] = {
#define DIAG(X, Y, Z) Z,
#include "m2lang/Basic/Diagnostic.def"
#undef DIAG
};
const char *getDiagnosticText(unsigned DiagID) {
  return DiagnosticText[DiagID];
}

SourceMgr::DiagKind DiagnosticKind[] = {
#define DIAG(X, Y, Z) SourceMgr::DK_##Y,
#include "m2lang/Basic/Diagnostic.def"
};
SourceMgr::DiagKind getDiagnosticKind(unsigned DiagID) {
  return DiagnosticKind[DiagID];
}
} // namespace

DiagnosticsEngine::DiagnosticsEngine(SourceMgr &SrcMgr)
    : SrcMgr(SrcMgr), NumErrors(0) {
  clear();
}

void DiagnosticsEngine::formatDiagnostic(StringRef DiagStr,
                                         SmallVectorImpl<char> &OutStr) const {
  // Very hacky. A more robust implemantation is required.
  const char *Ptr = DiagStr.begin();
  const char *DiagEnd = DiagStr.end();
  while (Ptr != DiagEnd) {
    if (Ptr[0] != '%') {
      const char *StrEnd = std::find(Ptr, DiagEnd, '%');
      OutStr.append(Ptr, StrEnd);
      Ptr = StrEnd;
      continue;
    } else {
      ++Ptr;
      // Needs a check!
      unsigned ArgNo = *Ptr++ - '0';
      OutStr.append(Args[ArgNo].begin(), Args[ArgNo].end());
    }
  }
}

void DiagnosticsEngine::emitDiagnostics() {
  const char *DiagText = getDiagnosticText(CurDiagID);
  const SourceMgr::DiagKind Kind = getDiagnosticKind(CurDiagID);
  SmallVector<char, 100> Msg;
  formatDiagnostic(DiagText, Msg);
  SrcMgr.PrintMessage(CurDiagLoc, Kind, Msg);
  if (Kind == SourceMgr::DK_Error)
    ++NumErrors;
  clear();
}

DiagnosticBuilder DiagnosticsEngine::report(SMLoc Loc,
                                            unsigned DiagID) {
  assert(CurDiagID == std::numeric_limits<unsigned>::max() &&
         "Multiple diagnostics in flight at once!");
  CurDiagLoc = Loc;
  CurDiagID = DiagID;
  return DiagnosticBuilder(this);
}
