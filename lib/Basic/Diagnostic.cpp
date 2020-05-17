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
#include "llvm/Support/raw_os_ostream.h"
#include <string>

using namespace m2lang;

namespace {
  const char* DiagnosticText[] = {
#define DIAG(X,Y) Y,
#include "m2lang/Basic/Diagnostic.def"
#undef DIAG
  };
  const char* GetDiagnosticText(unsigned DiagID) {
    return DiagnosticText[DiagID];
  }

}

DiagnosticsEngine::DiagnosticsEngine() : NumErrors(0) { clear(); }

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
    }
    else {
      ++Ptr;
      // Needs a check!
      unsigned ArgNo = *Ptr++ - '0';
      OutStr.append(Args[ArgNo].begin(), Args[ArgNo].end());
    }
  }
}

void DiagnosticsEngine::emitDiagnostics() {
  const char *DiagText = GetDiagnosticText(CurDiagID);
  SmallVector<char, 100> Out;
  formatDiagnostic(DiagText, Out);
  llvm::errs() << "Error: " << Out << "\n";
  ++NumErrors;
  clear();
}

DiagnosticBuilder DiagnosticsEngine::report(SourceLocation Loc,
                                            unsigned DiagID) {
  assert(CurDiagID == std::numeric_limits<unsigned>::max() &&
         "Multiple diagnostics in flight at once!");
  CurDiagLoc = Loc;
  CurDiagID = DiagID;
  return DiagnosticBuilder(this);
}
