//===--- Diagnostic.cppm - M2 Language Family Diagnostic Handling ---------===//
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

module;

#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"

export module m2lang.basic:Diagnostic;

export namespace m2lang {

namespace diag {
enum {
#define DIAG(X, Y, Z) X,
#include "Diagnostic.def"
#undef DIAG
};
} // namespace diag

class DiagnosticBuilder;

class DiagnosticsEngine : public llvm::RefCountedBase<DiagnosticsEngine> {
  friend class DiagnosticBuilder;

  /// The location of the current diagnostic that is in flight.
  llvm::SMLoc CurDiagLoc;

  /// The ID of the current diagnostic that is in flight.
  ///
  /// This is set to std::numeric_limits<unsigned>::max() when there is no
  /// diagnostic in flight.
  unsigned CurDiagID;

  enum {
    /// The maximum number of arguments we can hold.
    ///
    /// We currently only support up to 10 arguments (%0-%9).  A single
    /// diagnostic with more than that almost certainly has to be simplified
    /// anyway.
    MaxArguments = 10,
  };

  /// The source manager associated with this diagnostics engine.
  llvm::SourceMgr &SrcMgr;

  /// Number of errors reported
  unsigned NumErrors;

  llvm::SmallVector<llvm::StringRef, MaxArguments> Args;

  void emitDiagnostics();

  void formatDiagnostic(llvm::StringRef DiagStr,
                        llvm::SmallVectorImpl<char> &OutStr) const;

public:
  explicit DiagnosticsEngine(llvm::SourceMgr &SrcMgr);
  DiagnosticsEngine(const DiagnosticsEngine &) = delete;
  DiagnosticsEngine &operator=(const DiagnosticsEngine &) = delete;

  void clear() {
    CurDiagID = std::numeric_limits<unsigned>::max();
    Args.clear();
  }

  DiagnosticBuilder report(llvm::SMLoc Loc, unsigned DiagID);

  unsigned getNumErrors() { return NumErrors; }
};

class DiagnosticBuilder {
  friend DiagnosticsEngine;

  DiagnosticsEngine *Diag;

  explicit DiagnosticBuilder(DiagnosticsEngine *Diag) : Diag(Diag) {}

  void emit() { Diag->emitDiagnostics(); }

public:
  ~DiagnosticBuilder() { emit(); }

  void addArg(llvm::StringRef Arg) const { Diag->Args.push_back(Arg); }
};

inline const DiagnosticBuilder &operator<<(const DiagnosticBuilder &DB,
                                           llvm::StringRef S) {
  DB.addArg(S);
  return DB;
}

inline const DiagnosticBuilder &operator<<(const DiagnosticBuilder &DB,
                                           const char *Str) {
  DB.addArg(Str);
  return DB;
}

} // namespace m2lang

using namespace m2lang;

namespace {
const char *DiagnosticText[] = {
#define DIAG(X, Y, Z) Z,
#include "Diagnostic.def"
#undef DIAG
};
const char *getDiagnosticText(unsigned DiagID) {
  return DiagnosticText[DiagID];
}

llvm::SourceMgr::DiagKind DiagnosticKind[] = {
#define DIAG(X, Y, Z) llvm::SourceMgr::DK_##Y,
#include "Diagnostic.def"
};
llvm::SourceMgr::DiagKind getDiagnosticKind(unsigned DiagID) {
  return DiagnosticKind[DiagID];
}
} // namespace

DiagnosticsEngine::DiagnosticsEngine(llvm::SourceMgr &SrcMgr)
    : SrcMgr(SrcMgr), NumErrors(0) {
  clear();
}

void DiagnosticsEngine::formatDiagnostic(
    llvm::StringRef DiagStr, llvm::SmallVectorImpl<char> &OutStr) const {
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
  const llvm::SourceMgr::DiagKind Kind = getDiagnosticKind(CurDiagID);
  llvm::SmallVector<char, 100> Msg;
  formatDiagnostic(DiagText, Msg);
  SrcMgr.PrintMessage(CurDiagLoc, Kind, Msg);
  if (Kind == llvm::SourceMgr::DK_Error)
    ++NumErrors;
  clear();
}

DiagnosticBuilder DiagnosticsEngine::report(llvm::SMLoc Loc, unsigned DiagID) {
  assert(CurDiagID == std::numeric_limits<unsigned>::max() &&
         "Multiple diagnostics in flight at once!");
  CurDiagLoc = Loc;
  CurDiagID = DiagID;
  return DiagnosticBuilder(this);
}
