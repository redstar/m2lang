//===--- Diagnostic.h - M2 Language Family Diagnostic Handling --*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the Diagnostic-related interfaces.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_BASIC_DIAGNOSTC_H
#define M2LANG_BASIC_DIAGNOSTC_H

#include "m2lang/Basic/LLVM.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"

namespace m2lang {

namespace diag {
enum {
#define DIAG(X, Y, Z) X,
#include "m2lang/Basic/Diagnostic.def"
#undef DIAG
};
} // namespace diag

class DiagnosticBuilder;

class DiagnosticsEngine : public RefCountedBase<DiagnosticsEngine> {
  friend class DiagnosticBuilder;

  /// The location of the current diagnostic that is in flight.
  SMLoc CurDiagLoc;

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
  SourceMgr &SrcMgr;

  /// Number of errors reported
  unsigned NumErrors;

  SmallVector<StringRef, MaxArguments> Args;

  void emitDiagnostics();

  void formatDiagnostic(StringRef DiagStr, SmallVectorImpl<char> &OutStr) const;

public:
  explicit DiagnosticsEngine(SourceMgr &SrcMgr);
  DiagnosticsEngine(const DiagnosticsEngine &) = delete;
  DiagnosticsEngine &operator=(const DiagnosticsEngine &) = delete;

  void clear() {
    CurDiagID = std::numeric_limits<unsigned>::max();
    Args.clear();
  }

  DiagnosticBuilder report(SMLoc Loc, unsigned DiagID);

  unsigned getNumErrors() { return NumErrors; }
};

class DiagnosticBuilder {
  friend DiagnosticsEngine;

  DiagnosticsEngine *Diag;

  explicit DiagnosticBuilder(DiagnosticsEngine *Diag) : Diag(Diag) {}

  void emit() { Diag->emitDiagnostics(); }

public:
  ~DiagnosticBuilder() { emit(); }

  void addArg(StringRef Arg) const { Diag->Args.push_back(Arg); }
};

inline const DiagnosticBuilder &operator<<(const DiagnosticBuilder &DB,
                                           StringRef S) {
  DB.addArg(S);
  return DB;
}

inline const DiagnosticBuilder &operator<<(const DiagnosticBuilder &DB,
                                           const char *Str) {
  DB.addArg(Str);
  return DB;
}

} // namespace m2lang

#endif