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

//#include "m2lang/Basic/SourceLocation.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"

namespace m2lang {

namespace diag {
  enum {
#define DIAG(X,Y) X,
#include "m2lang/Basic/Diagnostic.def"
#undef DIAG
  };
}

class DiagnosticsEngine : public llvm::RefCountedBase<DiagnosticsEngine> {

  /// Number of errors reported
  unsigned NumErrors;

public:
  explicit DiagnosticsEngine();
  DiagnosticsEngine(const DiagnosticsEngine &) = delete;
  DiagnosticsEngine &operator=(const DiagnosticsEngine &) = delete;
  ~DiagnosticsEngine();

  //void Report(SourceLocation Loc, unsigned DiagID);
  void report(unsigned DiagID);

  unsigned getNumErrors() { return NumErrors; }
};

} // namespace m2lang

#endif