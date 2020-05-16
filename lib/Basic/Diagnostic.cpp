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

DiagnosticsEngine::DiagnosticsEngine() {

}

void DiagnosticsEngine::report(unsigned DiagID) {
  llvm::errs() << GetDiagnosticText(DiagID) << "\n";
  ++NumErrors;
}
