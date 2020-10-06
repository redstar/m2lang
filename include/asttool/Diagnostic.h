//===--- Diagnostic.h - ASTtool diagnostic output ---------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the error printing interface.
///
//===----------------------------------------------------------------------===//

#ifndef ASTTOOL_DIAGNOSTIC_H
#define ASTTOOL_DIAGNOSTIC_H

#include "llvm/Support/SMLoc.h"

namespace llvm {
class SourceMgr;
class Twine;
} // namespace llvm

namespace asttool {

class Diagnostic {
  llvm::SourceMgr &SrcMgr;
  unsigned Errors;

public:
  Diagnostic(llvm::SourceMgr &SrcMgr) : SrcMgr(SrcMgr), Errors(0) {}

  llvm::SourceMgr &srcMgr() { return SrcMgr; }

  bool errorsOccured() { return Errors > 0; }
  unsigned errorsPrinted() { return Errors; }

  void error(llvm::SMLoc Loc, const llvm::Twine &Msg);
  void error(const char *Loc, const llvm::Twine &Msg);
  void warning(llvm::SMLoc Loc, const llvm::Twine &Msg);
  void warning(const char *Loc, const llvm::Twine &Msg);
  void note(llvm::SMLoc Loc, const llvm::Twine &Msg);
  void note(const char *Loc, const llvm::Twine &Msg);
};
} // namespace asttool
#endif