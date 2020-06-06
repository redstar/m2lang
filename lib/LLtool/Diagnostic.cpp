//===--- Diag.h - LLtool diagnostic output ----------------------*- C++ -*-===//
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

#include "lltool/Diagnostic.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"

using namespace lltool;

void Diagnostic::error(llvm::SMLoc Loc, const llvm::Twine &Msg) {
  SrcMgr.PrintMessage(Loc, llvm::SourceMgr::DK_Error, Msg);
  ++Errors;
}

void Diagnostic::error(const char *Loc, const llvm::Twine &Msg) {
  error(llvm::SMLoc::getFromPointer(Loc), Msg);
}

void Diagnostic::warning(llvm::SMLoc Loc, const llvm::Twine &Msg) {
  SrcMgr.PrintMessage(Loc, llvm::SourceMgr::DK_Warning, Msg);
}

void Diagnostic::warning(const char *Loc, const llvm::Twine &Msg) {
  warning(llvm::SMLoc::getFromPointer(Loc), Msg);
}

void Diagnostic::note(llvm::SMLoc Loc, const llvm::Twine &Msg) {
  SrcMgr.PrintMessage(Loc, llvm::SourceMgr::DK_Note, Msg);
}

void Diagnostic::note(const char *Loc, const llvm::Twine &Msg) {
  note(llvm::SMLoc::getFromPointer(Loc), Msg);
}
