//===--- ASTContext.h - M2 Language Family AST Context ----------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the context class for the AST.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_AST_ASTCONTEXT_H
#define M2LANG_AST_ASTCONTEXT_H

#include "m2lang/Basic/LangOptions.h"
#include "llvm/Support/SourceMgr.h"

namespace m2lang {

class PervasiveType;

class ASTContext {
  LangOptions &LangOpts;
  llvm::SourceMgr &SrcMgr;

public:
#define BUILTIN_TYPE(Id) PervasiveType *Id##TyDe;
#include "m2lang/AST/PervasiveTypes.def"

public:
  ASTContext(LangOptions &LangOpts, llvm::SourceMgr &SrcMgr)
      : LangOpts(LangOpts), SrcMgr(SrcMgr) {
    initialize();
  }
  void initialize();

  const LangOptions &getLangOpts() const { return LangOpts; }

  llvm::SourceMgr &getSourceMgr() { return SrcMgr; }
  const llvm::SourceMgr &getSourceMgr() const { return SrcMgr; }
};

} // namespace m2lang

#endif