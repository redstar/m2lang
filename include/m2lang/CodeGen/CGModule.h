//===--- CGModule.h - Code Generator for Modules ----------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the code generator interface for moduls.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_CODEGEN_CGMODULE_H
#define M2LANG_CODEGEN_CGMODULE_H

#include "m2lang/AST/AST.h"
#include "m2lang/AST/ASTContext.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

namespace llvm {
class Constant;
class Type;
} // namespace llvm

namespace m2lang {

class CGModule {
  ASTContext &ASTCtx;
  llvm::Module *M;

public:
  llvm::Type *VoidTy;
  llvm::Type *Int32Ty;
  llvm::Type *Int64Ty;
  llvm::Constant *Int32Zero;

public:
  CGModule(ASTContext &ASTCtx, llvm::Module *M) : ASTCtx(ASTCtx), M(M) {
    initialize();
  }

  void initialize();

  llvm::LLVMContext &getLLVMCtx() { return M->getContext(); }
  llvm::Module *getModule() { return M; }

  ASTContext &getASTCtx() { return ASTCtx; }

  llvm::Type *convertType(TypeDenoter *TyDe);
  llvm::Type *convertType(Type *Ty);

  void run(CompilationModule *CM);
};

} // namespace m2lang
#endif