//===--- CGModule.cpp - Code Generator for Modules --------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the code generator implementation for modules.
///
//===----------------------------------------------------------------------===//

#include "m2lang/CodeGen/CGModule.h"
#include "m2lang/CodeGen/CGProcedure.h"
#include "m2lang/CodeGen/CGUtils.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

using namespace m2lang;

void CGModule::initialize() {
  VoidTy = llvm::Type::getVoidTy(getLLVMCtx());
  Int32Ty = llvm::Type::getInt32Ty(getLLVMCtx());
  Int64Ty = llvm::Type::getInt64Ty(getLLVMCtx());
  Int32Zero = llvm::ConstantInt::get(Int32Ty, 0, /*isSigned*/ true);
}

llvm::Type *CGModule::convertType(TypeDenoter *TYDe) {
  // TODO Implement.
  return Int32Ty;
}

llvm::Type *CGModule::convertType(Type *Ty) {
  return convertType(Ty->getTypeDenoter());
}

void CGModule::run(CompilationModule *CM) {
  ImplementationModule *PM = llvm::cast<ImplementationModule>(CM);
  for (auto *Decl : PM->getDecls()) {
    if (auto *Var = llvm::dyn_cast<Variable>(Decl)) {
      llvm::GlobalVariable *Msg = new llvm::GlobalVariable(
          *M, Int32Ty,
          /*isConstant=*/false, llvm::GlobalValue::PrivateLinkage, nullptr,
          utils::mangleName(Var));

    } else if (auto *Proc = llvm::dyn_cast<Procedure>(Decl)) {
      CGProcedure CGP(*this);
      CGP.run(Proc);
    }
  }
}
