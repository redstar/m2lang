//===--- CGProcedure.cpp - Code Generator for Procedures --------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the code generator implementation for procedures.
///
//===----------------------------------------------------------------------===//

#include "m2lang/CodeGen/CGProcedure.h"
#include "m2lang/CodeGen/CGUtils.h"
#include "llvm/IR/IRBuilder.h"

using namespace m2lang;

void CGProcedure::run(Procedure *Proc) {
  llvm::Type *Int32Ty = llvm::Type::getInt32Ty(getContext());
  llvm::Constant *Int32Zero = llvm::ConstantInt::get(Int32Ty, 0, true);

  auto Fty = llvm::FunctionType::get(Int32Ty, {Int32Ty}, false);
  auto Fn = llvm::Function::Create(Fty, llvm::GlobalValue::ExternalLinkage,
                                   utils::mangleName(Proc), M);
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(getContext(), "entry", Fn);

  auto Res = CurrentDef.try_emplace(BB);
  assert(!Res.second && "Could not insert new basic block");
  VariableValueMap &defs = Res.first->getSecond();

  llvm::IRBuilder<> Builder(BB);
  Builder.CreateRet(Int32Zero);
}
