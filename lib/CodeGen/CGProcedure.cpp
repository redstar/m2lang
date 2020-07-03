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
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"

using namespace m2lang;

llvm::Type *CGProcedure::mapType(FormalParameter *Param) {
  // TODO Implement
  return llvm::Type::getInt32Ty(getContext());
}

llvm::FunctionType *CGProcedure::createFunctionType(Procedure *Proc) {
  llvm::Type *ResultTy = llvm::Type::getVoidTy(getContext());
  if (Proc->getResultType()) {
    // ResultTy = mappType(Proc->getResultType());
  }
  auto FormalParams = Proc->getParams();
  llvm::SmallVector<llvm::Type *, 8> ParamTypes;
  for (auto FP : FormalParams) {
    llvm::Type *Ty = mapType(FP);
    ParamTypes.push_back(Ty);
  }
  return llvm::FunctionType::get(ResultTy, ParamTypes, /* IsVarArgs */ false);
}

llvm::Function *CGProcedure::createFunction(Procedure *Proc,
                                            llvm::FunctionType *FTy) {
  llvm::Function *Fn = llvm::Function::Create(
      Fty, llvm::GlobalValue::ExternalLinkage, utils::mangleName(Proc), M);
  // Give parameters a name.
  size_t Idx = 0;
  for (auto I = Fn->arg_begin(), E = Fn->arg_end(); I != E; ++I, ++Idx) {
    llvm::Argument *Arg = I;
    FormalParameter *FP = Proc->getParams()[Idx];
    Arg->setName(FP->getName());
  }
  return Fn;
}

std::pair<llvm::BasicBlock *, CGProcedure::VariableValueMap &>
CGProcedure::createBasicBlock(const Twine &Name,
                              llvm::BasicBlock *InsertBefore) {
  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(getContext(), Name, Fn, InsertBefore);

  auto Res = CurrentDef.try_emplace(BB);
  assert(Res.second && "Could not insert new basic block");
  VariableValueMap &defs = Res.first->getSecond();
  return std::pair<llvm::BasicBlock *, VariableValueMap &>(BB, defs);
}

void CGProcedure::run(Procedure *Proc) {
  llvm::Type *Int32Ty = llvm::Type::getInt32Ty(getContext());
  llvm::Constant *Int32Zero = llvm::ConstantInt::get(Int32Ty, 0, true);

  Fty = createFunctionType(Proc);
  Fn = createFunction(Proc, Fty);
  auto NewBBandDefs = createBasicBlock("entry");
  llvm::BasicBlock *BB = NewBBandDefs.first;
  VariableValueMap &Defs = NewBBandDefs.second;

  // Record values of parameters in the first basic block.
  size_t Idx = 0;
  for (auto I = Fn->arg_begin(), E = Fn->arg_end(); I != E; ++I, ++Idx) {
    llvm::Argument *Arg = I;
    FormalParameter *FP = Proc->getParams()[Idx];
    Defs.insert(std::pair<Declaration *, llvm::Value *>(FP, Arg));
  }

  auto Block = Proc->getBody();
  for (auto Stmt : Block.getStmts()) {

  }
  llvm::IRBuilder<> Builder(BB);
  Builder.CreateRet(Int32Zero);
}
