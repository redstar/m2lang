//===--- CodeGenerator.cpp - Modula-2 Language Code Generator ---*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the code generator implementation.
///
//===----------------------------------------------------------------------===//

#include "m2lang/CodeGen/CodeGenerator.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

using namespace m2lang;

CodeGenerator *CodeGenerator::create(llvm::TargetMachine *TM) {
  return new CodeGenerator(TM);
}

void CodeGenerator::run(CompilationModule *CM) {
  llvm::LLVMContext Ctx;
  llvm::Module *M = new llvm::Module("", Ctx);
  M->setTargetTriple(TM->getTargetTriple().getTriple());
  M->setDataLayout(TM->createDataLayout());

  llvm::Type *Int32Ty = llvm::Type::getInt32Ty(Ctx);

  ProgramModule *PM = llvm::cast<ProgramModule>(CM);
  for (auto *Decl : PM->getDecls()) {
    if (auto *Var = llvm::dyn_cast<Variable>(Decl)) {
      llvm::GlobalVariable *Msg = new llvm::GlobalVariable(
          *M, Int32Ty,
          /*isConstant=*/false, llvm::GlobalValue::PrivateLinkage, nullptr,
          Var->getName());

    } else if (auto *Proc = llvm::dyn_cast<Procedure>(Decl)) {
      auto Fty = llvm::FunctionType::get(Int32Ty, {Int32Ty}, true);
      auto Fn = llvm::Function::Create(Fty, llvm::GlobalValue::ExternalLinkage,
                                       Proc->getName(), M);
    }
  }

  M->print(llvm::outs(), nullptr);
}
