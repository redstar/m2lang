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
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

using namespace m2lang;

CodeGenerator *CodeGenerator::create(llvm::TargetMachine *TM) {
  return new CodeGenerator(TM);
}

void CodeGenerator::run(CompilationModule *CM, std::string FileName) {
  llvm::LLVMContext Ctx;
  llvm::Module *M = new llvm::Module(FileName, Ctx);
  M->setTargetTriple(TM->getTargetTriple().getTriple());
  M->setDataLayout(TM->createDataLayout());

  llvm::Type *Int32Ty = llvm::Type::getInt32Ty(Ctx);
  llvm::Constant *Int32Zero = llvm::ConstantInt::get(Int32Ty, 0, true);

  ProgramModule *PM = llvm::cast<ProgramModule>(CM);
  for (auto *Decl : PM->getDecls()) {
    if (auto *Var = llvm::dyn_cast<Variable>(Decl)) {
      llvm::GlobalVariable *Msg = new llvm::GlobalVariable(
          *M, Int32Ty,
          /*isConstant=*/false, llvm::GlobalValue::PrivateLinkage, nullptr,
          mangleName(Var));

    } else if (auto *Proc = llvm::dyn_cast<Procedure>(Decl)) {
      auto Fty = llvm::FunctionType::get(Int32Ty, {Int32Ty}, false);
      auto Fn = llvm::Function::Create(Fty, llvm::GlobalValue::ExternalLinkage,
                                       mangleName(Proc), M);
      llvm::BasicBlock *BB = llvm::BasicBlock::Create(Ctx, "entry", Fn);
      llvm::IRBuilder<> Builder(BB);
      Builder.CreateRet(Int32Zero);
    }
  }

  M->print(llvm::outs(), nullptr);
}

std::string CodeGenerator::mangleName(Declaration *Decl) {
  std::string Mangled;
  llvm::SmallString<16> Tmp;
  while (Decl) {
    llvm::StringRef Name = Decl->getName();
    Tmp.clear();
    Tmp.append(llvm::itostr(Name.size()));
    Tmp.append(Name);
    Mangled.insert(0, Tmp.c_str());
    Decl = Decl->getEnclosingDecl();
  }
  Mangled.insert(0, "_m");
  return Mangled;
}