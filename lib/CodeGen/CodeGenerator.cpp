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
#include "m2lang/CodeGen/CGModule.h"
#include "m2lang/CodeGen/CGProcedure.h"
#include "m2lang/CodeGen/CGUtils.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

using namespace m2lang;

CodeGenerator *CodeGenerator::create(ASTContext &ASTCtx, llvm::TargetMachine *TM) {
  return new CodeGenerator(ASTCtx, TM);
}

void CodeGenerator::run(CompilationModule *CM, std::string FileName) {
  llvm::LLVMContext Ctx;
  llvm::Module *M = new llvm::Module(FileName, Ctx);
  M->setTargetTriple(TM->getTargetTriple().getTriple());
  M->setDataLayout(TM->createDataLayout());

  CGModule CGM(ASTCtx, M);
  CGM.run(CM);

  M->print(llvm::outs(), nullptr);
}
