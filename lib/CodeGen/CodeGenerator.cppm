//===--- CodeGenerator.cppm - Modula-2 Language Code Generator ------------===//
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

module;

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include <string>

export module m2lang.codegen;

import :CGUnit;
import :CGModule;
import :CGProcedure;
import :CGTBAA;

import m2lang.ast;

namespace m2lang {

export class CodeGenerator {
  llvm::LLVMContext &Ctx;
  ASTContext &ASTCtx;
  llvm::TargetMachine *TM;
  CompilationModule *CM;

protected:
  CodeGenerator(llvm::LLVMContext &Ctx, ASTContext &ASTCtx,
                llvm::TargetMachine *TM)
      : Ctx(Ctx), ASTCtx(ASTCtx), TM(TM), CM(nullptr) {}

public:
  static CodeGenerator *create(llvm::LLVMContext &Ctx, ASTContext &ASTCtx,
                               llvm::TargetMachine *TM);

  std::unique_ptr<llvm::Module> run(CompilationModule *CM,
                                    std::string FileName);
};

} // end namespace m2lang


using namespace m2lang;

CodeGenerator *CodeGenerator::create(llvm::LLVMContext &Ctx, ASTContext &ASTCtx, llvm::TargetMachine *TM) {
  return new CodeGenerator(Ctx, ASTCtx, TM);
}

std::unique_ptr<llvm::Module> CodeGenerator::run(CompilationModule *CM, std::string FileName) {
  std::unique_ptr<llvm::Module> M = std::make_unique<llvm::Module>(FileName, Ctx);
  M->setTargetTriple(TM->getTargetTriple().getTriple());
  M->setDataLayout(TM->createDataLayout());

  CGModule CGM(ASTCtx, M.get());
  CGM.run(CM);
  llvm::verifyModule(*M, &llvm::errs());
  //M->print(llvm::outs(), nullptr);
  return M;
}
