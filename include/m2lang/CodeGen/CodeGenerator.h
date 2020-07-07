//===--- CodeGenerator.h - Modula-2 Language Code Generator -----*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the code generator interface.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_CODEGEN_CODEGENERATOR_H
#define M2LANG_CODEGEN_CODEGENERATOR_H

#include "m2lang/AST/AST.h"
#include "m2lang/AST/ASTContext.h"
#include "llvm/Target/TargetMachine.h"
#include <string>

namespace m2lang {

class CodeGenerator {
  ASTContext &ASTCtx;
  llvm::TargetMachine *TM;
  CompilationModule *CM;

protected:
  CodeGenerator(ASTContext &ASTCtx, llvm::TargetMachine *TM)
      : ASTCtx(ASTCtx), TM(TM), CM(nullptr) {}

public:
  static CodeGenerator *create(ASTContext &ASTCtx, llvm::TargetMachine *TM);

  void run(CompilationModule *CM, std::string FileName);
};

} // end namespace m2lang

#endif