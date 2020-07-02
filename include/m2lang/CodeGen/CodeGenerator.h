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
#include "llvm/Target/TargetMachine.h"
#include <string>

namespace m2lang {

class CodeGenerator {
  llvm::TargetMachine *TM;
  CompilationModule *CM;

protected:
  CodeGenerator(llvm::TargetMachine *TM) : TM(TM), CM(nullptr) {}

public:
  static CodeGenerator *create(llvm::TargetMachine *TM);

  void run(CompilationModule *CM, std::string FileName);
};

} // end namespace m2lang

#endif