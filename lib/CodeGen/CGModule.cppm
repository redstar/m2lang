//===--- CGModule.cppm - Code Generator for Modules -----------------------===//
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

module;

#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include <cstdint>

export module m2lang.codegen:CGModule;

import :CGProcedure;
import :CGUnit;
import m2lang.ast;

namespace m2lang {
class CGModule : public CGUnit {
public:
  CGModule(ASTContext &ASTCtx, llvm::Module *M) : CGUnit(ASTCtx, M) {}

  void emitDecls(Declaration *Mod, DeclarationList &Decls, const Block &InitBlk,
                 const Block &FinalBlk);
  void run(CompilationModule *CM);
};
} // namespace m2lang

using namespace m2lang;

void CGModule::emitDecls(Declaration *Mod, DeclarationList &Decls,
                         const Block &InitBlk, const Block &FinalBlk) {
  for (auto *Decl : Decls) {
    if (auto *Var = llvm::dyn_cast<Variable>(Decl)) {
      llvm::GlobalVariable *V = new llvm::GlobalVariable(
          *M, convertType(Var->getTypeDenoter()),
          /*isConstant=*/false, llvm::GlobalValue::PrivateLinkage, nullptr,
          utils::mangleName(Var));
      Globals[Var] = V;
    } else if (auto *Proc = llvm::dyn_cast<Procedure>(Decl)) {
      CGProcedure CGP(*this);
      CGP.run(Proc);
    } else if (auto *LM = llvm::dyn_cast<LocalModule>(Decl)) {
      emitDecls(LM, LM->getDecls(), LM->getInitBlk(), LM->getFinalBlk());
    }
  }
  if (!InitBlk.getStmts().empty()) {
    CGProcedure CGP(*this);
    CGP.run(InitBlk, utils::mangleName(Mod, "Init"));
  }
  if (!FinalBlk.getStmts().empty()) {
    CGProcedure CGP(*this);
    CGP.run(FinalBlk, utils::mangleName(Mod, "Final"));
  }
}

void CGModule::run(CompilationModule *CM) {
  ImplementationModule *PM = llvm::cast<ImplementationModule>(CM);
  emitDecls(PM, PM->getDecls(), PM->getInitBlk(), PM->getFinalBlk());
}
