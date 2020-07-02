//===--- CGProcedure.h - Code Generator for Procedures ----------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the code generator interface for procedures.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_CODEGEN_CGPROCEDURE_H
#define M2LANG_CODEGEN_CGPROCEDURE_H

#include "m2lang/AST/AST.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

namespace m2lang {

/*
 * The idea is to follow
 * https://compilers.cs.uni-saarland.de/projects/ssaconstr/ See also
 * https://compilers.cs.uni-saarland.de/papers/bbhlmz13cc.pdf
 */

class CGProcedure {
  llvm::Module *M;

  using VariableValueMap = llvm::DenseMap<Variable *, llvm::Value *>;
  llvm::DenseMap<llvm::BasicBlock *, VariableValueMap> CurrentDef;

private:
  llvm::LLVMContext &getContext() { return M->getContext(); }

public:
  CGProcedure(llvm::Module *M) : M(M) {}
  void run(Procedure *Proc);
};

} // namespace m2lang
#endif