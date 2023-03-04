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
#include "m2lang/Basic/LLVM.h"
#include "m2lang/CodeGen/CGModule.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/ValueHandle.h"

namespace llvm {
class BasicBlock;
class FunctionType;
class Function;
} // namespace llvm

namespace m2lang {

/*
 * The idea is to follow
 * https://compilers.cs.uni-saarland.de/projects/ssaconstr/ See also
 * https://compilers.cs.uni-saarland.de/papers/bbhlmz13cc.pdf
 */

class CGProcedure {
  CGModule &CGM;
  llvm::IRBuilder<> Builder;

  llvm::BasicBlock *Curr;
  llvm::BasicBlock *BBforExit;

  Procedure *Proc;
  llvm::FunctionType *Fty;
  llvm::Function *Fn;

  struct BasicBlockDef {
    // Maps the variable (or formal parameter) to its definition.
    llvm::DenseMap<Declaration *, llvm::TrackingVH<llvm::Value>> Defs;
    // Set of incompleted phi instructions.
    llvm::DenseMap<llvm::PHINode *, Declaration *> IncompletePhis;
    // Block is sealed, that is, no more predecessors will be added.
    unsigned Sealed : 1;

    BasicBlockDef() : Sealed(0) {}
  };

  llvm::DenseMap<llvm::BasicBlock *, BasicBlockDef> CurrentDef;

  void writeLocalVariable(llvm::BasicBlock *BB, Declaration *Decl, llvm::Value *Val);
  llvm::Value *readLocalVariable(llvm::BasicBlock *BB, Declaration *Decl);
  llvm::Value *readLocalVariableRecursive(llvm::BasicBlock *BB, Declaration *Decl);
  llvm::Value *addPhiOperands(llvm::BasicBlock *BB, Declaration *Decl,
                      llvm::PHINode *Phi);
  llvm::Value *tryRemoveTrivialPhi(llvm::PHINode *Phi);
  void sealBlock(llvm::BasicBlock *BB);

  void writeVariable(llvm::BasicBlock *BB, Declaration *Decl, llvm::Value *Val);
  llvm::Value *readVariable(llvm::BasicBlock *BB, Declaration *Decl);

  // TODO There must be a better solution for VAR arguments.
  llvm::DenseMap<FormalParameter *, llvm::Argument *> FormalParams;

private:
  llvm::LLVMContext &getContext() { return CGM.getLLVMCtx(); }

  void setCurr(llvm::BasicBlock *BB) {
    Curr = BB;
    Builder.SetInsertPoint(Curr);
  }

  llvm::BasicBlock *createBasicBlock(const Twine &Name,
                                     llvm::BasicBlock *InsertBefore = nullptr) {
    return llvm::BasicBlock::Create(getContext(), Name, Fn, InsertBefore);
  }

  llvm::Type *mapType(FormalParameter *Param);
  llvm::Type *mapType(Declaration *Decl);
  llvm::FunctionType *createFunctionType(Procedure *Proc);
  llvm::Function *createFunction(Procedure *Proc, llvm::FunctionType *FTy);

  llvm::Value *emitInfixExpr(InfixExpression *E);
  llvm::Value *emitPrefixExpr(PrefixExpression *E);
  llvm::Value *emitExpr(Expression *E);
  void emitAssign(AssignmentStatement *Stmt);
  void emitCall(ProcedureCallStatement *Stmt);
  void emitIf(IfStatement *Stmt);
  void emitCase(CaseStatement *Stmt);
  void emitWhile(WhileStatement *Stmt);
  void emitRepeat(RepeatStatement *Stmt);
  void emitFor(ForStatement *Stmt);
  void emitLoop(LoopStatement *Stmt);
  void emitWith(WithStatement *Stmt);
  void emitExit(ExitStatement *Stmt);
  void emitReturn(ReturnStatement *Stmt);
  void emitRetry(RetryStatement *Stmt);
  void emitStatements(const StatementList &Stmts);

public:
  CGProcedure(CGModule &CGM) : CGM(CGM), Builder(CGM.getLLVMCtx()) {}
  void run(Procedure *Proc);
};

} // namespace m2lang
#endif