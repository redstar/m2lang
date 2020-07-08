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
#include "llvm/IR/CFG.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"

using namespace m2lang;

void CGProcedure::writeVariable(llvm::BasicBlock *BB, Declaration *Decl,
                                llvm::Value *Val) {
  assert(BB && "Basic block is nullptr");
  assert((llvm::isa<Variable>(Decl) || llvm::isa<FormalParameter>(Decl)) &&
         "Declaration must be variable or formal parameter");
  assert(Val && "Value is nullptr");
  CurrentDef[BB].Defs[Decl] = Val;
}

llvm::Value *CGProcedure::readVariable(llvm::BasicBlock *BB,
                                       Declaration *Decl) {
  assert(BB && "Basic block is nullptr");
  assert((llvm::isa<Variable>(Decl) || llvm::isa<FormalParameter>(Decl)) &&
         "Declaration must be variable or formal parameter");
  auto Val = CurrentDef[BB].Defs.find(Decl);
  if (Val != CurrentDef[BB].Defs.end())
    return Val->second;
  return readVariableRecursive(BB, Decl);
}

llvm::Value *CGProcedure::readVariableRecursive(llvm::BasicBlock *BB,
                                                Declaration *Decl) {
  llvm::Value *Val = nullptr;
  if (!CurrentDef[BB].Sealed) {
    // Add incomplete phi for variable.
    llvm::PHINode *Phi = BB->empty()
        ?  llvm::PHINode::Create(mapType(Decl), 0, "", BB)
        : llvm::PHINode::Create(mapType(Decl), 0, "", &BB->front());
    CurrentDef[BB].incompletePhis[Phi] = Decl;
    Val = Phi;
  } else if (auto *PredBB = BB->getSinglePredecessor()) {
    // Only one predecessor.
    Val = readVariable(PredBB, Decl);
  } else {
    // Create empty phi instruction to break potential cycles.
    llvm::PHINode *Phi = BB->empty()
        ?  llvm::PHINode::Create(mapType(Decl), 0, "", BB)
        : llvm::PHINode::Create(mapType(Decl), 0, "", &BB->front());
    Val = Phi;
    writeVariable(BB, Decl, Val);
    addPhiOperands(BB, Decl, Phi);
  }
  writeVariable(BB, Decl, Val);
  return Val;
}

void CGProcedure::addPhiOperands(llvm::BasicBlock *BB, Declaration *Decl,
                                 llvm::PHINode *Phi) {
  for (auto I = llvm::pred_begin(BB), E = llvm::pred_end(BB); I != E; ++I) {
    Phi->addIncoming(readVariable(*I, Decl), *I);
  }
  tryRemoveTrivialPhi(Phi);
}

void CGProcedure::tryRemoveTrivialPhi(llvm::PHINode *Phi) {
  llvm::Value *Same = nullptr;
  for (llvm::Value *V : Phi->incoming_values()) {
    if (V == Same || V == Phi)
      continue;
    if (Same && V != Same)
      return;
    Same = V;
  }
  if (Same == nullptr)
    Same = llvm::UndefValue::get(Phi->getType());
  // Collect phi instructions using this one.
  llvm::SmallVector<llvm::PHINode*, 8> CandidatePhis;
  for (llvm::Use &U : Phi->uses()) {
    if (auto *P = llvm::dyn_cast<llvm::PHINode>(U.getUser()))
      CandidatePhis.push_back(P);
  }
  Phi->replaceAllUsesWith(Same);
  Phi->eraseFromParent();
  for (auto *P : CandidatePhis)
    tryRemoveTrivialPhi(P);
}

void CGProcedure::sealBlock(llvm::BasicBlock *BB) {
  assert(!CurrentDef[BB].Sealed && "Attempt to seal already sealed block");
  for (auto PhiDecl : CurrentDef[BB].incompletePhis) {
    addPhiOperands(BB, PhiDecl.second, PhiDecl.first);
  }
  CurrentDef[BB].incompletePhis.clear();
  CurrentDef[BB].Sealed = true;
}

llvm::Type *CGProcedure::mapType(FormalParameter *Param) {
  // TODO Implement
  return llvm::Type::getInt64Ty(getContext());
}

llvm::Type *CGProcedure::mapType(Declaration *Param) {
  // TODO Implement
  return llvm::Type::getInt64Ty(getContext());
}

llvm::FunctionType *CGProcedure::createFunctionType(Procedure *Proc) {
  llvm::Type *ResultTy = CGM.VoidTy;
  if (Proc->getResultType()) {
    ResultTy = mapType(Proc->getResultType());
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
  llvm::Function *Fn =
      llvm::Function::Create(Fty, llvm::GlobalValue::ExternalLinkage,
                             utils::mangleName(Proc), CGM.getModule());
  // Give parameters a name.
  size_t Idx = 0;
  for (auto I = Fn->arg_begin(), E = Fn->arg_end(); I != E; ++I, ++Idx) {
    llvm::Argument *Arg = I;
    FormalParameter *FP = Proc->getParams()[Idx];
    Arg->setName(FP->getName());
  }
  return Fn;
}

std::pair<llvm::BasicBlock *, CGProcedure::BasicBlockDef &>
CGProcedure::createBasicBlock(const Twine &Name,
                              llvm::BasicBlock *InsertBefore) {
  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(getContext(), Name, Fn, InsertBefore);

  auto Res = CurrentDef.try_emplace(BB);
  assert(Res.second && "Could not insert new basic block");
  BasicBlockDef &defs = Res.first->getSecond();
  return std::pair<llvm::BasicBlock *, BasicBlockDef &>(BB, defs);
}

llvm::Value *CGProcedure::emitInfixExpr(InfixExpression *E) {
llvm::outs() << "emitInfixExpr\n";
llvm::outs() << "emitInfixExpr - Left\n";
  llvm::Value *Left = emitExpr(E->getLeftExpression());
llvm::outs() << "emitInfixExpr - Right\n";
  llvm::Value *Right = emitExpr(E->getRightExpression());
llvm::outs() << "emitInfixExpr - done\n";
  llvm::Value *Result = nullptr;
  switch (E->getOperatorInfo().getKind()) {
  case tok::plus:
    Result = Builder.CreateNSWAdd(Left, Right);
    break;
  case tok::minus:
    Result = Builder.CreateNSWSub(Left, Right);
    break;
  case tok::star:
    Result = Builder.CreateNSWMul(Left, Right);
    break;
  case tok::slash:
    break;
  case tok::kw_DIV:
    Result = Builder.CreateSDiv(Left, Right);
    break;
  case tok::kw_MOD:
    Result = Builder.CreateSRem(Left, Right);
    break;
  case tok::equal:
    Result = Builder.CreateICmpEQ(Left, Right);
    break;
  case tok::hash:
    Result = Builder.CreateICmpNE(Left, Right);
    break;
  case tok::less:
    Result = Builder.CreateICmpSLT(Left, Right);
    break;
  case tok::lessequal:
    Result = Builder.CreateICmpSLE(Left, Right);
    break;
  case tok::greater:
    Result = Builder.CreateICmpSGT(Left, Right);
    break;
  case tok::greaterequal:
    Result = Builder.CreateICmpSGE(Left, Right);
    break;
  case tok::kw_AND:
    Result = Builder.CreateAnd(Left, Right);
    break;
  case tok::kw_OR:
    Result = Builder.CreateOr(Left, Right);
    break;
  default:
    llvm_unreachable("Wrong operator");
  }
  return Result;
}

llvm::Value *CGProcedure::emitPrefixExpr(PrefixExpression *E) {
llvm::outs() << "emitPrefixExpr\n";
  llvm::Value *Result = emitExpr(E->getExpression());
  switch (E->getOperatorInfo().getKind()) {
  case tok::plus:
    // Identity - nothing to do.
    break;
  case tok::minus:
    Result = Builder.CreateNeg(Result);
    break;
  case tok::kw_NOT:
    Result = Builder.CreateNot(Result);
    break;
  default:
    llvm_unreachable("Wrong operator");
  }
  return Result;
}

llvm::Value *CGProcedure::emitExpr(Expression *E) {
llvm::outs() << "emitExpr\n";
  if (auto *Infix = llvm::dyn_cast<InfixExpression>(E)) {
    return emitInfixExpr(Infix);
  } else if (auto *Prefix = llvm::dyn_cast<PrefixExpression>(E)) {
    return emitPrefixExpr(Prefix);
  } else if (auto *Desig = llvm::dyn_cast<Designator>(E)) {
    Declaration *Decl = Desig->getDecl();
    if (Decl) llvm::outs() << "emitExpr - Designator - Decl " << Decl->getName() << "\n";
    llvm::outs().flush();
    if (llvm::isa_and_nonnull<Variable>(Decl) || llvm::isa_and_nonnull<FormalParameter>(Decl))
      return readVariable(Curr, Decl);
    llvm::outs() << "Unsupported designator\n";
  } else if (auto *IntLit = llvm::dyn_cast<IntegerLiteral>(E)) {
    return llvm::ConstantInt::get(CGM.Int64Ty, IntLit->getValue());
  } else {
    llvm::outs() << "Cannot handle expression\n";
  }
  return nullptr;
}

void CGProcedure::emitAssign(AssignmentStatement *Stmt) {
  llvm::outs() << "emitAssign\n";
  llvm::Value *Right = emitExpr(Stmt->getExpression());
    Declaration *Decl = Stmt->getDesignator()->getDecl();
    if (llvm::isa_and_nonnull<Variable>(Decl) || llvm::isa_and_nonnull<FormalParameter>(Decl))
      writeVariable(Curr, Decl, Right);
}

void CGProcedure::emitCall(ProcedureCallStatement *Stmt) {
  llvm::outs() << "emitCall\n";
}

void CGProcedure::emitIf(IfStatement *Stmt) {
  llvm::outs() << "emitIf\n";

    bool HasElse = false; //Stmt->getElseStmts().size() > 0;

    // Create the required basic blocks.
    llvm::BasicBlock *IfBB =
        llvm::BasicBlock::Create(CGM.getLLVMCtx(), "if.body", Fn);
    //llvm::BasicBlock *ElseBB =
    //    HasElse ? llvm::BasicBlock::Create(CGM.getContext(), "else.body", Fn)
    //            : nullptr;
    llvm::BasicBlock *AfterIfBB =
        llvm::BasicBlock::Create(CGM.getLLVMCtx(), "after.if", Fn);

    llvm::Value *Cond = emitExpr(Stmt->getCond());
    Builder.CreateCondBr(Cond, IfBB, /*HasElse ? ElseBB :*/ AfterIfBB);
    sealBlock(Curr);

    setCurr(IfBB);
    emitStatements(Stmt->getStmts());
    if (!Curr->getTerminator()) {
      Builder.CreateBr(AfterIfBB);
    }
    sealBlock(Curr);
/*
    if (HasElse) {
      setCurr(ElseBB);
      emitStatements(Stmt->getIfStmts());
      if (!Curr->getTerminator()) {
        Builder.CreateBr(AfterIfBB);
      }
    }
  */
    setCurr(AfterIfBB);
}

void CGProcedure::emitWhile(WhileStatement *Stmt) {
  // The basic block for the condition.
  llvm::BasicBlock *WhileCondBB =
      llvm::BasicBlock::Create(CGM.getLLVMCtx(), "while.cond", Fn);
  // The basic block for the while body.
  llvm::BasicBlock *WhileBodyBB =
      llvm::BasicBlock::Create(CGM.getLLVMCtx(), "while.body", Fn);
  // The basic block after the while statement.
  llvm::BasicBlock *AfterWhileBB =
      llvm::BasicBlock::Create(CGM.getLLVMCtx(), "after.while", Fn);

  Builder.CreateBr(WhileCondBB);
  sealBlock(Curr);
  setCurr(WhileCondBB);
  llvm::Value *Cond = emitExpr(Stmt->getCond());
  Builder.CreateCondBr(Cond, WhileBodyBB, AfterWhileBB);

  setCurr(WhileBodyBB);
  emitStatements(Stmt->getStmts());
  Builder.CreateBr(WhileCondBB);
  sealBlock(WhileCondBB);
  sealBlock(WhileBodyBB);

  setCurr(AfterWhileBB);
}

void CGProcedure::emitRepeat(RepeatStatement *Stmt) {
  // The basic block with the repeat body.
  llvm::BasicBlock *RepeatBodyBB =
      llvm::BasicBlock::Create(CGM.getLLVMCtx(), "repeat.body", Fn);
  // The basic block after the while statement.
  llvm::BasicBlock *AfterRepeatBB =
      llvm::BasicBlock::Create(CGM.getLLVMCtx(), "after.repeat", Fn);

  Builder.CreateBr(RepeatBodyBB);
  setCurr(RepeatBodyBB);
  emitStatements(Stmt->getStmts());

  llvm::Value *Cond = emitExpr(Stmt->getCond());
  Builder.CreateCondBr(Cond, RepeatBodyBB, AfterRepeatBB);

  sealBlock(RepeatBodyBB);

  setCurr(AfterRepeatBB);
}

void CGProcedure::emitLoop(LoopStatement *Stmt) {
  // How to handle LOOP .. END without EXIT?
}

void CGProcedure::emitReturn(ReturnStatement *Stmt) {
  if (Stmt->getRetVal()) {
    llvm::Value *RetVal = emitExpr(Stmt->getRetVal());
    Builder.CreateRet(RetVal);
  } else {
    Builder.CreateRetVoid();
  }
}

void CGProcedure::emitStatements(const StatementList &Stmts) {
  llvm::outs() << "emitStatements\n";
  for (auto *S : Stmts) {
    if (auto *Stmt = llvm::dyn_cast<AssignmentStatement>(S))
      emitAssign(Stmt);
    else if (auto *Stmt = llvm::dyn_cast<ProcedureCallStatement>(S))
      emitCall(Stmt);
    else if (auto *Stmt = llvm::dyn_cast<IfStatement>(S))
      emitIf(Stmt);
    else if (auto *Stmt = llvm::dyn_cast<WhileStatement>(S))
      emitWhile(Stmt);
    else if (auto *Stmt = llvm::dyn_cast<RepeatStatement>(S))
      emitRepeat(Stmt);
    else if (auto *Stmt = llvm::dyn_cast<LoopStatement>(S))
      emitLoop(Stmt);
    else if (auto *Stmt = llvm::dyn_cast<ReturnStatement>(S))
      emitReturn(Stmt);
    else
      llvm_unreachable("Unknown statement");
  }
}

void CGProcedure::run(Procedure *Proc) {
  Fty = createFunctionType(Proc);
  Fn = createFunction(Proc, Fty);
  auto NewBBandDefs = createBasicBlock("entry");
  llvm::BasicBlock *BB = NewBBandDefs.first;
  BasicBlockDef &Defs = NewBBandDefs.second;
  setCurr(BB);

  // Record values of parameters in the first basic block.
  size_t Idx = 0;
  for (auto I = Fn->arg_begin(), E = Fn->arg_end(); I != E; ++I, ++Idx) {
    llvm::Argument *Arg = I;
    FormalParameter *FP = Proc->getParams()[Idx];
    Defs.Defs.insert(std::pair<Declaration *, llvm::Value *>(FP, Arg));
  }

  auto Block = Proc->getBody();
  emitStatements(Block.getStmts());

  sealBlock(Curr);
  // TODO Add ret instruction if necessary.
}
