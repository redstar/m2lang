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

#define AST_DISPATCHER
#include "m2lang/CodeGen/CGProcedure.h"
#include "m2lang/AST/AST.h"
#include "m2lang/Basic/TokenKinds.h"
#include "m2lang/CodeGen/CGUtils.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/ScopeExit.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <cstddef>

using namespace m2lang;

void CGProcedure::writeLocalVariable(llvm::BasicBlock *BB, Declaration *Decl,
                                llvm::Value *Val) {
  assert(BB && "Basic block is nullptr");
  assert((llvm::isa<Variable>(Decl) || llvm::isa<FormalParameter>(Decl)) &&
         "Declaration must be variable or formal parameter");
  assert(Val && "Value is nullptr");
  CurrentDef[BB].Defs[Decl] = Val;
}

llvm::Value *CGProcedure::readLocalVariable(llvm::BasicBlock *BB,
                                       Declaration *Decl) {
  assert(BB && "Basic block is nullptr");
  assert((llvm::isa<Variable>(Decl) || llvm::isa<FormalParameter>(Decl)) &&
         "Declaration must be variable or formal parameter");
  auto Val = CurrentDef[BB].Defs.find(Decl);
  if (Val != CurrentDef[BB].Defs.end())
    return Val->second;
  return readLocalVariableRecursive(BB, Decl);
}

llvm::Value *CGProcedure::readLocalVariableRecursive(llvm::BasicBlock *BB,
                                                Declaration *Decl) {
  llvm::Value *Val = nullptr;
  if (!CurrentDef[BB].Sealed) {
    // Add incomplete phi for variable.
    llvm::PHINode *Phi = BB->empty()
        ?  llvm::PHINode::Create(mapType(Decl), 0, "", BB)
        : llvm::PHINode::Create(mapType(Decl), 0, "", &BB->front());
    CurrentDef[BB].IncompletePhis[Phi] = Decl;
    Val = Phi;
  } else if (auto *PredBB = BB->getSinglePredecessor()) {
    // Only one predecessor.
    Val = readLocalVariable(PredBB, Decl);
  } else {
    // Create empty phi instruction to break potential cycles.
    llvm::PHINode *Phi = BB->empty()
        ?  llvm::PHINode::Create(mapType(Decl), 0, "", BB)
        : llvm::PHINode::Create(mapType(Decl), 0, "", &BB->front());
    writeLocalVariable(BB, Decl, Phi);
    Val = addPhiOperands(BB, Decl, Phi);
  }
  writeLocalVariable(BB, Decl, Val);
  return Val;
}

llvm::Value *CGProcedure::addPhiOperands(llvm::BasicBlock *BB, Declaration *Decl,
                                 llvm::PHINode *Phi) {
  for (llvm::BasicBlock *PredBB : llvm::predecessors(BB)) {
    Phi->addIncoming(readLocalVariable(PredBB, Decl), PredBB);
  }
  return tryRemoveTrivialPhi(Phi);
}

llvm::Value *CGProcedure::tryRemoveTrivialPhi(llvm::PHINode *Phi) {
  llvm::Value *Same = nullptr;
  for (llvm::Value *V : Phi->incoming_values()) {
    if (V == Same || V == Phi)
      continue;
    if (Same && V != Same)
      return Phi;
    Same = V;
  }
  if (Same == nullptr)
    Same = llvm::UndefValue::get(Phi->getType());
  // Collect phi instructions using this one. Do not remember self-references.
  llvm::SmallVector<llvm::PHINode *, 8> CandidatePhis;
  for (llvm::Use &U : Phi->uses()) {
    if (auto *P = llvm::dyn_cast<llvm::PHINode>(U.getUser()))
      if (P != Phi)
        CandidatePhis.push_back(P);
  }
  Phi->replaceAllUsesWith(Same);
  Phi->eraseFromParent();
  for (auto *P : CandidatePhis)
    tryRemoveTrivialPhi(P);
  return Same;
}

void CGProcedure::sealBlock(llvm::BasicBlock *BB) {
  assert(!CurrentDef[BB].Sealed && "Attempt to seal already sealed block");
  for (auto PhiDecl : CurrentDef[BB].IncompletePhis) {
    addPhiOperands(BB, PhiDecl.second, PhiDecl.first);
  }
  CurrentDef[BB].IncompletePhis.clear();
  CurrentDef[BB].Sealed = true;
}

void CGProcedure::writeVariable(llvm::BasicBlock *BB, Declaration *Decl,
                                llvm::Value *Val) {
  if (auto *V = llvm::dyn_cast<Variable>(Decl)) {
    if (V->getStorage() == Variable::Stack)
      writeLocalVariable(BB, Decl, Val);
    else if (V->getStorage() == Variable::Module) {
      auto *Inst = Builder.CreateStore(Val, CGM.getGlobal(Decl));
      CGM.decorateInst(Inst, V->getTypeDenoter());
    } else
      llvm::report_fatal_error("Nested procedures not yet supported");
  }
  else if (auto *FP = llvm::dyn_cast<FormalParameter>(Decl)) {
    if (FP->isCallByReference()) {
      auto *Inst = Builder.CreateStore(Val, FormalParams[FP]);
      CGM.decorateInst(Inst, FP->getType());
    }
    else
      writeLocalVariable(BB, Decl, Val);
  }
  else
      llvm::report_fatal_error("Unsupported declaration");
}

llvm::Value *CGProcedure::readVariable(llvm::BasicBlock *BB,
                                       Declaration *Decl) {
  if (auto *V = llvm::dyn_cast<Variable>(Decl)) {
    if (V->getStorage() == Variable::Stack)
      return readLocalVariable(BB, Decl);
    if (V->getStorage() == Variable::Module) {
      auto *Inst =  Builder.CreateLoad(mapType(Decl), CGM.getGlobal(Decl));
      CGM.decorateInst(Inst, V->getTypeDenoter());
      return Inst;
    }
    llvm::report_fatal_error("Nested procedures not yet supported");
  } else if (auto *FP = llvm::dyn_cast<FormalParameter>(Decl)) {
    if (FP->isCallByReference()) {
      auto *Inst = Builder.CreateLoad(mapType(FP, false), FormalParams[FP]);
      CGM.decorateInst(Inst, FP->getType());
      return Inst;
    }
    return  readLocalVariable(BB, Decl);
  } else
    llvm::report_fatal_error("Unsupported declaration");
}

llvm::Type *CGProcedure::mapType(Declaration *Decl, bool HonorReference) {
  // FIXME What about other declarations?
  if (auto *FP = llvm::dyn_cast<FormalParameter>(Decl)) {
    if (FP->isCallByReference() && HonorReference)
      return CGM.PtrTy;
    return CGM.convertType(FP->getType());
  }
  if (auto *T = llvm::dyn_cast<Type>(Decl))
    return CGM.convertType(T);
  return CGM.convertType(llvm::cast<Variable>(Decl)->getTypeDenoter());
}

llvm::FunctionType *CGProcedure::createFunctionType(Procedure *Proc) {
  llvm::Type *ResultTy = CGM.VoidTy;
  if (Proc->getResultType()) {
    ResultTy = mapType(Proc->getResultType());
  }
  auto FormalParams = Proc->getParams();
  llvm::SmallVector<llvm::Type *, 8> ParamTypes;
  for (auto *FP : FormalParams) {
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
  for (auto Pair : llvm::enumerate(Fn->args())) {
    llvm::Argument *Arg = &Pair.value();
    FormalParameter *FP = Proc->getParams()[Pair.index()];
    if (FP->isCallByReference()) {
      llvm::AttrBuilder Attr(CGM.getLLVMCtx());
      auto Sz = CGM.getModule()->getDataLayout().getTypeStoreSize(
          CGM.convertType(FP->getType()));
      Attr.addDereferenceableAttr(Sz);
      Attr.addAttribute(llvm::Attribute::NoCapture);
      Arg->addAttrs(Attr);
    }
    Arg->setName(FP->getName());
  }
  return Fn;
}

llvm::Value *CGProcedure::emitInfixExpr(InfixExpression *E) {
  llvm::Value *Left = emitExpr(E->getLeftExpression());
  llvm::Value *Right = emitExpr(E->getRightExpression());
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

llvm::Value *CGProcedure::emitIntLiteral(IntegerLiteral *IntLit) {
  return llvm::ConstantInt::get(CGM.Int64Ty, IntLit->getValue());
}

llvm::Value *CGProcedure::emitRealLiteral(RealLiteral *RealLit) {
  return llvm::ConstantFP::get(CGM.getLLVMCtx(), RealLit->getValue());
}

llvm::Value *CGProcedure::emitStringLiteral(StringLiteral *E) {
  llvm::report_fatal_error("Cannot handle expression: StringLiteral");
  return nullptr;
}

llvm::Value *CGProcedure::emitCharLiteral(CharLiteral *E) {
  llvm::report_fatal_error("Cannot handle expression: CharLiteral");
  return nullptr;
}

llvm::Value *CGProcedure::emitBoolLiteral(BooleanLiteral *BoolLit) {
  return llvm::ConstantInt::getBool(CGM.Int8Ty, BoolLit->isValue());
}

llvm::Value *CGProcedure::emitNilValue(NilValue *E) {
  return llvm::Constant::getNullValue(CGM.Int8Ty->getPointerTo());
}

llvm::Value *CGProcedure::emitDesignator(Designator *Desig) {
  Declaration *Decl = Desig->getDecl();
  if (auto *Const = llvm::dyn_cast<Constant>(Decl)) {
    assert(Desig->getSelectors().empty() && "No selectors expected");
    return emitExpr(Const->getConstExpr());
  }
  if (llvm::isa_and_nonnull<Variable>(Decl) ||
      llvm::isa_and_nonnull<FormalParameter>(Decl)) {
    llvm::Value *Val = readVariable(Curr, Decl);
    TypeDenoter *TyDe = Desig->getTypeDenoter();
    auto &Selectors = Desig->getSelectors();
    for (auto *I = Selectors.begin(), *E = Selectors.end(); I != E;) {
      if (auto *IdxSel = llvm::dyn_cast<IndexSelector>(*I)) {
        llvm::SmallVector<llvm::Value *, 4> IdxList;
        // TODO Scale index
        llvm::Value *Idx = emitExpr(IdxSel->getIndex());
        IdxList.push_back(Idx);
        for (++I; I != E;) {
          if (auto *IdxSel2 = llvm::dyn_cast<IndexSelector>(*I)) {
            // TODO Scale index
            IdxList.push_back(emitExpr(IdxSel2->getIndex()));
            ++I;
          } else
            break;
        }
        Val = Builder.CreateInBoundsGEP(Val->getType(), Val, IdxList);
        Val = Builder.CreateLoad(CGM.convertType(TyDe), Val);
      } else if (auto *D = llvm::dyn_cast<DereferenceSelector>(*I)) {
        Val = Builder.CreateLoad(CGM.convertType(TyDe), Val);
        ++I;
      } else {
        llvm::report_fatal_error("Unsupported selector");
      }
    }
    return Val;
  }
  llvm::report_fatal_error("Unsupported designator");
}

llvm::Value *CGProcedure::emitFuncCall(FunctionCall *E) {
  llvm::report_fatal_error("Cannot handle expression: FuncCall");
  return nullptr;
}

llvm::Value *CGProcedure::emitValueConstructor(ValueConstructor *E) {
  llvm::report_fatal_error("Cannot handle expression: ValueConstructor");
  return nullptr;
}

llvm::Value *CGProcedure::emitExpr(Expression *E) {
  static const dispatcher::ExpressionDispatcher<CGProcedure, llvm::Value *>
      EmitExpr(&CGProcedure::emitInfixExpr, &CGProcedure::emitPrefixExpr,
               &CGProcedure::emitIntLiteral, &CGProcedure::emitRealLiteral,
               &CGProcedure::emitStringLiteral, &CGProcedure::emitCharLiteral,
               &CGProcedure::emitBoolLiteral, &CGProcedure::emitNilValue,
               &CGProcedure::emitDesignator, &CGProcedure::emitFuncCall,
               &CGProcedure::emitValueConstructor);
  return EmitExpr(this, E);
}

void CGProcedure::emitAssign(AssignmentStatement *Stmt) {
  llvm::Value *Right = emitExpr(Stmt->getExpression());
    Declaration *Decl = Stmt->getDesignator()->getDecl();
    if (llvm::isa_and_nonnull<Variable>(Decl) || llvm::isa_and_nonnull<FormalParameter>(Decl))
      writeVariable(Curr, Decl, Right);
}

void CGProcedure::emitCall(ProcedureCallStatement *Stmt) {
  llvm::outs() << "emitCall\n";
}

void CGProcedure::emitIf(IfStatement *Stmt) {
  // Create basic block for the ELSE clause.
  bool HasElse = !Stmt->getElseStmts().empty();
  llvm::BasicBlock *ElseBB =
      HasElse ? llvm::BasicBlock::Create(CGM.getLLVMCtx(), "else.body", Fn)
              : nullptr;

  // Create basic block for the statemntes after the IF.
  llvm::BasicBlock *AfterIfBB = createBasicBlock("after.if");

  // List of blocks to seal.
  llvm::SmallVector<llvm::BasicBlock *, 4> SealBB;

  for (size_t I = 0, E = Stmt->getGuardedStmts().size(); I < E; ++I) {
    // Create basic block for the statements.
    llvm::BasicBlock *IfBB =
        createBasicBlock(I == 0 ? "if.body" : "elsif.body");
    SealBB.push_back(IfBB);

    llvm::BasicBlock *NextBB = (I + 1) == E ? (HasElse ? ElseBB : AfterIfBB)
                                            : createBasicBlock("elsif.cond");
    llvm::Value *Cond = emitExpr(Stmt->getGuardedStmts()[I].getCond());
    Builder.CreateCondBr(Cond, IfBB, NextBB);

    setCurr(IfBB);
    emitStatements(Stmt->getGuardedStmts()[I].getStmts());
    if (!Curr->getTerminator())
      Builder.CreateBr(AfterIfBB);
    setCurr(IfBB);
  }
  if (HasElse) {
    setCurr(ElseBB);
    SealBB.push_back(ElseBB);
    emitStatements(Stmt->getElseStmts());
    if (!Curr->getTerminator())
      Builder.CreateBr(AfterIfBB);
  }

  // Seal all created blocks.
  for (auto *BB : SealBB)
    sealBlock(BB);

  setCurr(AfterIfBB);
}

void CGProcedure::emitCase(CaseStatement *Stmt) {
  llvm::outs() << "emitCase\n";
}

void CGProcedure::emitWhile(WhileStatement *Stmt) {
  // The basic block for the condition.
  llvm::BasicBlock *WhileCondBB = createBasicBlock("while.cond");
  // The basic block for the while body.
  llvm::BasicBlock *WhileBodyBB = createBasicBlock("while.body");
  // The basic block after the while statement.
  llvm::BasicBlock *AfterWhileBB = createBasicBlock("after.while");

  Builder.CreateBr(WhileCondBB);
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
  llvm::BasicBlock *RepeatBodyBB = createBasicBlock("repeat.body");
  // The basic block after the while statement.
  llvm::BasicBlock *AfterRepeatBB = createBasicBlock("after.repeat");

  Builder.CreateBr(RepeatBodyBB);
  setCurr(RepeatBodyBB);
  emitStatements(Stmt->getStmts());

  llvm::Value *Cond = emitExpr(Stmt->getCond());
  Builder.CreateCondBr(Cond, RepeatBodyBB, AfterRepeatBB);

  sealBlock(RepeatBodyBB);

  setCurr(AfterRepeatBB);
}

void CGProcedure::emitFor(ForStatement *Stmt) {
  // The basic block with the for loop condition.
  llvm::BasicBlock *ForCondBB = createBasicBlock("for.cond");
  // The basic block with the for body.
  llvm::BasicBlock *ForBodyBB = createBasicBlock("for.body");
  // The basic block after the for statement.
  llvm::BasicBlock *AfterForBB = createBasicBlock("after.for");

  // Assign the initial value.
  Variable *CtlVar = Stmt->getControlVariable();
  llvm::Value *InitialValue = emitExpr(Stmt->getInitialValue());
  writeVariable(Curr, CtlVar, InitialValue);
  Builder.CreateBr(ForCondBB);

  // Check the condition for the loop.
  // FIXME: Handle negative step size.
  setCurr(ForCondBB);
  llvm::Value *Ctl = readVariable(Curr, CtlVar);
  llvm::Value *FinalValue = emitExpr(Stmt->getFinalValue());
  llvm::Value *Cond = Builder.CreateICmpSLE(Ctl, FinalValue);
  Builder.CreateCondBr(Cond, ForBodyBB, AfterForBB);

  // Create the loop body, and increment the control variable.
  setCurr(ForBodyBB);
  emitStatements(Stmt->getForStmts());
  Ctl = readVariable(Curr, CtlVar);
  llvm::Value *StepSize = emitExpr(Stmt->getStepSize());
  writeVariable(Curr, CtlVar, Builder.CreateAdd(Ctl, StepSize));
  Builder.CreateBr(ForCondBB);

  // Set the new current block, and seal the finished blocks.
  setCurr(AfterForBB);
  sealBlock(ForCondBB);
  sealBlock(ForBodyBB);
}

void CGProcedure::emitLoop(LoopStatement *Stmt) {
  // The basic block for the loop body.
  llvm::BasicBlock *LoopBodyBB;
  // The basic block after the loop statement.
  llvm::BasicBlock *AfterLoopBB = createBasicBlock("after.loop");
  llvm::BasicBlock *SavedBBforExit = BBforExit;
  auto SetAtEnd = llvm::make_scope_exit([&]() { BBforExit = SavedBBforExit; });
  BBforExit = AfterLoopBB;

  LoopBodyBB = createBasicBlock("loop.body");
  Builder.CreateBr(LoopBodyBB);
  setCurr(LoopBodyBB);

  emitStatements(Stmt->getStmts());
  Builder.CreateBr(LoopBodyBB);
  sealBlock(LoopBodyBB);

  setCurr(AfterLoopBB);
}

void CGProcedure::emitWith(WithStatement *Stmt) {
  llvm::outs() << "emitWith\n";
}

void CGProcedure::emitExit(ExitStatement *Stmt) {
  assert(BBforExit && "No BB for EXIT. Possible sema error.");
  Builder.CreateBr(BBforExit);
}

void CGProcedure::emitReturn(ReturnStatement *Stmt) {
  if (Stmt->getRetVal()) {
    llvm::Value *RetVal = emitExpr(Stmt->getRetVal());
    Builder.CreateRet(RetVal);
  } else {
    Builder.CreateRetVoid();
  }
}

void CGProcedure::emitRetry(RetryStatement *Stmt) {
  llvm::outs() << "emitRetry\n";
}

void CGProcedure::emitStatements(const StatementList &Stmts) {
  static const dispatcher::StatementDispatcher<CGProcedure> EmitStmt(
      &CGProcedure::emitAssign, &CGProcedure::emitCall, &CGProcedure::emitIf,
      &CGProcedure::emitCase, &CGProcedure::emitWhile, &CGProcedure::emitRepeat,
      &CGProcedure::emitFor, &CGProcedure::emitLoop, &CGProcedure::emitWith,
      &CGProcedure::emitExit, &CGProcedure::emitReturn,
      &CGProcedure::emitRetry);
  llvm::outs() << "emitStatements\n";
  for (auto *S : Stmts)
    EmitStmt(this, S);
}

void CGProcedure::run(Procedure *Proc) {
  Fty = createFunctionType(Proc);
  Fn = createFunction(Proc, Fty);
  setCurr(createBasicBlock("entry"));
  BBforExit = nullptr;

  // Record values of parameters in the first basic block.
  for (auto Pair : llvm::enumerate(Fn->args())) {
    llvm::Argument *Arg = &Pair.value();
    FormalParameter *FP = Proc->getParams()[Pair.index()];
    // Create mapping FormalParameter -> llvm::Argument for VAR parameters.
    FormalParams[FP] = Arg;
    writeLocalVariable(Curr, FP, Arg);
  }

  // Allocate space for local variables of aggregate types.
  for (auto *D : Proc->getDecls()) {
    if (auto *Var = llvm::dyn_cast<Variable>(D)) {
      llvm::Type *Ty = mapType(Var);
      if (Ty->isAggregateType()) {
        llvm::Value *Val = Builder.CreateAlloca(Ty);
        writeLocalVariable(Curr, Var, Val);
      }
    }
  }

  auto Block = Proc->getBody();
  emitStatements(Block.getStmts());

  for (auto &[BB, Def] : CurrentDef)
    if (!Def.Sealed)
      sealBlock(BB);
  // TODO Add ret instruction if necessary.
}

void CGProcedure::run(const Block &Block, const Twine &Name) {
  Fty = llvm::FunctionType::get(CGM.VoidTy, {}, /* IsVarArgs */ false);
  Fn = llvm::Function::Create(Fty, llvm::GlobalValue::ExternalLinkage, Name,
                              CGM.getModule());
  setCurr(createBasicBlock("entry"));
  BBforExit = nullptr;
  emitStatements(Block.getStmts());
  // TODO Handle exception statements.

  for (auto &[BB, Def] : CurrentDef)
    if (!Def.Sealed)
      sealBlock(BB);
}
