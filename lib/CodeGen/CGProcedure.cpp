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
    Val = Phi;
    writeLocalVariable(BB, Decl, Val);
    addPhiOperands(BB, Decl, Phi);
  }
  writeLocalVariable(BB, Decl, Val);
  return Val;
}

void CGProcedure::addPhiOperands(llvm::BasicBlock *BB, Declaration *Decl,
                                 llvm::PHINode *Phi) {
  for (auto I = llvm::pred_begin(BB), E = llvm::pred_end(BB); I != E; ++I) {
    Phi->addIncoming(readLocalVariable(*I, Decl), *I);
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
  for (auto PhiDecl : CurrentDef[BB].IncompletePhis) {
    addPhiOperands(BB, PhiDecl.second, PhiDecl.first);
  }
  CurrentDef[BB].IncompletePhis.clear();
  CurrentDef[BB].Sealed = true;
}

void CGProcedure::writeVariable(llvm::BasicBlock *BB, Declaration *Decl,
                                llvm::Value *Val) {
  if (auto *V = llvm::dyn_cast<Variable>(Decl)) {
    if (V->getEnclosingDecl() == Proc)
      writeLocalVariable(BB, Decl, Val);
    else if (V->getEnclosingDecl() == CGM.getCompilationModule()) {
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
    if (V->getEnclosingDecl() == Proc)
      return readLocalVariable(BB, Decl);
    else if (V->getEnclosingDecl() == CGM.getCompilationModule()) {
      auto *Inst =  Builder.CreateLoad(mapType(Decl), CGM.getGlobal(Decl));
      CGM.decorateInst(Inst, V->getTypeDenoter());
      return Inst;
    } else
      llvm::report_fatal_error("Nested procedures not yet supported");
  } else if (auto *FP = llvm::dyn_cast<FormalParameter>(Decl)) {
    if (FP->isCallByReference()) {
      auto *Inst = Builder.CreateLoad(mapType(FP)->getPointerElementType(), FormalParams[FP]);
      CGM.decorateInst(Inst, FP->getType());
      return Inst;
    }
    else
      return  readLocalVariable(BB, Decl);
  } else
    llvm::report_fatal_error("Unsupported declaration");
}

llvm::Type *CGProcedure::mapType(FormalParameter *Param) {
  // FIXME How to handle open array parameters?
  llvm::Type *Ty = CGM.convertType(Param->getType());
  if (Param->isCallByReference())
    Ty = Ty->getPointerTo();
  return Ty;
}

llvm::Type *CGProcedure::mapType(Declaration *Decl) {
  // FIXME What about other declarations?
  if (auto *FP = llvm::dyn_cast<FormalParameter>(Decl))
    return mapType(FP);
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
    if (FP->isCallByReference()) {
      llvm::AttrBuilder Attr;
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

llvm::Value *CGProcedure::emitExpr(Expression *E) {
  if (auto *Infix = llvm::dyn_cast<InfixExpression>(E)) {
    return emitInfixExpr(Infix);
  } else if (auto *Prefix = llvm::dyn_cast<PrefixExpression>(E)) {
    return emitPrefixExpr(Prefix);
  } else if (auto *Desig = llvm::dyn_cast<Designator>(E)) {
    Declaration *Decl = Desig->getDecl();
    if (auto *Const = llvm::dyn_cast<Constant>(Decl)) {
      assert(Desig->getSelectorList().empty() && "No selectors expected");
      return emitExpr(Const->getConstExpr());
    }
    if (llvm::isa_and_nonnull<Variable>(Decl) || llvm::isa_and_nonnull<FormalParameter>(Decl)) {
      llvm::Value *Val = readVariable(Curr, Decl);
      TypeDenoter *TyDe = Desig->getTypeDenoter();
      auto &Selectors = Desig->getSelectorList();
      for (auto *I = Selectors.begin(), *E = Selectors.end(); I != E; ) {
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
          Val = Builder.CreateInBoundsGEP(Val, IdxList);
          Val = Builder.CreateLoad(Val->getType()->getPointerElementType(), Val);
        }
        else if (auto *D = llvm::dyn_cast<DereferenceSelector>(*I)) {
          Val = Builder.CreateLoad(Val->getType()->getPointerElementType(), Val);
          ++I;
        }
        else {
          llvm::report_fatal_error("Unsupported selector");
        }
      }
      return Val;
    }
    llvm::report_fatal_error("Unsupported designator");
  } else if (auto *IntLit = llvm::dyn_cast<IntegerLiteral>(E)) {
    return llvm::ConstantInt::get(CGM.Int64Ty, IntLit->getValue());
  } else if (auto *Nil = llvm::dyn_cast<NilValue>(E)) {
    return llvm::Constant::getNullValue(CGM.Int8Ty->getPointerTo());
  } else {
    llvm::report_fatal_error("Cannot handle expression");
  }
  return nullptr;
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
  llvm::outs() << "emitIf\n";

    bool HasElse = false; //Stmt->getElseStmts().size() > 0;

    // Create the required basic blocks.
    llvm::BasicBlock *IfBB = createBasicBlock("if.body");
    // llvm::BasicBlock *ElseBB =
    //    HasElse ? llvm::BasicBlock::Create(CGM.getContext(), "else.body", Fn)
    //            : nullptr;
    llvm::BasicBlock *AfterIfBB = createBasicBlock("after.if");

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
  llvm::BasicBlock *WhileCondBB = createBasicBlock("while.cond");
  // The basic block for the while body.
  llvm::BasicBlock *WhileBodyBB = createBasicBlock("while.body");
  // The basic block after the while statement.
  llvm::BasicBlock *AfterWhileBB = createBasicBlock("after.while");

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
  this->Proc = Proc;
  Fty = createFunctionType(Proc);
  Fn = createFunction(Proc, Fty);
  setCurr(createBasicBlock("entry"));

  // Record values of parameters in the first basic block.
  size_t Idx = 0;
  for (auto I = Fn->arg_begin(), E = Fn->arg_end(); I != E; ++I, ++Idx) {
    llvm::Argument *Arg = I;
    FormalParameter *FP = Proc->getParams()[Idx];
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

  sealBlock(Curr);
  // TODO Add ret instruction if necessary.
}
