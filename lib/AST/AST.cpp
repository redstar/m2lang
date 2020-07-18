//===--- AST.cpp - Modula-2 Abstract Syntax Tree ----------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the AST implementation.
///
//===----------------------------------------------------------------------===//

#include "m2lang/AST/AST.h"

using namespace m2lang;

ImplementationModule *ImplementationModule::create(Declaration *EnclosingDecl,
                                                   SMLoc Loc, StringRef Name,
                                                   bool IsUnsafeGuarded) {
  return new ImplementationModule(EnclosingDecl, Loc, Name, IsUnsafeGuarded);
}

DefinitionModule *DefinitionModule::create(Declaration *EnclosingDecl,
                                           SMLoc Loc, StringRef Name,
                                           bool IsUnsafeGuarded) {
  return new DefinitionModule(EnclosingDecl, Loc, Name, IsUnsafeGuarded);
}

RefiningDefinitionModule *
RefiningDefinitionModule::create(Declaration *EnclosingDecl, SMLoc Loc,
                                 StringRef Name, bool IsUnsafeGuarded) {
  return new RefiningDefinitionModule(EnclosingDecl, Loc, Name,
                                      IsUnsafeGuarded);
}

RefiningImplementationModule *
RefiningImplementationModule::create(Declaration *EnclosingDecl, SMLoc Loc,
                                     StringRef Name, bool IsUnsafeGuarded) {
  return new RefiningImplementationModule(EnclosingDecl, Loc, Name,
                                          IsUnsafeGuarded);
}

Type *Type::create(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
                   TypeDenoter *Denoter) {
  return new Type(EnclosingDecl, Loc, Name, Denoter);
}

Constant *Constant::create(Declaration *EnclosingDecl, SMLoc Loc,
                           StringRef Name, TypeDenoter *TyDe,
                           Expression *ConstExpr) {
  return new Constant(EnclosingDecl, Loc, Name, TyDe, ConstExpr);
}

Variable *Variable::create(Declaration *EnclosingDecl, SMLoc Loc,
                           StringRef Name, TypeDenoter *Denoter,
                           Expression *Addr) {
  return new Variable(EnclosingDecl, Loc, Name, Denoter, Addr);
}

FormalParameter *FormalParameter::create(Declaration *EnclosingDecl, SMLoc Loc,
                                         StringRef Name, Type *Ty, bool IsVar,
                                         unsigned OpenArrayLevel) {
  return new FormalParameter(EnclosingDecl, Loc, Name, Ty, IsVar,
                             OpenArrayLevel);
}

Procedure *Procedure::create(Declaration *EnclosingDecl, SMLoc Loc,
                             StringRef Name) {
  return new Procedure(EnclosingDecl, Loc, Name);
}

LocalModule *LocalModule::create(Declaration *EnclosingDecl, SMLoc Loc,
                                 StringRef Name) {
  return new LocalModule(EnclosingDecl, Loc, Name);
}

Class *Class::create(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name) {
  return new Class(EnclosingDecl, Loc, Name);
}

PervasiveType *PervasiveType::create(pervasive::PervasiveTypeKind TypeKind) {
  return new PervasiveType(TypeKind);
}

RecordType *RecordType::create() { return new RecordType(); }

ArrayType *ArrayType::create(TypeDenoter *ComponentType,
                             TypeDenoter *IndexType) {
  return new ArrayType(ComponentType, IndexType);
}

ProcedureType *ProcedureType::create(Type *ResultType) {
  return new ProcedureType(ResultType);
}

PointerType *PointerType::create(TypeDenoter *TyDen) {
  return new PointerType(TyDen);
}

PointerType *PointerType::create(const StringRef &Name) {
  return new PointerType(Name);
}

SubrangeType *SubrangeType::create(Type *RangeType, Expression *From,
                                   Expression *To) {
  return new SubrangeType(RangeType, From, To);
}

EnumerationType *EnumerationType::create() { return new EnumerationType(); }

SetType *SetType::create(TypeDenoter *BaseType, bool IsPacked) {
  return new SetType(BaseType, IsPacked);
}

InfixExpression *InfixExpression::create(Expression *Left, Expression *Right,
                                         const OperatorInfo &Op,
                                         TypeDenoter *Denoter, bool IsConst) {
  return new InfixExpression(Left, Right, Op, Denoter, IsConst);
}

PrefixExpression *PrefixExpression::create(Expression *E,
                                           const OperatorInfo &Op,
                                           TypeDenoter *Denoter, bool IsConst) {
  return new PrefixExpression(E, Op, Denoter, IsConst);
}

IndexSelector *IndexSelector::create(Expression *Index) {
  return new IndexSelector(Index);
}

FieldSelector *FieldSelector::create() {
  return new FieldSelector();
}

DereferenceSelector *DereferenceSelector::create() {
  return new DereferenceSelector();
}

Designator *Designator::create(Declaration *Decl, const SelectorList &Selectors,
                               TypeDenoter *Denoter, bool IsVariable,
                               bool IsConst) {
  return new Designator(Decl, Selectors, Denoter, IsVariable, IsConst);
}

ValueConstructor *ValueConstructor::create(TypeDenoter *Denoter) {
  return new ValueConstructor(Denoter);
}

FunctionCall *FunctionCall::create(Designator *Desig,
                                   const ActualParameterList &ActualParameters,
                                   TypeDenoter *Denoter, bool IsConst) {
  return new FunctionCall(Desig, ActualParameters, Denoter, IsConst);
}

AssignmentStatement *AssignmentStatement::create(SMLoc Loc, Designator *Left,
                                                 Expression *Right) {
  return new AssignmentStatement(Loc, Left, Right);
}

ProcedureCallStatement *
ProcedureCallStatement::create(SMLoc Loc, Designator *Proc,
                               const ActualParameterList &ActualParameters) {
  return new ProcedureCallStatement(Loc, Proc, ActualParameters);
}

IfStatement *IfStatement::create(SMLoc Loc, Expression *Cond, StatementList &Stmts) {
  // Cond must be boolean expression.
  return new IfStatement(Loc, Cond, Stmts);
}

CaseStatement *CaseStatement::create(SMLoc Loc) {
  return new CaseStatement(Loc);
}

WhileStatement *WhileStatement::create(SMLoc Loc, Expression *Cond,
                                       StatementList &Stmts) {
  // Cond must be boolean expression.
  return new WhileStatement(Loc, Cond, Stmts);
}

RepeatStatement *RepeatStatement::create(SMLoc Loc, Expression *Cond, StatementList &Stmts) {
  // Cond must be boolean expression.
  return new RepeatStatement(Loc, Cond, Stmts);
}

ForStatement *ForStatement::create(SMLoc Loc, Variable *ControlVariable,
                                   Expression *InitialValue,
                                   Expression *FinalValue, Expression *StepSize,
                                   const StatementList &ForStmts) {
  return new ForStatement(Loc, ControlVariable, InitialValue, FinalValue,
                          StepSize, ForStmts);
}

LoopStatement *LoopStatement::create(SMLoc Loc, StatementList &Stmts) {
  return new LoopStatement(Loc, Stmts);
}

WithStatement *WithStatement::create(SMLoc Loc) {
  return new WithStatement(Loc);
}

ExitStatement *ExitStatement::create(SMLoc Loc) {
  return new ExitStatement(Loc);
}

ReturnStatement *ReturnStatement::create(SMLoc Loc, Expression *RetVal) {
  return new ReturnStatement(Loc, RetVal);
}

RetryStatement *RetryStatement::create(SMLoc Loc) {
  return new RetryStatement(Loc);
}
