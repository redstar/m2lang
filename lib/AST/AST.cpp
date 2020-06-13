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

ProgramModule *ProgramModule::create(Declaration *EnclosingDecl, SMLoc Loc,
                                     StringRef Name) {
  return new ProgramModule(EnclosingDecl, Loc, Name);
}

Type *Type::create(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
                   TypeDenoter *Denoter) {
  return new Type(EnclosingDecl, Loc, Name, Denoter);
}

Constant *Constant::create(Declaration *EnclosingDecl, SMLoc Loc,
                           StringRef Name, Type *TypeDecl,
                           Expression *ConstExpr) {
  return new Constant(EnclosingDecl, Loc, Name, TypeDecl, ConstExpr);
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

PervasiveType *PervasiveType::create() { return new PervasiveType(); }

NamedType *NamedType::create(Type *TypeDecl) { return new NamedType(TypeDecl); }

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

Designator *Designator::create(Declaration *Decl, const SelectorList &Selectors,
                               TypeDenoter *Denoter, bool IsVariable,
                               bool IsConst) {
  return new Designator(Decl, Selectors, Denoter, IsVariable, IsConst);
}

ValueConstructor *ValueConstructor::create(TypeDenoter *Denoter) {
  return new ValueConstructor(Denoter);
}

FunctionCall *FunctionCall::create(Designator *Desig,
                                   const ExpressionList &ActualParameters,
                                   TypeDenoter *Denoter, bool IsConst) {
  return new FunctionCall(Desig, ActualParameters, Denoter, IsConst);
}

AssignmentStatement *AssignmentStatement::create(Designator *Left,
                                                 Expression *Right) {
  return new AssignmentStatement(Left, Right);
}

ProcedureCallStatement *
ProcedureCallStatement::create(Designator *Proc,
                               const ExpressionList &ActualParameters) {
  return new ProcedureCallStatement(Proc, ActualParameters);
}

IfStatement *IfStatement::create(Expression *Cond) {
  // Cond must be boolean expression.
  return new IfStatement(Cond);
}

CaseStatement *CaseStatement::create() { return nullptr; }

WhileStatement *WhileStatement::create(Expression *Cond, StatementList &Stmts,
                                       SMLoc Loc) {
  // Cond must be boolean expression.
  return new WhileStatement(Cond, Stmts, Loc);
}

RepeatStatement *RepeatStatement::create(Expression *Cond, StatementList &Stmts,
                                         SMLoc Loc) {
  // Cond must be boolean expression.
  return new RepeatStatement(Cond, Stmts, Loc);
}

ForStatement *ForStatement::create(SMLoc Loc, Variable *ControlVariable,
                                   Expression *InitialValue,
                                   Expression *FinalValue, Expression *StepSize,
                                   const StatementList &ForStmts) {
  return new ForStatement(Loc, ControlVariable, InitialValue, FinalValue,
                          StepSize, ForStmts);
}

LoopStatement *LoopStatement::create(StatementList &Stmts, SMLoc Loc) {
  return new LoopStatement(Stmts, Loc);
}

WithStatement *WithStatement::create() { return nullptr; }

ExitStatement *ExitStatement::create(SMLoc Loc) {
  return new ExitStatement(Loc);
}

ReturnStatement *ReturnStatement::create(Expression *E) {
  return new ReturnStatement(E);
}

RetryStatement *RetryStatement::create(SMLoc Loc) {
  return new RetryStatement(Loc);
}
