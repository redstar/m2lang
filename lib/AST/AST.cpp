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

Type *Type::create(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name) {
  return new Type(EnclosingDecl, Loc, Name);
}

Constant *Constant::create(Declaration *EnclosingDecl, SMLoc Loc,
                           StringRef Name, Type *TypeDecl,
                           Expression *ConstExpr) {
  return new Constant(EnclosingDecl, Loc, Name, TypeDecl, ConstExpr);
}

Variable *Variable::create(Declaration *EnclosingDecl, SMLoc Loc,
                           StringRef Name, Type *TypeDecl) {
  return new Variable(EnclosingDecl, Loc, Name, TypeDecl);
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

ForStatement *ForStatement::create() { return nullptr; }

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

InfixExpression *InfixExpression::create(Expression *Left, Expression *Right,
                                         const OperatorInfo &Op) {
  return new InfixExpression(Left, Right, Op);
}

PrefixExpression *PrefixExpression::create(Expression *E,
                                           const OperatorInfo &Op) {
  return new PrefixExpression(E, Op);
}
