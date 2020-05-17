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

CompilationUnit *CompilationUnit::create() { return nullptr; }

ModuleDecl *ModuleDecl::create() { return nullptr; }

ProcedureDecl *ProcedureDecl::create() { return nullptr; }

ConstantDecl *ConstantDecl::create(SourceLocation Loc, StringRef Name,
                                   Expr *E) {
  return new ConstantDecl(Loc, Name, E);
}

TypeDecl *TypeDecl::create(SourceLocation Loc, StringRef Name, Type *Ty) {
  return new TypeDecl(Loc, Name, Ty);
}

VariableDecl *VariableDecl::create(SourceLocation Loc, StringRef Name,
                                   Type *Ty) {
  return new VariableDecl(Loc, Name, Ty);
}

IfStmt *IfStmt::create(Expr *Cond) {
  // Cond must be boolean expression.
  return new IfStmt(Cond);
}

CaseStmt *CaseStmt::create() { return nullptr; }

WhileStmt *WhileStmt::create(Expr *Cond, StmtList &Stmts, SourceLocation Loc) {
  // Cond must be boolean expression.
  return new WhileStmt(Cond, Stmts, Loc);
}

RepeatStmt *RepeatStmt::create(Expr *Cond, StmtList &Stmts, SourceLocation Loc) {
  // Cond must be boolean expression.
  return new RepeatStmt(Cond, Stmts, Loc);
}

ForStmt *ForStmt::create() { return nullptr; }

LoopStmt *LoopStmt::create(StmtList &Stmts, SourceLocation Loc) {
  return new LoopStmt(Stmts, Loc);
}

WithStmt *WithStmt::create() { return nullptr; }

ExitStmt *ExitStmt::create(SourceLocation Loc) { return new ExitStmt(Loc); }

ReturnStmt *ReturnStmt::create(Expr *E) { return new ReturnStmt(E); }

RetryStmt *RetryStmt::create(SourceLocation Loc) { return new RetryStmt(Loc); }

InfixExpression *InfixExpression::create(Expr *Left, Expr *Right,
                                         const OperatorInfo &Op) {
  return new InfixExpression(Left, Right, Op);
}

PrefixExpression *PrefixExpression::create(Expr *E, const OperatorInfo &Op) {
  return new PrefixExpression(E, Op);
}
