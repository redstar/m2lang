//===--- Sema.h - M2 Language Family Semantic Analyzer ----------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the semantic analyzer classes for Modula-2.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_SEMA_SEMA_H
#define M2LANG_SEMA_SEMA_H

#include "m2lang/AST/AST.h"

namespace m2lang {

class Sema final {
public:
  void actOnModuleDecl();
  void actOnProcedureDecl();
  ConstantDecl *actOnConstantDecl(SourceLocation Loc, StringRef Name, Expr *E);
  TypeDecl *actOnTypeDecl(SourceLocation Loc, StringRef Name, Type *Ty);
  VariableDecl *actOnVariableDecl(SourceLocation Loc, StringRef Name, Type *Ty);
  Stmt *actOnIfStmt(Expr *Cond);
  Stmt *actOnCaseStmt();
  Stmt *actOnWhileStmt(Expr *Cond, StmtList &Stmts, SourceLocation Loc);
  Stmt *actOnRepeatStmt(Expr *Cond, StmtList &Stmts, SourceLocation Loc);
  Stmt *actOnLoopStmt(StmtList &Stmts, SourceLocation Loc);
  Stmt *actOnForStmt();
  Stmt *actOnWithStmt();
  Stmt *actOnExitStmt(SourceLocation Loc);
  Stmt *actOnReturnStmt(Expr *E);
  Stmt *actOnRetryStmt(SourceLocation Loc);
  void actOnConstantExpression();
  Expr *actOnExpression(Expr *Left, Expr *Right, const OperatorInfo &Op);
  Expr *actOnSimpleExpression(Expr *Left, Expr *Right, const OperatorInfo &Op);
  Expr *actOnTerm(Expr *Left, Expr *Right, const OperatorInfo &Op);
  Expr *actOnFactor(Expr *E, const OperatorInfo &Op);
};

} // namespace m2lang

#endif