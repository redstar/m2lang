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
#include "m2lang/Basic/Diagnostic.h"

namespace m2lang {

class Sema final {
  DiagnosticsEngine &Diags;

public:
  Sema(DiagnosticsEngine &Diags) : Diags(Diags) {}

  void actOnProgramModule(SMLoc Loc, std::string Name);
  void actOnModuleDecl();
  void actOnProcedureDecl();
  ConstantDecl *actOnConstantDecl(SMLoc Loc, StringRef Name, Expr *E);
  TypeDecl *actOnTypeDecl(SMLoc Loc, StringRef Name, Type *Ty);
  VariableDecl *actOnVariableDecl(SMLoc Loc, StringRef Name, Type *Ty);
  Stmt *actOnIfStmt(Expr *Cond);
  Stmt *actOnCaseStmt();
  Stmt *actOnWhileStmt(Expr *Cond, StmtList &Stmts, SMLoc Loc);
  Stmt *actOnRepeatStmt(Expr *Cond, StmtList &Stmts, SMLoc Loc);
  Stmt *actOnLoopStmt(StmtList &Stmts, SMLoc Loc);
  Stmt *actOnForStmt();
  Stmt *actOnWithStmt();
  Stmt *actOnExitStmt(SMLoc Loc);
  Stmt *actOnReturnStmt(Expr *E);
  Stmt *actOnRetryStmt(SMLoc Loc);
  void actOnConstantExpression();
  Expr *actOnExpression(Expr *Left, Expr *Right, const OperatorInfo &Op);
  Expr *actOnSimpleExpression(Expr *Left, Expr *Right, const OperatorInfo &Op);
  Expr *actOnTerm(Expr *Left, Expr *Right, const OperatorInfo &Op);
  Expr *actOnFactor(Expr *E, const OperatorInfo &Op);
};

} // namespace m2lang

#endif