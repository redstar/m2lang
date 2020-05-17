//===--- Sema.h - M2 Language Family Semantic Analyzer ----------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the semantic analyzer implementation.
///
//===----------------------------------------------------------------------===//

#include "m2lang/Sema/Sema.h"
#include "llvm/Support/raw_ostream.h"

using namespace m2lang;

void Sema::actOnProgramModule(SourceLocation Loc, std::string Name) {
  llvm::outs() << "actOnProgramModule\n";
}

void Sema::actOnModuleDecl() { llvm::outs() << "actOnModuleDecl\n"; }

void Sema::actOnProcedureDecl() { llvm::outs() << "actOnProcedureDecl\n"; }

ConstantDecl *Sema::actOnConstantDecl(SourceLocation Loc, StringRef Name,
                                      Expr *E) {
  llvm::outs() << "actOnConstantDecl: Loc = " << Loc << " Name = " << Name
               << "\n";
  return ConstantDecl::create(Loc, Name, E);
}

TypeDecl *Sema::actOnTypeDecl(SourceLocation Loc, StringRef Name, Type *Ty) {
  llvm::outs() << "actOnTypeDecl: Loc = " << Loc << " Name = " << Name << "\n";
  return TypeDecl::create(Loc, Name, Ty);
}

VariableDecl *Sema::actOnVariableDecl(SourceLocation Loc, StringRef Name,
                                      Type *Ty) {
  llvm::outs() << "actOnVariableDecl: Loc = " << Loc << " Name = " << Name
               << "\n";
  return VariableDecl::create(Loc, Name, Ty);
}

Stmt *Sema::actOnIfStmt(Expr *Cond) {
  llvm::outs() << "actOnIfStmt\n";
  return IfStmt::create(Cond);
}

Stmt *Sema::actOnCaseStmt() {
  llvm::outs() << "actOnCaseStmt\n";
  return nullptr;
}

Stmt *Sema::actOnWhileStmt(Expr *Cond, StmtList &Stmts, SourceLocation Loc) {
  llvm::outs() << "actOnWhileStmt\n";
  return WhileStmt::create(Cond, Stmts, Loc);
}

Stmt *Sema::actOnRepeatStmt(Expr *Cond, StmtList &Stmts, SourceLocation Loc) {
  llvm::outs() << "actOnRepeatStmt\n";
  return RepeatStmt::create(Cond, Stmts, Loc);
}

Stmt *Sema::actOnLoopStmt(StmtList &Stmts, SourceLocation Loc) {
  llvm::outs() << "actOnLoopStmt\n";
  return LoopStmt::create(Stmts, Loc);
}

Stmt *Sema::actOnForStmt() {
  llvm::outs() << "actOnForStmt\n";
  return nullptr;
}

Stmt *Sema::actOnWithStmt() {
  llvm::outs() << "actOnWithStmt\n";
  return nullptr;
}

Stmt *Sema::actOnExitStmt(SourceLocation Loc) {
  llvm::outs() << "actOnExitStmt\n";
  return ExitStmt::create(Loc);
}

Stmt *Sema::actOnReturnStmt(Expr *E) {
  llvm::outs() << "actOnReturnStmt\n";
  return ReturnStmt::create(E);
}

Stmt *Sema::actOnRetryStmt(SourceLocation Loc) {
  llvm::outs() << "actOnRetryStmt\n";
  return RetryStmt::create(Loc);
}

void Sema::actOnConstantExpression() {
  llvm::outs() << "actOnConstantExpression\n";
}

Expr *Sema::actOnExpression(Expr *Left, Expr *Right, const OperatorInfo &Op) {
  llvm::outs() << "actOnExpression\n";
  // Op is a relational operation.
  return InfixExpression::create(Left, Right, Op);
}

Expr *Sema::actOnSimpleExpression(Expr *Left, Expr *Right, const OperatorInfo &Op) {
  llvm::outs() << "actOnSimpleExpression\n";
  // Op is a term operation.
  return InfixExpression::create(Left, Right, Op);
}

Expr *Sema::actOnTerm(Expr *Left, Expr *Right, const OperatorInfo &Op) {
  llvm::outs() << "actOnTerm\n";
  // Op is a factor operation.
  return InfixExpression::create(Left, Right, Op);
}

Expr *Sema::actOnFactor(Expr *E, const OperatorInfo &Op) {
  llvm::outs() << "actOnFactor\n";
  return PrefixExpression::create(E, Op);
}
