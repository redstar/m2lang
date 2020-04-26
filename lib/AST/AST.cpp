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

Module *Module::create() { return nullptr; }

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

IfStmt *IfStmt::create() { return nullptr; }

CaseStmt *CaseStmt::create() { return nullptr; }

WhileStmt *WhileStmt::create() { return nullptr; }

RepeatStmt *RepeatStmt::create() { return nullptr; }

ForStmt *ForStmt::create() { return nullptr; }

LoopStmt *LoopStmt::create() { return nullptr; }

WithStmt *WithStmt::create() { return nullptr; }

Expression *Expression::create(SourceLocation Loc, SimpleExpression *Left,
                               SimpleExpression *Right,
                               tok::TokenKind Relation) {
  return new Expression(Loc, Left, Right, Relation);
}

SimpleExpression *SimpleExpression::create(SourceLocation Loc,
                                           tok::TokenKind UnaryOp, Term *T,
                                           std::vector<OpAndTerm> OpsAndTerms) {
  return new SimpleExpression(Loc, UnaryOp, T, OpsAndTerms);
}

Term *Term::create(SourceLocation Loc, Factor *F,
                   std::vector<OpAndFactor> OpsAndFactors) {
  return new Term(Loc, F, OpsAndFactors);
}
