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

void Sema::actOnIfStmt() { llvm::outs() << "actOnIfStmt\n"; }

void Sema::actOnCaseStmt() { llvm::outs() << "actOnCaseStmt\n"; }

void Sema::actOnWhileStmt() { llvm::outs() << "actOnWhileStmt\n"; }

void Sema::actOnRepeatStmt() { llvm::outs() << "actOnRepeatStmt\n"; }

void Sema::actOnLoopStmt() { llvm::outs() << "actOnLoopStmt\n"; }

void Sema::actOnForStmt() { llvm::outs() << "actOnForStmt\n"; }

void Sema::actOnWithStmt() { llvm::outs() << "actOnWithStmt\n"; }

void Sema::actOnExitStmt() { llvm::outs() << "actOnExitStmt\n"; }

void Sema::actOnReturnStmt() { llvm::outs() << "actOnReturnStmt\n"; }

void Sema::actOnConstantExpression() { llvm::outs() << "actOnConstantExpression\n"; }

void Sema::actOnExpression() { llvm::outs() << "actOnExpression\n"; }

void Sema::actOnSimpleExpression() { llvm::outs() << "actOnSimpleExpression\n"; }

void Sema::actOnTerm() { llvm::outs() << "actOnTerm\n"; }

void Sema::actOnFactor() { llvm::outs() << "actOnFactor\n"; }
