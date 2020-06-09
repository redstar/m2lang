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
#include "m2lang/Sema/Scope.h"

namespace m2lang {

class Identifier {
  SMLoc Loc;
  StringRef Name;

public:
  Identifier() = default;
  Identifier(SMLoc Loc, StringRef Name) : Loc(Loc), Name(Name) {}

  SMLoc getLoc() const { return Loc; }
  StringRef getName() const { return Name; }
};

using IdentifierList = llvm::SmallVector<Identifier, 8>;
using VariableIdentifierList =
    llvm::SmallVector<std::pair<Identifier, Expression *>, 8>;

class Sema final {
  DiagnosticsEngine &Diags;

  Scope *CurrentScope;
  Declaration *CurrentDecl;

  friend class EnterDeclScope;
  void enterScope(Declaration *Decl);
  void leaveScope();

public:
  Sema(DiagnosticsEngine &Diags)
      : Diags(Diags), CurrentScope(nullptr), CurrentDecl(nullptr) {}

  void initialize();

  bool isModule(StringRef Name);
  bool isClass(StringRef Name);

  ProgramModule *actOnProgramModule(Identifier ModuleName);
  void actOnProgramModule(ProgramModule *Mod, Identifier ModuleName,
                          DeclarationList Decls, Block InitBlk, Block FinalBlk);
  LocalModule *actOnLocalModule(Identifier ModuleName);
  Procedure *actOnProcedure(Identifier ProcName);
  void actOnProcedure(Procedure *Proc, Identifier ProcName);
  void actOnForwardProcedure(Procedure *Proc);
  void actOnConstant(DeclarationList &Decls, Identifier Name, Expression *Expr);
  void actOnType(DeclarationList &Decls, Identifier TypeName,
                 TypeDenoter *TyDen);
  void actOnVariable(DeclarationList &Decls, VariableIdentifierList &VarIdList,
                     TypeDenoter *TyDen);
  NamedType *actOnNamedType(SMLoc Loc, Declaration *Decl);
  Statement *actOnIfStmt(Expression *Cond);
  Statement *actOnCaseStmt();
  Statement *actOnWhileStmt(Expression *Cond, StatementList &Stmts, SMLoc Loc);
  Statement *actOnRepeatStmt(Expression *Cond, StatementList &Stmts, SMLoc Loc);
  Statement *actOnLoopStmt(StatementList &Stmts, SMLoc Loc);
  Statement *actOnForStmt();
  Statement *actOnWithStmt();
  Statement *actOnExitStmt(SMLoc Loc);
  Statement *actOnReturnStmt(Expression *E);
  Statement *actOnRetryStmt(SMLoc Loc);
  void actOnConstantExpression();
  Expression *actOnExpression(Expression *Left, Expression *Right,
                              const OperatorInfo &Op);
  Expression *actOnSimpleExpression(Expression *Left, Expression *Right,
                                    const OperatorInfo &Op);
  Expression *actOnTerm(Expression *Left, Expression *Right,
                        const OperatorInfo &Op);
  Expression *actOnFactor(Expression *E, const OperatorInfo &Op);
};

class EnterDeclScope {
  Sema &Semantics;

public:
  EnterDeclScope(Sema &Semantics, Declaration *Decl) : Semantics(Semantics) {
    Semantics.enterScope(Decl);
  }
  ~EnterDeclScope() { Semantics.leaveScope(); }
};

} // namespace m2lang

#endif