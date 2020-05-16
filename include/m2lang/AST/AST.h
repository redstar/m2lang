//===--- AST.h - M2 Language Family Abstract Syntax Tree --------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the abstract syntax tree classes.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_AST_AST_H
#define M2LANG_AST_AST_H

#include "m2lang/Basic/LLVM.h"
#include "m2lang/Basic/SourceLocation.h"
#include "m2lang/Basic/TokenKinds.h"
#include "llvm/ADT/StringRef.h"
#include <string>
#include <vector>

namespace m2lang {

class Module {
  std::string Name;
  bool IsGeneric;
  int Priority;

public:
  static Module *create();
};

class Type {};

class OperatorInfo {
  SourceLocation Loc;
  uint32_t Kind : 16;
  uint32_t IsUnspecified : 1;

public:
  OperatorInfo() : Loc(0), Kind(tok::unknown), IsUnspecified(true) {}
  OperatorInfo(SourceLocation Loc, tok::TokenKind Kind,
               bool IsUnspecified = false)
      : Loc(Loc), Kind(Kind), IsUnspecified(IsUnspecified) {}

  SourceLocation getLocation() const { return Loc; }
  tok::TokenKind getKind() const { return static_cast<tok::TokenKind>(Kind); }
  bool isUnspecified() const { return IsUnspecified; }
};

class Expr {};

class InfixExpression : public Expr {
protected:
  Expr *Left;
  Expr *Right;
  const OperatorInfo Op;

  InfixExpression(Expr *Left, Expr *Right, OperatorInfo Op)
      : Left(Left), Right(Right), Op(Op) {}

public:
  static InfixExpression *create(Expr *Left, Expr *Right,
                                 const OperatorInfo &Op);

  static InfixExpression *create(Expr *E) {
    return create(E, nullptr, OperatorInfo(0, tok::unknown, true));
  }
};

class PrefixExpression : public Expr {
protected:
  Expr *E;
  const OperatorInfo Op;

  PrefixExpression(Expr *E, OperatorInfo Op) : E(E), Op(Op) {}

public:
  static PrefixExpression *create(Expr *E, const OperatorInfo &Op);
};

class Factor : public Expr {
  Factor() {}
public:
  static Factor *create();
};

class Decl {
protected:
  SourceLocation Loc;
  StringRef Name;

  Decl(SourceLocation Loc, StringRef Name) : Loc(Loc), Name(Name) {}
};

using DeclList = std::vector<Decl *>;

class ModuleDecl : public Decl {
public:
  static ModuleDecl *create();
};

class ProcedureDecl : public Decl {
public:
  static ProcedureDecl *create();
};

class ConstantDecl : public Decl {
  Expr *E;

protected:
  ConstantDecl(SourceLocation Loc, StringRef Name, Expr *E)
      : Decl(Loc, Name), E(E) {}

public:
  static ConstantDecl *create(SourceLocation Loc, StringRef Name, Expr *E);
};

class TypeDecl : public Decl {
  Type *Ty;

protected:
  TypeDecl(SourceLocation Loc, StringRef Name, Type *Ty)
      : Decl(Loc, Name), Ty(Ty) {}

public:
  static TypeDecl *create(SourceLocation Loc, StringRef Name, Type *Ty);
};

class VariableDecl : public Decl {
  Type *Ty;

protected:
  VariableDecl(SourceLocation Loc, StringRef Name, Type *Ty)
      : Decl(Loc, Name), Ty(Ty) {}

public:
  static VariableDecl *create(SourceLocation Loc, StringRef Name, Type *Ty);
};

class Stmt {};

using StmtList = std::vector<Stmt *>;

class IfStmt : public Stmt {
  Expr *Cond;

protected:
  IfStmt(Expr *Cond) : Cond(Cond) {}

public:
  static IfStmt *create(Expr *Cond);
};

class CaseStmt : public Stmt {
public:
  static CaseStmt *create();
};

class WhileStmt : public Stmt {
  Expr *Cond;
  StmtList Stmts;
  SourceLocation Loc;

protected:
  WhileStmt(Expr *Cond, StmtList &Stmts, SourceLocation Loc)
      : Cond(Cond), Stmts(Stmts), Loc(Loc) {}

public:
  static WhileStmt *create(Expr *Cond, StmtList &Stmts, SourceLocation Loc);
};

class RepeatStmt : public Stmt {
  Expr *Cond;
  StmtList Stmts;
  SourceLocation Loc;

protected:
  RepeatStmt(Expr *Cond, StmtList &Stmts, SourceLocation Loc)
      : Cond(Cond), Stmts(Stmts), Loc(Loc) {}

public:
  static RepeatStmt *create(Expr *Cond, StmtList &Stmts, SourceLocation Loc);
};

class ForStmt : public Stmt {
public:
  static ForStmt *create();
};

class LoopStmt : public Stmt {
  StmtList Stmts;
  SourceLocation Loc;

protected:
  LoopStmt(StmtList &Stmts, SourceLocation Loc) : Stmts(Stmts), Loc(Loc) {}

public:
  static LoopStmt *create(StmtList &Stmts, SourceLocation Loc);
};

class WithStmt : public Stmt {
public:
  static WithStmt *create();
};

class ExitStmt : public Stmt {
  SourceLocation Loc;

protected:
  ExitStmt(SourceLocation Loc) : Loc(Loc) {}

public:
  static ExitStmt *create(SourceLocation Loc);
};

class ReturnStmt : public Stmt {
  Expr *E;

protected:
  ReturnStmt(Expr *E) : E(E) {}

public:
  static ReturnStmt *create(Expr *E);
};

class RetryStmt : public Stmt {
  SourceLocation Loc;

protected:
  RetryStmt(SourceLocation Loc) : Loc(Loc) {}

public:
  static RetryStmt *create(SourceLocation Loc);
};

} // namespace m2lang

#endif