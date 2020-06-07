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
#include "m2lang/Basic/TokenKinds.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include <string>
#include <vector>

namespace m2lang {

class CompilationUnit {
public:
  enum Type {
    ProgramModule,
    DefinitionModule,
    ImplementationModule,
    GenericDefinitionModule,
    GenerigImplementationModule,
    RefiningDefinitionModule,
    RefiningImplementationModule
  };

private:
  std::string Name;
  bool IsUnsafeGuarded;
  int Priority;

public:
  static CompilationUnit *create();
};

class Type {};

class OperatorInfo {
  SMLoc Loc;
  uint32_t Kind : 16;
  uint32_t IsUnspecified : 1;

public:
  OperatorInfo() : Loc(), Kind(tok::unknown), IsUnspecified(true) {}
  OperatorInfo(SMLoc Loc, tok::TokenKind Kind, bool IsUnspecified = false)
      : Loc(Loc), Kind(Kind), IsUnspecified(IsUnspecified) {}

  SMLoc getLocation() const { return Loc; }
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
    return create(E, nullptr, OperatorInfo(SMLoc(), tok::unknown, true));
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
  SMLoc Loc;
  StringRef Name;

  Decl(SMLoc Loc, StringRef Name) : Loc(Loc), Name(Name) {}
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
  ConstantDecl(SMLoc Loc, StringRef Name, Expr *E) : Decl(Loc, Name), E(E) {}

public:
  static ConstantDecl *create(SMLoc Loc, StringRef Name, Expr *E);
};

class TypeDecl : public Decl {
  Type *Ty;

protected:
  TypeDecl(SMLoc Loc, StringRef Name, Type *Ty) : Decl(Loc, Name), Ty(Ty) {}

public:
  static TypeDecl *create(SMLoc Loc, StringRef Name, Type *Ty);
};

class VariableDecl : public Decl {
  Type *Ty;

protected:
  VariableDecl(SMLoc Loc, StringRef Name, Type *Ty) : Decl(Loc, Name), Ty(Ty) {}

public:
  static VariableDecl *create(SMLoc Loc, StringRef Name, Type *Ty);
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
  SMLoc Loc;

protected:
  WhileStmt(Expr *Cond, StmtList &Stmts, SMLoc Loc)
      : Cond(Cond), Stmts(Stmts), Loc(Loc) {}

public:
  static WhileStmt *create(Expr *Cond, StmtList &Stmts, SMLoc Loc);
};

class RepeatStmt : public Stmt {
  Expr *Cond;
  StmtList Stmts;
  SMLoc Loc;

protected:
  RepeatStmt(Expr *Cond, StmtList &Stmts, SMLoc Loc)
      : Cond(Cond), Stmts(Stmts), Loc(Loc) {}

public:
  static RepeatStmt *create(Expr *Cond, StmtList &Stmts, SMLoc Loc);
};

class ForStmt : public Stmt {
public:
  static ForStmt *create();
};

class LoopStmt : public Stmt {
  StmtList Stmts;
  SMLoc Loc;

protected:
  LoopStmt(StmtList &Stmts, SMLoc Loc) : Stmts(Stmts), Loc(Loc) {}

public:
  static LoopStmt *create(StmtList &Stmts, SMLoc Loc);
};

class WithStmt : public Stmt {
public:
  static WithStmt *create();
};

class ExitStmt : public Stmt {
  SMLoc Loc;

protected:
  ExitStmt(SMLoc Loc) : Loc(Loc) {}

public:
  static ExitStmt *create(SMLoc Loc);
};

class ReturnStmt : public Stmt {
  Expr *E;

protected:
  ReturnStmt(Expr *E) : E(E) {}

public:
  static ReturnStmt *create(Expr *E);
};

class RetryStmt : public Stmt {
  SMLoc Loc;

protected:
  RetryStmt(SMLoc Loc) : Loc(Loc) {}

public:
  static RetryStmt *create(SMLoc Loc);
};

} // namespace m2lang

#endif