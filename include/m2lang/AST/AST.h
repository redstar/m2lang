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

class SimpleExpression;
class Term;
class Factor;

class Expression : public Expr {
protected:
  Expr *Left;
  Expr *Right;
  const OperatorInfo Op;

  Expression(Expr *Left, Expr *Right, OperatorInfo Op)
      : Left(Left), Right(Right), Op(Op) {}

public:
  static Expression *create(Expr *Left, Expr *Right, const OperatorInfo &Op);

  static Expression *create(Expr *E) {
    return create(E, nullptr, OperatorInfo(0, tok::unknown, true));
  }
};

class SimpleExpression : public Expr {
public:
  using OpAndTerm = std::pair<tok::TokenKind, Term *>;

protected:
  tok::TokenKind UnaryOp;
  Term *T;
  std::vector<OpAndTerm> OpsAndTerms;

  SimpleExpression(tok::TokenKind UnaryOp, Term *T,
                   std::vector<OpAndTerm> OpsAndTerms)
      : UnaryOp(UnaryOp), T(T), OpsAndTerms(OpsAndTerms) {}

public:
  static SimpleExpression *create(tok::TokenKind UnaryOp, Term *T,
                                  std::vector<OpAndTerm> OpsAndTerms);
};

class Term : public Expr {
public:
  using OpAndFactor = std::pair<tok::TokenKind, Factor *>;

protected:
  Factor *F;
  std::vector<OpAndFactor> OpsAndFactors;

  Term(Factor *F, std::vector<OpAndFactor> OpsAndFactors)
      : F(F), OpsAndFactors(OpsAndFactors) {}

public:
  static Term *create(Factor *F, std::vector<OpAndFactor> OpsAndFactors);
};

class Factor : public Expr {};

class Decl {
protected:
  SourceLocation Loc;
  StringRef Name;

  Decl(SourceLocation Loc, StringRef Name) : Loc(Loc), Name(Name) {}
};

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

class IfStmt : public Stmt {
public:
  static IfStmt *create();
};

class CaseStmt : public Stmt {
public:
  static CaseStmt *create();
};

class WhileStmt : public Stmt {
public:
  static WhileStmt *create();
};

class RepeatStmt : public Stmt {
public:
  static RepeatStmt *create();
};

class ForStmt : public Stmt {
public:
  static ForStmt *create();
};

class LoopStmt : public Stmt {
public:
  static LoopStmt *create();
};

class WithStmt : public Stmt {
public:
  static WithStmt *create();
};

class ExitStmt : public Stmt {
public:
  static ExitStmt *create();
};

class ReturnStmt : public Stmt {
public:
  static ReturnStmt *create();
};

} // namespace m2lang

#endif