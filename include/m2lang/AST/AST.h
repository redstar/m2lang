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
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include <string>
#include <vector>

namespace m2lang {

class Declaration;
class Expression;
class FormalParameter;
class Selector;
class Statement;
class Type;
class TypeDenoter;
using ActualParameter = llvm::PointerUnion<Expression *, Type *>;

// TODO Evaluate average size of these lists.
using ActualParameterList = SmallVector<ActualParameter, 8>;
using DeclarationList = SmallVector<Declaration *, 8>;
using FormalParameterList = SmallVector<FormalParameter *, 8>;
using ExpressionList = SmallVector<Expression *, 8>;
using SelectorList = llvm::SmallVector<Selector *, 8>;
using StatementList = SmallVector<Statement *, 8>;
using TypeDenoterList = SmallVector<TypeDenoter *, 8>;

class Block {
  StatementList Stmts;
  StatementList ExceptStmts;

public:
  Block() = default;
  Block(const StatementList &Stmts, const StatementList &ExceptStmts)
      : Stmts(Stmts), ExceptStmts(ExceptStmts) {}

  StatementList getStmts() const { return Stmts; }
  StatementList getExceptStmts() const { return ExceptStmts; }
};

class Declaration {
public:
  enum DeclKind {
    DK_DefinitionModule,
    DK_ImplementationModule,
    DK_RefiningDefinitionModule,
    DK_RefiningImplementationModule,
    DK_Constant,
    DK_Type,
    DK_Var,
    DK_FormalParameter,
    DK_Procedure,
    DK_LocalModule,
    DK_Class,
  };

private:
  const DeclKind Kind;
  Declaration *EnclosingDecl;
  SMLoc Loc;
  StringRef Name;

protected:
  Declaration(DeclKind Kind, Declaration *EnclosingDecl, SMLoc Loc,
              StringRef Name)
      : Kind(Kind), EnclosingDecl(EnclosingDecl), Loc(Loc), Name(Name) {}

public:
  DeclKind getKind() const { return Kind; }
  Declaration *getEnclosingDecl() const { return EnclosingDecl; }
  SMLoc getLoc() const { return Loc; }
  StringRef getName() const { return Name; }
};

class CompilationModule : public Declaration {
protected:
  CompilationModule(DeclKind Kind, Declaration *EnclosingDecl, SMLoc Loc,
                    StringRef Name)
      : Declaration(Kind, EnclosingDecl, Loc, Name) {}

public:
  static bool classof(const Declaration *Decl) {
    return Decl->getKind() >= DK_DefinitionModule &&
           Decl->getKind() < DK_Constant;
  }
};

class ImplementationModule : public CompilationModule {
  DeclarationList Decls;
  Block InitBlk;
  Block FinalBlk;
  Expression *Protection;
  bool IsUnsafeGuarded;
  bool IsProgramModule;

protected:
  ImplementationModule(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
                       bool IsUnsafeGuarded)
      : CompilationModule(DK_ImplementationModule, EnclosingDecl, Loc, Name),
        IsUnsafeGuarded(IsUnsafeGuarded), IsProgramModule(false) {}

public:
  static ImplementationModule *create(Declaration *EnclosingDecl, SMLoc Loc,
                                      StringRef Name, bool IsUnsafeGuarded);

  void update(Expression *Protection, const DeclarationList &Decls,
              const Block &InitBlk, const Block &FinalBlk,
              bool IsProgramModule) {
    this->Protection = Protection;
    this->Decls = Decls;
    this->InitBlk = InitBlk;
    this->FinalBlk = FinalBlk;
    this->IsProgramModule = IsProgramModule;
  }

  const DeclarationList &getDecls() const { return Decls; }
  bool isUnsafeGuarded() const { return IsUnsafeGuarded; }
  bool isProgramModule() const { return IsProgramModule; }

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_ImplementationModule;
  }
};

class DefinitionModule : public CompilationModule {
  DeclarationList Decls;
  bool IsUnsafeGuarded;

protected:
  DefinitionModule(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
                   bool IsUnsafeGuarded)
      : CompilationModule(DK_DefinitionModule, EnclosingDecl, Loc, Name),
        IsUnsafeGuarded(IsUnsafeGuarded) {}

public:
  static DefinitionModule *create(Declaration *EnclosingDecl, SMLoc Loc,
                                  StringRef Name, bool IsUnsafeGuarded);

  void update(const DeclarationList &Decls) { this->Decls = Decls; }

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_DefinitionModule;
  }
};

class RefiningDefinitionModule : public CompilationModule {
  ActualParameterList ActualModulParams;
  bool IsUnsafeGuarded;

protected:
  RefiningDefinitionModule(Declaration *EnclosingDecl, SMLoc Loc,
                           StringRef Name, bool IsUnsafeGuarded)
      : CompilationModule(DK_RefiningDefinitionModule, EnclosingDecl, Loc,
                          Name),
        IsUnsafeGuarded(IsUnsafeGuarded) {}

public:
  static RefiningDefinitionModule *create(Declaration *EnclosingDecl, SMLoc Loc,
                                          StringRef Name, bool IsUnsafeGuarded);

  void update(const ActualParameterList &ActualModulParams) {
    this->ActualModulParams = ActualModulParams;
  }

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_RefiningDefinitionModule;
  }
};

class RefiningImplementationModule : public CompilationModule {
  ActualParameterList ActualModulParams;
  bool IsUnsafeGuarded;

protected:
  RefiningImplementationModule(Declaration *EnclosingDecl, SMLoc Loc,
                               StringRef Name, bool IsUnsafeGuarded)
      : CompilationModule(DK_RefiningImplementationModule, EnclosingDecl, Loc,
                          Name),
        IsUnsafeGuarded(IsUnsafeGuarded) {}

public:
  static RefiningImplementationModule *create(Declaration *EnclosingDecl,
                                              SMLoc Loc, StringRef Name,
                                              bool IsUnsafeGuarded);

  void update(const ActualParameterList &ActualModulParams) {
    this->ActualModulParams = ActualModulParams;
  }

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_RefiningImplementationModule;
  }
};

class Type : public Declaration {
  TypeDenoter *Denoter;

protected:
  Type(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
       TypeDenoter *Denoter)
      : Declaration(DK_Type, EnclosingDecl, Loc, Name), Denoter(Denoter) {}

public:
  static Type *create(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
                      TypeDenoter *Denoter);

  TypeDenoter *getTypeDenoter() const { return Denoter; }

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_Type;
  }
};

class Constant : public Declaration {
  Type *TypeDecl;
  Expression *ConstExpr;

protected:
  Constant(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
           Type *TypeDecl, Expression *ConstExpr)
      : Declaration(DK_Constant, EnclosingDecl, Loc, Name), TypeDecl(TypeDecl),
        ConstExpr(ConstExpr) {}

public:
  static Constant *create(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
                          Type *TypeDecl, Expression *ConstExpr);

  TypeDenoter *getTypeDenoter() const { return TypeDecl->getTypeDenoter(); }

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_Constant;
  }
};

class Variable : public Declaration {
  TypeDenoter *Denoter;
  Expression *Addr;

protected:
  Variable(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
           TypeDenoter *Denoter, Expression *Addr)
      : Declaration(DK_Var, EnclosingDecl, Loc, Name), Denoter(Denoter),
        Addr(Addr) {}

public:
  static Variable *create(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
                          TypeDenoter *Denoter, Expression *Addr);

  TypeDenoter *getTypeDenoter() const { return Denoter; }

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_Var;
  }
};

class FormalParameter : public Declaration {
  Type *Ty;
  bool IsVar;
  // Number of "ARRAY OF" prefixes.
  // This is only > 0 for formal types (e.g. in procedures, modules)
  unsigned OpenArrayLevel;

protected:
  FormalParameter(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
                  Type *Ty, bool IsVar, unsigned OpenArrayLevel)
      : Declaration(DK_FormalParameter, EnclosingDecl, Loc, Name), Ty(Ty),
        IsVar(IsVar), OpenArrayLevel(OpenArrayLevel) {}

public:
  static FormalParameter *create(Declaration *EnclosingDecl, SMLoc Loc,
                                 StringRef Name, Type *Ty, bool IsVar,
                                 unsigned OpenArrayLevel = 0);

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_FormalParameter;
  }
};

class Procedure : public Declaration {
  FormalParameterList Params;
  Type *ResultType;
  DeclarationList Decls;
  Block Body;
  bool IsForward;

protected:
  Procedure(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Declaration(DK_Procedure, EnclosingDecl, Loc, Name),
        ResultType(nullptr), IsForward(false) {}

public:
  static Procedure *create(Declaration *EnclosingDecl, SMLoc Loc,
                           StringRef Name);

  void update(const FormalParameterList &Params, Type *ResultType,
              const DeclarationList &Decls, const Block &Body) {
    this->Params = Params;
    this->ResultType = ResultType;
    this->Decls = Decls;
    this->Body = Body;
  }

  bool isForward() const { return IsForward; }
  void setForward() { IsForward = true; }

  const FormalParameterList &getParams() const { return Params; }
  Type *getResultType() const { return ResultType; }
  const DeclarationList &getDecls() const { return Decls; }
  const Block &getBody() const { return Body; }

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_Procedure;
  }
};

class LocalModule : public Declaration {
protected:
  LocalModule(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Declaration(DK_LocalModule, EnclosingDecl, Loc, Name) {}

public:
  static LocalModule *create(Declaration *EnclosingDecl, SMLoc Loc,
                             StringRef Name);

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_LocalModule;
  }
};

class Class : public Declaration {
protected:
  Class(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Declaration(DK_Class, EnclosingDecl, Loc, Name) {}

public:
  static Class *create(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name);

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_Class;
  }
};

class TypeDenoter {
public:
  enum TypeDenoterKind {
    TDK_Pervasive,
    TDK_Named,
    TDK_Record,
    TDK_Array,
    TDK_Pointer,
    TDK_Procedure,
    TDK_Subrange,
    TDK_Enumeration,
    TDK_Set,
  };

private:
  const TypeDenoterKind Kind;

protected:
  TypeDenoter(TypeDenoterKind Kind) : Kind(Kind) {}

public:
  TypeDenoterKind getKind() const { return Kind; }
};

class PervasiveType : public TypeDenoter {

protected:
  PervasiveType() : TypeDenoter(TDK_Pervasive) {}

public:
  static PervasiveType *create();

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Pervasive;
  }
};

class NamedType : public TypeDenoter {
  Type *TypeDecl;

protected:
  NamedType(Type *TypeDecl) : TypeDenoter(TDK_Named), TypeDecl(TypeDecl) {}

public:
  static NamedType *create(Type *TypeDecl);

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Named;
  }
};

class RecordType : public TypeDenoter {

protected:
  RecordType() : TypeDenoter(TDK_Record) {}

public:
  static RecordType *create();

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Record;
  }
};

class ArrayType : public TypeDenoter {
  TypeDenoter *ComponentType;
  TypeDenoterList IndexList;

protected:
  ArrayType(TypeDenoter *ComponentType, const TypeDenoterList &IndexList)
      : TypeDenoter(TDK_Array), ComponentType(ComponentType),
        IndexList(IndexList) {}

public:
  static ArrayType *create(TypeDenoter *ComponentType,
                           const TypeDenoterList &IndexList);

  TypeDenoter *getComponentType() const { return ComponentType; }
  const TypeDenoterList &getIndexList() const { return IndexList; }

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Array;
  }
};

class ProcedureType : public TypeDenoter {
  Type *ResultType;

protected:
  ProcedureType(Type *ResultType)
      : TypeDenoter(TDK_Procedure), ResultType(ResultType) {}

public:
  static ProcedureType *create(Type *ResultType);

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Procedure;
  }
};

class PointerType : public TypeDenoter {
  TypeDenoter *TyDen;
  StringRef Name;
  bool IsResolved;

protected:
  PointerType(TypeDenoter *TyDen)
      : TypeDenoter(TDK_Pointer), TyDen(TyDen), IsResolved(true) {}
  PointerType(const StringRef &Name)
      : TypeDenoter(TDK_Pointer), TyDen(nullptr), Name(Name),
        IsResolved(false) {}

public:
  static PointerType *create(TypeDenoter *TyDen);
  static PointerType *create(const StringRef &Name);

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Pointer;
  }
};

class SubrangeType : public TypeDenoter {
  Type *RangeType; // If specified, then it is an ordinal type
  Expression *From;
  Expression *To;

protected:
  SubrangeType(Type *RangeType, Expression *From, Expression *To)
      : TypeDenoter(TDK_Subrange), From(From), To(To) {}

public:
  static SubrangeType *create(Type *RangeType, Expression *From,
                              Expression *To);

  Type *getRangeType() const { return RangeType; }
  Expression *getFrom() const { return From; }
  Expression *getTo() const { return To; }

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Subrange;
  }
};

class EnumerationType : public TypeDenoter {

protected:
  EnumerationType() : TypeDenoter(TDK_Enumeration) {}

public:
  static EnumerationType *create();

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Enumeration;
  }
};

class SetType : public TypeDenoter {
  TypeDenoter *BaseType;
  bool IsPacked;

protected:
  SetType(TypeDenoter *TyBaseTypeen, bool IsPacked)
      : TypeDenoter(TDK_Set), BaseType(BaseType), IsPacked(IsPacked) {}

public:
  static SetType *create(TypeDenoter *BaseType, bool IsPacked);

  TypeDenoter *getBaseType() const { return BaseType; }
  bool isPacked() const { return IsPacked; }

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Set;
  }
};

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

class Expression {
public:
  enum ExpressionKind {
    EK_Infix,
    EK_Prefix,
    EK_IntegerLiteral,
    EK_RealLiteral,
    EK_StringLiteral,
    EK_CharLiteral,
    EK_BooleanLiteral,
    EK_Designator,
    EK_FunctionCall,
    EK_ValueConstructor,
    // Incomplete
  };

private:
  const ExpressionKind Kind;
  TypeDenoter *Denoter;

  // Synthesized attribute: Is expression constant?
  bool IsConst;

protected:
  Expression(ExpressionKind Kind, TypeDenoter *Denoter, bool IsConst)
      : Kind(Kind), Denoter(Denoter), IsConst(IsConst) {}

public:
  TypeDenoter *getTypeDenoter() const { return Denoter; }
  bool isConst() const { return IsConst; }

  ExpressionKind getKind() const { return Kind; }
};

class InfixExpression : public Expression {
private:
  Expression *Left;
  Expression *Right;
  const OperatorInfo Op;

protected:
  InfixExpression(Expression *Left, Expression *Right, OperatorInfo Op,
                  TypeDenoter *Denoter, bool IsConst)
      : Expression(EK_Infix, Denoter, IsConst), Left(Left), Right(Right),
        Op(Op) {}

public:
  static InfixExpression *create(Expression *Left, Expression *Right,
                                 const OperatorInfo &Op, TypeDenoter *Denoter,
                                 bool IsConst);

  Expression *getLeft() { return Left; }
  Expression *getRight() { return Right; }

  static InfixExpression *create(Expression *E, TypeDenoter *Denoter,
                                 bool IsConst) {
    return create(E, nullptr, OperatorInfo(SMLoc(), tok::unknown, true),
                  Denoter, IsConst);
  }

  static bool classof(const Expression *Expr) {
    return Expr->getKind() == EK_Infix;
  }
};

class PrefixExpression : public Expression {
protected:
  Expression *E;
  const OperatorInfo Op;

  PrefixExpression(Expression *E, OperatorInfo Op, TypeDenoter *Denoter,
                   bool IsConst)
      : Expression(EK_Prefix, Denoter, IsConst), E(E), Op(Op) {}

public:
  static PrefixExpression *create(Expression *E, const OperatorInfo &Op,
                                  TypeDenoter *Denoter, bool IsConst);

  static bool classof(const Expression *Expr) {
    return Expr->getKind() == EK_Prefix;
  }
};

template <Expression::ExpressionKind K, typename T>
class Literal : public Expression {
  T Value;

protected:
  Literal(const T &Value, TypeDenoter *Denoter)
      : Expression(K, Denoter, true), Value(Value) {}

public:
  static Literal<K, T> *create(const T &Value, TypeDenoter *Denoter) {
    return new Literal<K, T>(Value, Denoter);
  }

  T getValue() const { return Value; }

  static bool classof(const Expression *Expr) { return Expr->getKind() == K; }
};

using IntegerLiteral = Literal<Expression::EK_IntegerLiteral, llvm::APInt>;
using RealLiteral = Literal<Expression::EK_RealLiteral, llvm::APFloat>;
using StringLiteral = Literal<Expression::EK_StringLiteral, StringRef>;
using CharLiteral = Literal<Expression::EK_StringLiteral, unsigned>;
using BooleanLiteral = Literal<Expression::EK_BooleanLiteral, bool>;

class Selector {};

class IndexedSelector : public Selector {};

class FieldSelector : public Selector {};

class Designator : public Expression {
  Declaration *Decl;
  SelectorList Selectors;

  // Synthesized attribute: Is expression variable (denotes an address)?
  bool IsVariable;

protected:
  Designator(Declaration *Decl, const SelectorList &Selectors,
             TypeDenoter *Denoter, bool IsVariable, bool IsConst)
      : Expression(EK_Designator, Denoter, IsConst), Selectors(Selectors) {}

public:
  static Designator *create(Declaration *Decl, const SelectorList &Selectors,
                            TypeDenoter *Denoter, bool IsVariable,
                            bool IsConst);

  Declaration *getDecl() const { return Decl; }
  const SelectorList &getSelectorList() const { return Selectors; }
  bool isVariable() const { return IsVariable; }

  static bool classof(const Expression *Expr) {
    return Expr->getKind() == EK_Designator;
  }
};

class FunctionCall : public Expression {
  Designator *Desig;
  ActualParameterList ActualParameters;

protected:
  FunctionCall(Designator *Desig, const ActualParameterList &ActualParameters,
               TypeDenoter *Denoter, bool IsConst)
      : Expression(EK_FunctionCall, Denoter, IsConst), Desig(Desig),
        ActualParameters(ActualParameters) {}

public:
  static FunctionCall *create(Designator *Desig,
                              const ActualParameterList &ActualParameters,
                              TypeDenoter *Denoter, bool IsConst);

  static bool classof(const Expression *Expr) {
    return Expr->getKind() == EK_FunctionCall;
  }
};

class ValueConstructor : public Expression {
protected:
  ValueConstructor(TypeDenoter *Denoter)
      : Expression(EK_ValueConstructor, Denoter, true) {}

public:
  static ValueConstructor *create(TypeDenoter *Denoter);

  static bool classof(const Expression *Expr) {
    return Expr->getKind() == EK_ValueConstructor;
  }
};

class Statement {
public:
  enum StmtKind {
    SK_Assignment,
    SK_ProcedureCall,
    SK_If,
    SK_Case,
    SK_While,
    SK_Repeat,
    SK_For,
    SK_Loop,
    SK_With,
    SK_Exit,
    SK_Return,
    SK_Retry
  };

private:
  const StmtKind Kind;

protected:
  Statement(StmtKind Kind) : Kind(Kind) {}

public:
  StmtKind getKind() const { return Kind; }
};

class AssignmentStatement : public Statement {
  Designator *Left;
  Expression *Right;

protected:
  AssignmentStatement(Designator *Left, Expression *Right)
      : Statement(SK_Assignment), Left(Left), Right(Right) {}

public:
  static AssignmentStatement *create(Designator *Left, Expression *Right);

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_Assignment;
  }
};

class ProcedureCallStatement : public Statement {
  Designator *Proc;
  ActualParameterList ActualParameters;

protected:
  ProcedureCallStatement(Designator *Proc,
                         const ActualParameterList &ActualParameters)
      : Statement(SK_ProcedureCall), Proc(Proc),
        ActualParameters(ActualParameters) {}

public:
  static ProcedureCallStatement *
  create(Designator *Proc, const ActualParameterList &ActualParameters);

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_ProcedureCall;
  }
};

class IfStatement : public Statement {
  Expression *Cond;

protected:
  IfStatement(Expression *Cond) : Statement(SK_If), Cond(Cond) {}

public:
  static IfStatement *create(Expression *Cond);

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_If;
  }
};

class CaseStatement : public Statement {
protected:
  CaseStatement() : Statement(SK_Case) {}

public:
  static CaseStatement *create();

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_Case;
  }
};

class WhileStatement : public Statement {
  Expression *Cond;
  StatementList Stmts;
  SMLoc Loc;

protected:
  WhileStatement(Expression *Cond, StatementList &Stmts, SMLoc Loc)
      : Statement(SK_While), Cond(Cond), Stmts(Stmts), Loc(Loc) {}

public:
  static WhileStatement *create(Expression *Cond, StatementList &Stmts,
                                SMLoc Loc);

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_While;
  }
};

class RepeatStatement : public Statement {
  Expression *Cond;
  StatementList Stmts;
  SMLoc Loc;

protected:
  RepeatStatement(Expression *Cond, StatementList &Stmts, SMLoc Loc)
      : Statement(SK_Repeat), Cond(Cond), Stmts(Stmts), Loc(Loc) {}

public:
  static RepeatStatement *create(Expression *Cond, StatementList &Stmts,
                                 SMLoc Loc);

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_Repeat;
  }
};

class ForStatement : public Statement {
  SMLoc Loc;
  Variable *ControlVariable;
  Expression *InitialValue;
  Expression *FinalValue;
  Expression *StepSize;
  StatementList ForStmts;

protected:
  ForStatement(SMLoc Loc, Variable *ControlVariable, Expression *InitialValue,
               Expression *FinalValue, Expression *StepSize,
               const StatementList &ForStmts)
      : Statement(SK_For), Loc(Loc), ControlVariable(ControlVariable),
        InitialValue(InitialValue), FinalValue(FinalValue), StepSize(StepSize),
        ForStmts(ForStmts) {}

public:
  static ForStatement *create(SMLoc Loc, Variable *ControlVariable,
                              Expression *InitialValue, Expression *FinalValue,
                              Expression *StepSize,
                              const StatementList &ForStmts);

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_For;
  }
};

class LoopStatement : public Statement {
  StatementList Stmts;
  SMLoc Loc;

protected:
  LoopStatement(StatementList &Stmts, SMLoc Loc)
      : Statement(SK_Loop), Stmts(Stmts), Loc(Loc) {}

public:
  static LoopStatement *create(StatementList &Stmts, SMLoc Loc);

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_Loop;
  }
};

class WithStatement : public Statement {
  WithStatement() : Statement(SK_With) {}

public:
  static WithStatement *create();

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_With;
  }
};

class ExitStatement : public Statement {
  SMLoc Loc;

protected:
  ExitStatement(SMLoc Loc) : Statement(SK_Exit), Loc(Loc) {}

public:
  static ExitStatement *create(SMLoc Loc);

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_Exit;
  }
};

class ReturnStatement : public Statement {
  Expression *E;

protected:
  ReturnStatement(Expression *E) : Statement(SK_Return), E(E) {}

public:
  static ReturnStatement *create(Expression *E);

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_Return;
  }
};

class RetryStatement : public Statement {
  SMLoc Loc;

protected:
  RetryStatement(SMLoc Loc) : Statement(SK_Retry), Loc(Loc) {}

public:
  static RetryStatement *create(SMLoc Loc);

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_Retry;
  }
};

} // namespace m2lang

#endif