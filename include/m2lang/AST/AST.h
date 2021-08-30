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

#include "m2lang/AST/ASTContext.h"
#include "m2lang/AST/PervasiveTypeKinds.h"
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

#if 0
namespace m2lang {

class Constant;
class Declaration;
class Expression;
class FixedRecordField;
class FormalParameter;
class FormalParameterType;
class Selector;
class Statement;
class Type;
class TypeDenoter;
using ActualParameter = llvm::PointerUnion<Expression *, Type *>;

// TODO Evaluate average size of these lists.
using ActualParameterList = SmallVector<ActualParameter, 4>;
using ConstantList = llvm::SmallVector<Constant *, 4>;
using DeclarationList = SmallVector<Declaration *, 4>;
using FormalParameterList = SmallVector<FormalParameter *, 4>;
using ExpressionList = SmallVector<Expression *, 4>;
using SelectorList = llvm::SmallVector<Selector *, 4>;
using StatementList = SmallVector<Statement *, 4>;
using TypeDenoterList = SmallVector<TypeDenoter *, 4>;

using FormalParameterTypeList = llvm::SmallVector<FormalParameterType, 8>;
using RecordFieldList = SmallVector<FixedRecordField, 4>;

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

#define AST_DECLARATION
#include "m2lang/AST/ast.inc"

} // namespace m2lang

#else

namespace m2lang {

class Constant;
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
using ConstantList = llvm::SmallVector<Constant *, 8>;
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

public:
  Declaration(DeclKind Kind, Declaration *EnclosingDecl, SMLoc Loc,
              StringRef Name)
      : Kind(Kind), EnclosingDecl(EnclosingDecl), Loc(Loc), Name(Name) {}

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

public:
  ImplementationModule(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
                       bool IsUnsafeGuarded)
      : CompilationModule(DK_ImplementationModule, EnclosingDecl, Loc, Name),
        IsUnsafeGuarded(IsUnsafeGuarded), IsProgramModule(false) {}

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

public:
  DefinitionModule(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
                   bool IsUnsafeGuarded)
      : CompilationModule(DK_DefinitionModule, EnclosingDecl, Loc, Name),
        IsUnsafeGuarded(IsUnsafeGuarded) {}

  void update(const DeclarationList &Decls) { this->Decls = Decls; }

  const DeclarationList &getDecls() const { return Decls; }
  bool isUnsafeGuarded() const { return IsUnsafeGuarded; }

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_DefinitionModule;
  }
};

class RefiningDefinitionModule : public CompilationModule {
  ActualParameterList ActualModulParams;
  bool IsUnsafeGuarded;

public:
  RefiningDefinitionModule(Declaration *EnclosingDecl, SMLoc Loc,
                           StringRef Name, bool IsUnsafeGuarded)
      : CompilationModule(DK_RefiningDefinitionModule, EnclosingDecl, Loc,
                          Name),
        IsUnsafeGuarded(IsUnsafeGuarded) {}

  void update(const ActualParameterList &ActualModulParams) {
    this->ActualModulParams = ActualModulParams;
  }

  const ActualParameterList &getActualModulParams() const { return ActualModulParams; }
  bool isUnsafeGuarded() const { return IsUnsafeGuarded; }

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_RefiningDefinitionModule;
  }
};

class RefiningImplementationModule : public CompilationModule {
  ActualParameterList ActualModulParams;
  bool IsUnsafeGuarded;

public:
  RefiningImplementationModule(Declaration *EnclosingDecl, SMLoc Loc,
                               StringRef Name, bool IsUnsafeGuarded)
      : CompilationModule(DK_RefiningImplementationModule, EnclosingDecl, Loc,
                          Name),
        IsUnsafeGuarded(IsUnsafeGuarded) {}

  void update(const ActualParameterList &ActualModulParams) {
    this->ActualModulParams = ActualModulParams;
  }

  const ActualParameterList &getActualModulParams() const { return ActualModulParams; }
  bool isUnsafeGuarded() const { return IsUnsafeGuarded; }

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_RefiningImplementationModule;
  }
};

class Type : public Declaration {
  TypeDenoter *Denoter;

public:
  Type(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
       TypeDenoter *Denoter)
      : Declaration(DK_Type, EnclosingDecl, Loc, Name), Denoter(Denoter) {}

  TypeDenoter *getTypeDenoter() const { return Denoter; }

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_Type;
  }
};

class Constant : public Declaration {
  TypeDenoter *TyDe;
  Expression *ConstExpr;

public:
  Constant(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
           TypeDenoter *TyDe, Expression *ConstExpr)
      : Declaration(DK_Constant, EnclosingDecl, Loc, Name), TyDe(TyDe),
        ConstExpr(ConstExpr) {}

  TypeDenoter *getTypeDenoter() const { return TyDe; }
  Expression *getConstExpr() const { return ConstExpr; }

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_Constant;
  }
};

class Variable : public Declaration {
  TypeDenoter *Denoter;
  Expression *Addr;

public:
  Variable(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
           TypeDenoter *Denoter, Expression *Addr)
      : Declaration(DK_Var, EnclosingDecl, Loc, Name), Denoter(Denoter),
        Addr(Addr) {}

  TypeDenoter *getTypeDenoter() const { return Denoter; }
  Expression *getAddr() const { return Addr; }

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_Var;
  }
};

class FormalParameter : public Declaration {
  TypeDenoter *Ty;
  bool IsCallByReference;

public:
  FormalParameter(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name,
                  TypeDenoter *Ty, bool IsCallByReference)
      : Declaration(DK_FormalParameter, EnclosingDecl, Loc, Name), Ty(Ty),
        IsCallByReference(IsCallByReference) {}

  TypeDenoter *getType() const { return Ty; }
  bool isCallByReference() const { return IsCallByReference; }

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

public:
  Procedure(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Declaration(DK_Procedure, EnclosingDecl, Loc, Name),
        ResultType(nullptr), IsForward(false) {}

  // Update the procedure heading.
  void update(const FormalParameterList &Params, Type *ResultType) {
    this->Params = Params;
    this->ResultType = ResultType;
  }

  // Update the procedure body.
  void update(const DeclarationList &Decls, const Block &Body) {
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
public:
  LocalModule(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Declaration(DK_LocalModule, EnclosingDecl, Loc, Name) {}

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_LocalModule;
  }
};

class Class : public Declaration {
public:
  Class(Declaration *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Declaration(DK_Class, EnclosingDecl, Loc, Name) {}

  static bool classof(const Declaration *Decl) {
    return Decl->getKind() == DK_Class;
  }
};

class TypeDenoter {
public:
  enum TypeDenoterKind {
    TDK_Pervasive,
    TDK_Record,
    TDK_Array,
    TDK_Pointer,
    TDK_Procedure,
    TDK_OpenArray,
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
  pervasive::PervasiveTypeKind TypeKind;

public:
  PervasiveType(pervasive::PervasiveTypeKind TypeKind)
      : TypeDenoter(TDK_Pervasive), TypeKind(TypeKind) {}

  pervasive::PervasiveTypeKind getTypeKind() const { return TypeKind; }

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Pervasive;
  }
};

// TODO Too simple for variant fields.
class FixedRecordField {
  StringRef Name;
  TypeDenoter *TyDe;

public:
  FixedRecordField(StringRef Name, TypeDenoter *TyDe)
      : Name(Name), TyDe(TyDe) {}

  const StringRef getName() const { return Name; }
  TypeDenoter *getTypeDenoter() const { return TyDe; }
};

using RecordFieldList = SmallVector<FixedRecordField, 8>;

class RecordType : public TypeDenoter {
  RecordFieldList Fields;

public:
  RecordType(const RecordFieldList &Fields)
      : TypeDenoter(TDK_Record), Fields(Fields) {}

  const RecordFieldList &getFields() const { return Fields; }

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Record;
  }
};

class ArrayType : public TypeDenoter {
  TypeDenoter *ComponentType;
  TypeDenoter *IndexType;

public:
  ArrayType(TypeDenoter *ComponentType, TypeDenoter *IndexType)
      : TypeDenoter(TDK_Array), ComponentType(ComponentType),
        IndexType(IndexType) {}

  TypeDenoter *getComponentType() const { return ComponentType; }
  TypeDenoter *getIndexType() const { return IndexType; }

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Array;
  }
};

class FormalParameterType {
  SMLoc Loc;
  TypeDenoter *FormalType;
  bool IsCallByReference;

public:
  FormalParameterType(SMLoc Loc, TypeDenoter *FormalType,
                      bool IsCallByReference)
      : Loc(Loc), FormalType(FormalType), IsCallByReference(IsCallByReference) {
  }

  SMLoc getLoc() const { return Loc; }
  TypeDenoter *getFormalType() const { return FormalType; }
  bool isCallByReference() const { return IsCallByReference; }
};

using FormalParameterTypeList = llvm::SmallVector<FormalParameterType, 8>;

class ProcedureType : public TypeDenoter {
  Type *ResultType;
  FormalParameterTypeList ParameterTypes;

public:
  ProcedureType(Type *ResultType, FormalParameterTypeList &ParameterTypes)
      : TypeDenoter(TDK_Procedure), ResultType(ResultType),
        ParameterTypes(ParameterTypes) {}

  Type *getResultType() const { return ResultType; }
  const FormalParameterTypeList &getParameterTypes() const {
    return ParameterTypes;
  }

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Procedure;
  }
};

// A formal type is either a parameter formal type or an open array formal type.
// The parameter formal type (a type identifier) is modelled as the TypeDenoter
// of that type. The OpenArrayFormalType is used to denote the open array formal type.
// ISO 10514:1994, Clause 6.3.10
class OpenArrayFormalType : public TypeDenoter {
  // Can only be another open array or the type denoter of an identified
  // type.
  TypeDenoter *ComponentType;

public:
  OpenArrayFormalType(TypeDenoter *ComponentType)
      : TypeDenoter(TDK_OpenArray), ComponentType(ComponentType) {}

  TypeDenoter *getComponentType() const { return ComponentType; }

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_OpenArray;
  }
};

class PointerType : public TypeDenoter {
  TypeDenoter *TyDen;
  StringRef Name;
  bool IsResolved;

public:
  PointerType(TypeDenoter *TyDen)
      : TypeDenoter(TDK_Pointer), TyDen(TyDen), IsResolved(true) {}
  PointerType(const StringRef &Name)
      : TypeDenoter(TDK_Pointer), TyDen(nullptr), Name(Name),
        IsResolved(false) {}

  TypeDenoter *getTyDen() const { return TyDen; }
  StringRef getName() const { return Name; }
  bool isResolved() const { return IsResolved; }
  void setIsResolved(bool V = true) { IsResolved = V; }

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Pointer;
  }
};

class SubrangeType : public TypeDenoter {
  Type *RangeType; // If specified, then it is an ordinal type
  Expression *From;
  Expression *To;

public:
  SubrangeType(Type *RangeType, Expression *From, Expression *To)
      : TypeDenoter(TDK_Subrange), From(From), To(To) {}

  Type *getRangeType() const { return RangeType; }
  Expression *getFrom() const { return From; }
  Expression *getTo() const { return To; }

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Subrange;
  }
};

class EnumerationType : public TypeDenoter {
  ConstantList Members;

public:
  EnumerationType() : TypeDenoter(TDK_Enumeration) {}

  void addMember(Constant *Member) {
    Members.push_back(Member);
  }

  static bool classof(const TypeDenoter *TyDenot) {
    return TyDenot->getKind() == TDK_Enumeration;
  }
};

class SetType : public TypeDenoter {
  TypeDenoter *BaseType;
  bool IsPacked;

public:
  SetType(TypeDenoter *BaseType, bool IsPacked)
      : TypeDenoter(TDK_Set), BaseType(BaseType), IsPacked(IsPacked) {}

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
    EK_Nil,
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
  void setDenoter(TypeDenoter *TyDen) {
    Denoter = TyDen;
  }

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

public:
  InfixExpression(Expression *Left, Expression *Right, OperatorInfo Op,
                  TypeDenoter *Denoter, bool IsConst)
      : Expression(EK_Infix, Denoter, IsConst), Left(Left), Right(Right),
        Op(Op) {}

  Expression *getLeftExpression() { return Left; }
  Expression *getRightExpression() { return Right; }
  const OperatorInfo &getOperatorInfo() const { return Op; }

  static bool classof(const Expression *Expr) {
    return Expr->getKind() == EK_Infix;
  }
};

class PrefixExpression : public Expression {
public:
  Expression *E;
  const OperatorInfo Op;

  PrefixExpression(Expression *E, OperatorInfo Op, TypeDenoter *Denoter,
                   bool IsConst)
      : Expression(EK_Prefix, Denoter, IsConst), E(E), Op(Op) {}

  Expression *getExpression() const { return E; }
  const OperatorInfo &getOperatorInfo() const { return Op; }

  static bool classof(const Expression *Expr) {
    return Expr->getKind() == EK_Prefix;
  }
};

template <Expression::ExpressionKind K, typename T>
class Literal : public Expression {
  T Value;

public:
  Literal(TypeDenoter *Denoter, const T &Value)
      : Expression(K, Denoter, true), Value(Value) {}

  static Literal<K, T> *create(TypeDenoter *Denoter, const T &Value) {
    return new Literal<K, T>(Denoter, Value);
  }

  T getValue() const { return Value; }

  static bool classof(const Expression *Expr) { return Expr->getKind() == K; }
};

using IntegerLiteral = Literal<Expression::EK_IntegerLiteral, llvm::APInt>;
using RealLiteral = Literal<Expression::EK_RealLiteral, llvm::APFloat>;
using StringLiteral = Literal<Expression::EK_StringLiteral, StringRef>;
using CharLiteral = Literal<Expression::EK_StringLiteral, unsigned>;
using BooleanLiteral = Literal<Expression::EK_BooleanLiteral, bool>;

class NilValue : public Expression {
public:
  NilValue(TypeDenoter *Denoter)
      : Expression(EK_Nil, Denoter, true) {}

  static bool classof(const Expression *Expr) {
    return Expr->getKind() == EK_Nil;
  }
};

class Selector {
public:
  enum SelectorKind {
    SK_Index,
    SK_Field,
    SK_Dereference,
  };

private:
  const SelectorKind Kind;

  // The type denoter decribes the base type.
  // E.g. the component type of an index selector
  TypeDenoter *TyDe;

protected:
  Selector(SelectorKind Kind, TypeDenoter *TyDe) : Kind(Kind), TyDe(TyDe) {}

public:
  SelectorKind getKind() const { return Kind; }
  TypeDenoter *getTypeDenoter() const { return TyDe; }
};

class IndexSelector : public Selector {
  Expression *Index;

public:
  IndexSelector(Expression *Index, TypeDenoter *TyDe)
      : Selector(SK_Index, TyDe), Index(Index) {}

  Expression *getIndex() const { return Index; }

  static bool classof(const Selector *Sel) {
    return Sel->getKind() == SK_Index;
  }
};

class FieldSelector : public Selector {
public:
  FieldSelector(TypeDenoter *TyDe) : Selector(SK_Field, TyDe) {}

  static bool classof(const Selector *Sel) {
    return Sel->getKind() == SK_Field;
  }
};

class DereferenceSelector : public Selector {
public:
  DereferenceSelector(TypeDenoter *TyDe) : Selector(SK_Dereference, TyDe) {}

  static bool classof(const Selector *Sel) {
    return Sel->getKind() == SK_Dereference;
  }
};

class Designator : public Expression {
  Declaration *Decl;
  SelectorList Selectors;

  // Synthesized attribute: Is expression a reference (denotes an address)?
  bool IsReference;

public:
  Designator(Declaration *Decl, const SelectorList &Selectors,
             TypeDenoter *Denoter, bool IsVariable, bool IsConst)
      : Expression(EK_Designator, Denoter, IsConst), Decl(Decl),
        Selectors(Selectors), IsReference(IsVariable) {}

  Designator(Declaration *Decl, TypeDenoter *Denoter, bool IsReference,
             bool IsConst)
      : Expression(EK_Designator, Denoter, IsConst), Decl(Decl),
        IsReference(IsReference) {}

  void addSelector(Selector *Selector) {
    Selectors.push_back(Selector);
    setDenoter(Selector->getTypeDenoter());
  }

  Declaration *getDecl() const { return Decl; }
  const SelectorList &getSelectorList() const { return Selectors; }

  // Returns true if this is a variable designator, e.g. the left side of an
  // assignment.
  bool isReference() const { return IsReference; }

  static bool classof(const Expression *Expr) {
    return Expr->getKind() == EK_Designator;
  }
};

class FunctionCall : public Expression {
  Designator *Desig;
  ActualParameterList ActualParameters;

public:
  FunctionCall(Designator *Desig, const ActualParameterList &ActualParameters,
               TypeDenoter *Denoter, bool IsConst)
      : Expression(EK_FunctionCall, Denoter, IsConst), Desig(Desig),
        ActualParameters(ActualParameters) {}

  Designator *getDesig() const { return Desig; }
  const ActualParameterList &getActualParameters() const {
    return ActualParameters;
  }

  static bool classof(const Expression *Expr) {
    return Expr->getKind() == EK_FunctionCall;
  }
};

class ValueConstructor : public Expression {
public:
  ValueConstructor(TypeDenoter *Denoter)
      : Expression(EK_ValueConstructor, Denoter, true) {}

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
  SMLoc Loc;

protected:
  Statement(StmtKind Kind, SMLoc Loc) : Kind(Kind), Loc(Loc) {}

public:
  SMLoc getLoc() const { return Loc; }

  StmtKind getKind() const { return Kind; }
};

class AssignmentStatement : public Statement {
  Designator *Left;
  Expression *Right;

public:
  AssignmentStatement(SMLoc Loc, Designator *Left, Expression *Right)
      : Statement(SK_Assignment, Loc), Left(Left), Right(Right) {}

  Designator *getDesignator() const {return Left; }
  Expression *getExpression() const { return Right; }

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_Assignment;
  }
};

class ProcedureCallStatement : public Statement {
  Designator *Proc;
  ActualParameterList ActualParameters;

public:
  ProcedureCallStatement(SMLoc Loc, Designator *Proc,
                         const ActualParameterList &ActualParameters)
      : Statement(SK_ProcedureCall, Loc), Proc(Proc),
        ActualParameters(ActualParameters) {}

  Designator *getProc() const { return Proc; }
  const ActualParameterList &getActualParameters() const {
    return ActualParameters;
  }

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_ProcedureCall;
  }
};

class IfStatement : public Statement {
  Expression *Cond;
  StatementList Stmts;

public:
  IfStatement(SMLoc Loc, Expression *Cond, StatementList &Stmts)
      : Statement(SK_If, Loc), Cond(Cond), Stmts(Stmts) {}

  Expression *getCond() const { return Cond; }
  const StatementList &getStmts() const { return Stmts; }

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_If;
  }
};

class CaseStatement : public Statement {
public:
  CaseStatement(SMLoc Loc) : Statement(SK_Case, Loc) {}

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_Case;
  }
};

class WhileStatement : public Statement {
  Expression *Cond;
  StatementList Stmts;

public:
  WhileStatement(SMLoc Loc, Expression *Cond, StatementList &Stmts)
      : Statement(SK_While, Loc), Cond(Cond), Stmts(Stmts) {}

  Expression *getCond() const { return Cond; }
  const StatementList &getStmts() const { return Stmts; }

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_While;
  }
};

class RepeatStatement : public Statement {
  Expression *Cond;
  StatementList Stmts;

public:
  RepeatStatement(SMLoc Loc, Expression *Cond, StatementList &Stmts)
      : Statement(SK_Repeat, Loc), Cond(Cond), Stmts(Stmts) {}

  Expression *getCond() const { return Cond; }
  const StatementList &getStmts() const { return Stmts; }

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_Repeat;
  }
};

class ForStatement : public Statement {
  Variable *ControlVariable;
  Expression *InitialValue;
  Expression *FinalValue;
  Expression *StepSize;
  StatementList ForStmts;

public:
  ForStatement(SMLoc Loc, Variable *ControlVariable, Expression *InitialValue,
               Expression *FinalValue, Expression *StepSize,
               const StatementList &ForStmts)
      : Statement(SK_For, Loc), ControlVariable(ControlVariable),
        InitialValue(InitialValue), FinalValue(FinalValue), StepSize(StepSize),
        ForStmts(ForStmts) {}

  Variable *getControlVariable() const { return ControlVariable; }
  Expression *getInitialValue() const { return InitialValue; }
  Expression *getFinalValue() const { return FinalValue; }
  Expression *getStepSize() const { return StepSize; }
  const StatementList &getForStmts() const { return ForStmts; }

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_For;
  }
};

class LoopStatement : public Statement {
  StatementList Stmts;

public:
  LoopStatement(SMLoc Loc, StatementList &Stmts)
      : Statement(SK_Loop, Loc), Stmts(Stmts) {}

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_Loop;
  }
};

class WithStatement : public Statement {
public:
  WithStatement(SMLoc Loc) : Statement(SK_With, Loc) {}

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_With;
  }
};

class ExitStatement : public Statement {
public:
  ExitStatement(SMLoc Loc) : Statement(SK_Exit, Loc) {}

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_Exit;
  }
};

class ReturnStatement : public Statement {
  Expression *RetVal;

public:
  ReturnStatement(SMLoc Loc, Expression *RetVal) : Statement(SK_Return, Loc), RetVal(RetVal) {}

  Expression *getRetVal() const { return RetVal; }

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_Return;
  }
};

class RetryStatement : public Statement {
public:
  RetryStatement(SMLoc Loc) : Statement(SK_Retry, Loc) {}

  static bool classof(const Statement *Stmt) {
    return Stmt->getKind() == SK_Retry;
  }
};

} // namespace m2lang
#endif

#endif