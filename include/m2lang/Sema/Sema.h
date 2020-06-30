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

class FormalType {
  Declaration *Decl;
  unsigned OpenArrayLevel;

public:
  FormalType() = default;
  FormalType(Declaration *Decl, unsigned OpenArrayLevel)
      : Decl(Decl), OpenArrayLevel(OpenArrayLevel) {}

  Declaration *getDecl() const { return Decl; }
  unsigned getOpenArrayLevel() const { return OpenArrayLevel; }
};

using IdentifierList = llvm::SmallVector<Identifier, 8>;
using VariableIdentifierList =
    llvm::SmallVector<std::pair<Identifier, Expression *>, 8>;

class Sema final {
  DiagnosticsEngine &Diags;

  Scope *CurrentScope;
  Declaration *CurrentDecl;

  // Declarations in the global scope. Possible move to Context class.
  TypeDenoter *BitSetTypeDenoter;
  TypeDenoter *CharTypeDenoter;
  TypeDenoter *IntegerTypeDenoter;
  TypeDenoter *CardinalTypeDenoter;
  TypeDenoter *BooleanTypeDenoter;
  TypeDenoter *RealTypeDenoter;
  TypeDenoter *LongRealTypeDenoter;
  TypeDenoter *ComplexTypeDenoter;
  TypeDenoter *LongComplexTypeDenoter;
  Type *BitSetType;
  Type *CharType;
  Type *IntegerType;
  Type *CardinalType;
  Type *BooleanType;
  Type *RealType;
  Type *LongRealType;
  Type *ComplexType;
  Type *LongComplexType;
  BooleanLiteral *TrueLiteral;
  BooleanLiteral *FalseLiteral;
  Constant *TrueConst;
  Constant *FalseConst;

  friend class EnterDeclScope;
  void enterScope(Declaration *Decl);
  void leaveScope();

public:
  Sema(DiagnosticsEngine &Diags)
      : Diags(Diags), CurrentScope(nullptr), CurrentDecl(nullptr) {
    initialize();
  }

  void initialize();

  bool isUndeclared(StringRef Name);
  bool isModule(StringRef Name);
  bool isClass(StringRef Name);

  // Declarations
  template <typename T>
  T *actOnCompilationModule(Identifier ModuleName, bool IsUnsafeGuarded) {
    return T::create(CurrentDecl, ModuleName.getLoc(), ModuleName.getName(),
                     IsUnsafeGuarded);
  }

  template <typename T> T *actOnCompilationModule(Identifier ModuleName) {
    return T::create(CurrentDecl, ModuleName.getLoc(), ModuleName.getName());
  }

  void actOnImplementationModule(ImplementationModule *Mod,
                                 Identifier ModuleName, Expression *Protection,
                                 DeclarationList Decls, Block InitBlk,
                                 Block FinalBlk, bool IsProgramModule);
  void actOnDefinitionModule(DefinitionModule *Mod, Identifier ModuleName,
                             DeclarationList Decls);
  void actOnRefiningDefinitionModule(RefiningDefinitionModule *Mod,
                                     Identifier ModuleName,
                                     ActualParameterList ActualModulParams);
  void actOnRefiningImplementationModule(RefiningImplementationModule *Mod,
                                         Identifier ModuleName,
                                         ActualParameterList ActualModulParams);
  LocalModule *actOnLocalModule(Identifier ModuleName);
  Procedure *actOnProcedure(Identifier ProcName);
  void actOnProcedure(DeclarationList &Decls, Procedure *Proc,
                      Identifier ProcName, FormalParameterList &Params,
                      Declaration *ResultType, const DeclarationList &ProcDecls,
                      Block Body, bool IsFunction);
  void actOnForwardProcedure(DeclarationList &Decls, Procedure *Proc);
  void actOnConstant(DeclarationList &Decls, Identifier Name, Expression *Expr);
  void actOnType(DeclarationList &Decls, Identifier TypeName,
                 TypeDenoter *TyDen);
  void actOnVariable(DeclarationList &Decls, VariableIdentifierList &VarIdList,
                     TypeDenoter *TyDen);
  void actOnActualParameter(ActualParameterList &Params, Expression *Expr);
  void actOnFormalParameter(FormalParameterList &Params,
                            const IdentifierList &IdList, bool IsVar,
                            const FormalType &FTy);

  // Qualified identifier
  Declaration *actOnModuleIdentifier(Declaration *ModDecl, Identifier Name);
  Declaration *actOnClassIdentifier(Declaration *ModDecl, Identifier Name);
  Declaration *actOnQualifiedIdentifier(Declaration *Decl, Identifier Name);
  Type *actOnTypeIdentifier(Declaration *TypeDecl);

  // Types
  NamedType *actOnNamedType(SMLoc Loc, Declaration *Decl);
  RecordType *actOnRecordType();
  ArrayType *actOnArrayType(TypeDenoter *ComponentType,
                            const TypeDenoterList &IndexList);
  ProcedureType *actOnProcedureType(Type *ResultType);
  PointerType *actOnPointerType(TypeDenoter *TyDen);
  PointerType *actOnPointerType(const StringRef &Name);
  SubrangeType *actOnSubrangeType(Declaration *Decl, Expression *From,
                                  Expression *To);
  EnumerationType *actOnEnumerationType();
  SetType *actOnSetType(TypeDenoter *BaseType, bool IsPacked);

  // Statements
  void actOnAssignmentStmt(StatementList &Stmts, Designator *Left,
                           Expression *Right);
  void actOnProcedureCallStmt(StatementList &Stmts, Designator *Proc,
                              const ActualParameterList &ActualParameters);
  void actOnIfStmt(StatementList &Stmts, Expression *Cond);
  void actOnCaseStmt(StatementList &Stmts);
  void actOnWhileStmt(StatementList &Stmts, SMLoc Loc, Expression *Cond,
                      StatementList &WhileStmts);
  void actOnRepeatStmt(StatementList &Stmts, SMLoc Loc, Expression *Cond,
                       StatementList &RepeatStmts);
  void actOnLoopStmt(StatementList &Stmts, SMLoc Loc, StatementList &LoopStmts);
  void actOnForStmt(StatementList &Stmts, SMLoc Loc, Identifier ControlVariable,
                    Expression *InitialValue, Expression *FinalValue,
                    Expression *StepSize, const StatementList &ForStmts);
  void actOnWithStmt(StatementList &Stmts, Designator *Desig,
                     StatementList &WithStmts);
  void actOnExitStmt(StatementList &Stmts, SMLoc Loc);
  void actOnReturnStmt(StatementList &Stmts, Expression *E);
  void actOnRetryStmt(StatementList &Stmts, SMLoc Loc);

  // Expressions
  Expression *actOnExpression(Expression *Left, Expression *Right,
                              const OperatorInfo &Op);
  Expression *actOnSimpleExpression(Expression *Left, Expression *Right,
                                    const OperatorInfo &Op);
  Expression *actOnTerm(Expression *Left, Expression *Right,
                        const OperatorInfo &Op);
  Expression *actOnPrefixOperator(Expression *E, const OperatorInfo &Op);
  Expression *actOnFactor(Expression *E, const OperatorInfo &Op);
  Expression *actOnIntegerLiteral(SMLoc Loc, StringRef LiteralData);
  Expression *actOnRealLiteral(SMLoc Loc, StringRef LiteralData);
  Expression *actOnStringLiteral(SMLoc Loc, StringRef LiteralData);
  Expression *actOnCharLiteral(SMLoc Loc, StringRef LiteralData);
  Designator *actOnDesignator(Declaration *QualId,
                              const SelectorList &Selectors);
  Expression *actOnFunctionCall(Expression *DesignatorExpr,
                                const ActualParameterList &ActualParameters);
  Expression *
  actOnValueConstructor(Declaration *QualId /*, ConstructorValues */);
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