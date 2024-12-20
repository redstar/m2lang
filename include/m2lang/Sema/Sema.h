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
#include "m2lang/AST/Scope.h"

namespace m2lang {
class ASTContext;
class DiagnosticsEngine;

class Sema final {
  ASTContext &ASTCtx;
  DiagnosticsEngine &Diags;

  // The outermost scope, holding the global (system-wide) declarations.
  Scope GlobalScope;

  // The scope holding the pervasive types, procedures, and functions.
  Scope PervasiveScope;

  // The current scope.
  Scope *CurrentScope;

  // The recent declaration creating a lexical scope.
  // Can be nullptr, which denotes the outermost lexical scope.
  ScopedDeclaration *CurrentDecl;

  // Declarations in the global scope. Possible move to Context class.
  BooleanLiteral *TrueLiteral;
  BooleanLiteral *FalseLiteral;
  Constant *TrueConst;
  Constant *FalseConst;

  // List of unresolved pointers. The list is ordered by the lexical scope.
  llvm::SmallVector<std::tuple<ScopedDeclaration *, PointerType *, Identifier>, 4>
      UnresolvedPointer;

  friend class EnterDeclScope;
  void enterScope(ScopedDeclaration *Decl);
  void leaveScope();
  bool addToScope(Scope *Scope, Declaration *Decl);
  bool addToCurrentScope(Declaration *Decl);

  // List of EXIT statements.
  friend class ExitStmtHandler;
  using ExitStmtList = llvm::SmallVector<ExitStatement *, 0>;
  ExitStmtList ExitStmts;
  bool IsInsideLoop = false;
  bool addToExitStmtList(ExitStatement *Exit);

  bool isWholeNumberType(PervasiveType *T) {
    switch (T->getTypeKind()) {
    case pervasive::WholeNumber:
#define WHOLENUMBER_TYPE(Id, Name) case pervasive::Id:
#include "m2lang/AST/PervasiveTypes.def"
      return true;
    default:
      return false;
    }
  }

  bool isWholeNumberType(TypeDenoter *T) {
    if (auto *Pervasive = llvm::dyn_cast<PervasiveType>(T))
      return isWholeNumberType(Pervasive);
    return false;
  }

  bool isRealType(PervasiveType *T) {
    switch (T->getTypeKind()) {
    case pervasive::RealNumber:
#define FLOATING_TYPE(Id, Name) case pervasive::Id:
#include "m2lang/AST/PervasiveTypes.def"
      return true;
    default:
      return false;
    }
  }

  bool isRealType(TypeDenoter *T) {
    if (auto *Pervasive = llvm::dyn_cast<PervasiveType>(T))
      return isRealType(Pervasive);
    return false;
  }

  bool isComplexType(PervasiveType *T) {
    switch (T->getTypeKind()) {
    case pervasive::ComplexNumber:
#define COMPLEX_TYPE(Id, Name) case pervasive::Id:
#include "m2lang/AST/PervasiveTypes.def"
      return true;
    default:
      return false;
    }
  }

  bool isComplexType(TypeDenoter *T) {
    if (auto *Pervasive = llvm::dyn_cast<PervasiveType>(T))
      return isComplexType(Pervasive);
    return false;
  }

  bool isOrdinalType(PervasiveType *T) {
    switch (T->getTypeKind()) {
    case pervasive::WholeNumber:
#define ORDINAL_TYPE(Id, Name) case pervasive::Id:
#include "m2lang/AST/PervasiveTypes.def"
      return true;
    default:
      return false;
    }
  }

  bool isOrdinalType(TypeDenoter *T) {
    if (auto *Pervasive = llvm::dyn_cast<PervasiveType>(T))
      return isWholeNumberType(Pervasive);
    if (llvm::isa<EnumerationType>(T) || llvm::isa<SubrangeType>(T))
      return true;
    return false;
  }

  TypeDenoter *exprCompatible(TypeDenoter *Left, TypeDenoter *Right);
  bool assignCompatible(TypeDenoter *Tv, TypeDenoter *Te);

  void handleUnresolvedPointer(ScopedDeclaration *DeclScope);

public:
  Sema(ASTContext &ASTCtx, DiagnosticsEngine &Diags)
      : ASTCtx(ASTCtx), Diags(Diags), CurrentScope(nullptr),
        CurrentDecl(nullptr) {
    initialize();
  }

  void initialize();

  bool isUndeclared(StringRef Name);
  bool isModule(StringRef Name);
  bool isClass(StringRef Name);

  // Declarations
  template <typename T>
  T *actOnCompilationModule(Identifier ModuleName, bool IsUnsafeGuarded) {
    Scope *ModuleScope = new Scope(PervasiveScope);
    return new (ASTCtx) T(CurrentDecl, ModuleName.getLoc(),
                          ModuleName.getName(), ModuleScope, IsUnsafeGuarded);
  }

  template <typename T> T *actOnCompilationModule(Identifier ModuleName) {
    Scope *ModuleScope = new Scope(PervasiveScope);
    return new (ASTCtx)
        T(CurrentDecl, ModuleName.getLoc(), ModuleName.getName(), ModuleScope);
  }

  void actOnImplementationModule(ImplementationModule *Mod,
                                 Identifier ModuleName, Expression *Protection,
                                 DeclarationList &Decls, Block &InitBlk,
                                 Block &FinalBlk, bool IsProgramModule);
  void actOnDefinitionModule(DefinitionModule *Mod, Identifier ModuleName,
                             DeclarationList &Decls);
  void actOnRefiningDefinitionModule(RefiningDefinitionModule *Mod,
                                     Identifier ModuleName,
                                     ActualParameterList ActualModulParams);
  void actOnRefiningImplementationModule(RefiningImplementationModule *Mod,
                                         Identifier ModuleName,
                                         ActualParameterList ActualModulParams);
  LocalModule *actOnLocalModule(Identifier ModuleName);
  void actOnLocalModule(LocalModule *Mod, Identifier ModuleName,
                        Expression *Protection, DeclarationList &Decls,
                        Block &InitBlk, Block &FinalBlk);
  void unique(IdentifierList &IdList, unsigned Error, unsigned Note);
  Scope *getScopeOfModule(Declaration *Decl);
  void actOnSimpleImport(ImportItemList &Imports, IdentifierList &IdList);
  void actOnUnqualifiedImport(ImportItemList &Imports, Identifier ModuleName,
                              IdentifierList &IdList);
  Procedure *actOnProcedure(Identifier ProcName);
  void actOnProcedureHeading(DeclarationList &Decls, Procedure *Proc,
                             FormalParameterList &Params, Type *ResultType);
  void actOnProcedure(Procedure *Proc, Identifier ProcName,
                      const DeclarationList &ProcDecls, Block Body,
                      bool IsFunction);
  void actOnForwardProcedure(DeclarationList &Decls, Procedure *Proc);
  void actOnConstant(DeclarationList &Decls, Identifier Name, Expression *Expr);
  void actOnType(DeclarationList &Decls, Identifier TypeName,
                 TypeDenoter *TyDen);
  void actOnVariable(DeclarationList &Decls, VariableIdentifierList &VarIdList,
                     TypeDenoter *TyDen);
  void actOnActualParameter(ActualParameterList &Params, Expression *Expr);
  void actOnFormalParameter(FormalParameterList &Params,
                            const IdentifierList &IdList,
                            bool IsCallByReference, TypeDenoter *FTy);
  void actOnExportList(LocalModule *LM, IdentifierList &IdList,
                       bool IsQualified);
  void extendScopeOfDecl(Scope *Sc, Declaration *Decl);
  void actOnBlockBegin();
  void actOnModuleBlockEnd();

  // Qualified identifier
  Declaration *actOnQualifiedIdentifier(Declaration *Decl, Identifier Name);
  Type *actOnTypeIdentifier(Declaration *TypeDecl);

  // Types
  TypeDenoter *actOnTypeIdentifier(SMLoc Loc, Declaration *Decl);
  TypeDenoter *actOnOrdinalTypeIdentifier(Declaration *Decl);
  RecordType *actOnRecordType(StringIndexMap &FieldMap, RecordFieldList &Fields);
  void actOnFixedFields(StringIndexMap &FieldMap, RecordFieldList &Fields,
                        const IdentifierList &IdList, TypeDenoter *TyDe);
  ArrayType *actOnArrayType(TypeDenoter *ComponentType,
                            const TypeDenoterList &IndexTypeList);
  void actOnFormalParameterType(FormalParameterTypeList &ParameterTypes,
                                SMLoc Loc, bool IsCallByReference,
                                TypeDenoter *TyDe);
  ProcedureType *actOnProcedureType(Type *ResultType,
                                    FormalParameterTypeList &ParameterTypes);
  TypeDenoter *actOnFormalType(Type *Ty, unsigned OpenArrayLevel);
  PointerType *actOnPointerType(TypeDenoter *TyDen);
  PointerType *actOnPointerType(Identifier Name);
  SubrangeType *actOnSubrangeType(Declaration *Decl, Expression *From,
                                  Expression *To);
  EnumerationType *actOnEnumerationType(const IdentifierList &IdList);
  SetType *actOnSetType(TypeDenoter *BaseType, bool IsPacked);

  // Statements
  void actOnAssignmentStmt(StatementList &Stmts, SMLoc Loc, Designator *Left,
                           Expression *Right);
  void actOnProcedureCallStmt(StatementList &Stmts, SMLoc Loc, Designator *Proc,
                              const ActualParameterList &ActualParameters);
  void actOnIfStmt(StatementList &Stmts, GuardedStatementList &GuardedStmts,
                   StatementList &ElseStmts);
  void actOnGuardedStmt(GuardedStatementList &GuardedStmts, SMLoc Loc,
                        Expression *Cond, StatementList &Stmts);
  void actOnCaseStmt(StatementList &Stmts, SMLoc Loc);
  void actOnWhileStmt(StatementList &Stmts, SMLoc Loc, Expression *Cond,
                      StatementList &WhileStmts);
  void actOnRepeatStmt(StatementList &Stmts, SMLoc Loc, Expression *Cond,
                       StatementList &RepeatStmts);
  void actOnLoopStmt(StatementList &Stmts, SMLoc Loc, StatementList &LoopStmts);
  void actOnForStmt(StatementList &Stmts, SMLoc Loc, Identifier ControlVariable,
                    Expression *InitialValue, Expression *FinalValue,
                    Expression *StepSize, const StatementList &ForStmts);
  void actOnWithStmt(StatementList &Stmts, SMLoc Loc, Designator *Desig,
                     StatementList &WithStmts);
  void actOnExitStmt(StatementList &Stmts, SMLoc Loc);
  void actOnReturnStmt(StatementList &Stmts, SMLoc Loc, Expression *E);
  void actOnRetryStmt(StatementList &Stmts, SMLoc Loc);

  // Expressions
  Expression *actOnExpression(Expression *Left, Expression *Right,
                              const OperatorInfo &Op);
  Expression *actOnSimpleExpression(Expression *Left, Expression *Right,
                                    const OperatorInfo &Op);
  Expression *actOnTerm(Expression *Left, Expression *Right,
                        const OperatorInfo &Op);
  Expression *actOnPrefixOperator(Expression *E, const OperatorInfo &Op);
  Expression *actOnNot(Expression *E, const OperatorInfo &Op);
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

  Expression *actOnOrdinalExpression(SMLoc Loc, Expression *E);

  // Selectors.
  void actOnIndexSelector(SMLoc Loc, SelectorList &Selectors, Expression *E);
  void actOnFieldSelector(SelectorList &Selectors, Identifier Field);
  void actOnDereferenceSelector(SMLoc Loc, SelectorList &Selectors);
};

class EnterDeclScope {
  Sema &Semantics;

public:
  EnterDeclScope(Sema &Semantics, ScopedDeclaration *Decl)
      : Semantics(Semantics) {
    Semantics.enterScope(Decl);
  }
  ~EnterDeclScope() { Semantics.leaveScope(); }
};

class ExitStmtHandler {
  Sema &Semantics;
  Sema::ExitStmtList Exits;
  bool IsInsideLoop;

public:
  ExitStmtHandler(Sema &Semantics) : Semantics(Semantics) {
    std::swap(Semantics.ExitStmts, Exits);
    IsInsideLoop = Semantics.IsInsideLoop;
    Semantics.IsInsideLoop = true;
  }
  ~ExitStmtHandler() {
    std::swap(Semantics.ExitStmts, Exits);
    Semantics.IsInsideLoop = IsInsideLoop;
  }
};
} // namespace m2lang

#endif