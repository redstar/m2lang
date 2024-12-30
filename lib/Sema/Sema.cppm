
//===--- Sema.cppm - M2 Language Family Semantic Analyzer -----------------===//
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

module;

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <tuple>

export module m2lang.sema;

import m2lang.ast;
import m2lang.basic;

export namespace m2lang {

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
  llvm::SmallVector<std::tuple<ScopedDeclaration *, PointerType *, Identifier>,
                    4>
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

  bool isUndeclared(llvm::StringRef Name);
  bool isModule(llvm::StringRef Name);
  bool isClass(llvm::StringRef Name);

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
  TypeDenoter *actOnTypeIdentifier(llvm::SMLoc Loc, Declaration *Decl);
  TypeDenoter *actOnOrdinalTypeIdentifier(Declaration *Decl);
  RecordType *actOnRecordType(StringIndexMap &FieldMap,
                              RecordFieldList &Fields);
  void actOnFixedFields(StringIndexMap &FieldMap, RecordFieldList &Fields,
                        const IdentifierList &IdList, TypeDenoter *TyDe);
  ArrayType *actOnArrayType(TypeDenoter *ComponentType,
                            const TypeDenoterList &IndexTypeList);
  void actOnFormalParameterType(FormalParameterTypeList &ParameterTypes,
                                llvm::SMLoc Loc, bool IsCallByReference,
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
  void actOnAssignmentStmt(StatementList &Stmts, llvm::SMLoc Loc,
                           Designator *Left, Expression *Right);
  void actOnProcedureCallStmt(StatementList &Stmts, llvm::SMLoc Loc,
                              Designator *Proc,
                              const ActualParameterList &ActualParameters);
  void actOnIfStmt(StatementList &Stmts, GuardedStatementList &GuardedStmts,
                   StatementList &ElseStmts);
  void actOnGuardedStmt(GuardedStatementList &GuardedStmts, llvm::SMLoc Loc,
                        Expression *Cond, StatementList &Stmts);
  void actOnCaseStmt(StatementList &Stmts, llvm::SMLoc Loc);
  void actOnWhileStmt(StatementList &Stmts, llvm::SMLoc Loc, Expression *Cond,
                      StatementList &WhileStmts);
  void actOnRepeatStmt(StatementList &Stmts, llvm::SMLoc Loc, Expression *Cond,
                       StatementList &RepeatStmts);
  void actOnLoopStmt(StatementList &Stmts, llvm::SMLoc Loc,
                     StatementList &LoopStmts);
  void actOnForStmt(StatementList &Stmts, llvm::SMLoc Loc,
                    Identifier ControlVariable, Expression *InitialValue,
                    Expression *FinalValue, Expression *StepSize,
                    const StatementList &ForStmts);
  void actOnWithStmt(StatementList &Stmts, llvm::SMLoc Loc, Designator *Desig,
                     StatementList &WithStmts);
  void actOnExitStmt(StatementList &Stmts, llvm::SMLoc Loc);
  void actOnReturnStmt(StatementList &Stmts, llvm::SMLoc Loc, Expression *E);
  void actOnRetryStmt(StatementList &Stmts, llvm::SMLoc Loc);

  // Expressions
  Expression *actOnExpression(Expression *Left, Expression *Right,
                              const OperatorInfo &Op);
  Expression *actOnSimpleExpression(Expression *Left, Expression *Right,
                                    const OperatorInfo &Op);
  Expression *actOnTerm(Expression *Left, Expression *Right,
                        const OperatorInfo &Op);
  Expression *actOnPrefixOperator(Expression *E, const OperatorInfo &Op);
  Expression *actOnNot(Expression *E, const OperatorInfo &Op);
  Expression *actOnIntegerLiteral(llvm::SMLoc Loc, llvm::StringRef LiteralData);
  Expression *actOnRealLiteral(llvm::SMLoc Loc, llvm::StringRef LiteralData);
  Expression *actOnStringLiteral(llvm::SMLoc Loc, llvm::StringRef LiteralData);
  Expression *actOnCharLiteral(llvm::SMLoc Loc, llvm::StringRef LiteralData);
  Designator *actOnDesignator(Declaration *QualId,
                              const SelectorList &Selectors);
  Expression *actOnFunctionCall(Expression *DesignatorExpr,
                                const ActualParameterList &ActualParameters);
  Expression *
  actOnValueConstructor(Declaration *QualId /*, ConstructorValues */);

  Expression *actOnOrdinalExpression(llvm::SMLoc Loc, Expression *E);

  // Selectors.
  void actOnIndexSelector(llvm::SMLoc Loc, SelectorList &Selectors,
                          Expression *E);
  void actOnFieldSelector(SelectorList &Selectors, Identifier Field);
  void actOnDereferenceSelector(llvm::SMLoc Loc, SelectorList &Selectors);
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

using namespace m2lang;

void Sema::initialize() {
  CurrentScope = &PervasiveScope;
  CurrentDecl = nullptr;
#define PERVASIVE_TYPE(Id, Name)                                               \
  CurrentScope->insert(                                                        \
      new (ASTCtx) Type(CurrentDecl, llvm::SMLoc(), Name, ASTCtx.Id##TyDe));
#include "m2lang/AST/PervasiveTypes.def"
#define PROCEDURE(Name)                                                        \
  CurrentScope->insert(new (ASTCtx) PervasiveFunction(                         \
      CurrentDecl, llvm::SMLoc(), #Name, pervasive::Proc_##Name));
#define FUNCTION(Name)                                                         \
  CurrentScope->insert(new (ASTCtx) PervasiveFunction(                         \
      CurrentDecl, llvm::SMLoc(), #Name, pervasive::Func_##Name));
#include "m2lang/AST/PervasiveFunctions.def"
  Constant *Nil =
      new (ASTCtx) Constant(CurrentDecl, llvm::SMLoc(), "NIL", ASTCtx.NilTyDe,
                            new (ASTCtx) NilValue(ASTCtx.NilTyDe));
  TrueLiteral = new (ASTCtx) BooleanLiteral(ASTCtx.BooleanTyDe, true);
  FalseLiteral = new (ASTCtx) BooleanLiteral(ASTCtx.BooleanTyDe, false);
  TrueConst = new (ASTCtx) Constant(CurrentDecl, llvm::SMLoc(), "TRUE",
                                    ASTCtx.BooleanTyDe, TrueLiteral);
  FalseConst = new (ASTCtx) Constant(CurrentDecl, llvm::SMLoc(), "FALSE",
                                     ASTCtx.BooleanTyDe, FalseLiteral);
  CurrentScope->insert(Nil);
  CurrentScope->insert(TrueConst);
  CurrentScope->insert(FalseConst);

  // Standard module SYS.
  DefinitionModule *SYS = new (ASTCtx) DefinitionModule(
      CurrentDecl, llvm::SMLoc(), "SYS", new Scope(PervasiveScope),
      /*UnsafeGuarded=*/false);
  // TODO Need OpaqueTypeDenoter.
  TypeDenoter *LocTyDe = nullptr;
  SYS->getScope()->insert(new (ASTCtx)
                              Type(CurrentDecl, llvm::SMLoc(), "LOC", LocTyDe));
  PointerType *AddrTyDe = new (ASTCtx) PointerType();
  AddrTyDe->setTyDen(LocTyDe);
  SYS->getScope()->insert(
      new (ASTCtx) Type(CurrentDecl, llvm::SMLoc(), "ADDR", AddrTyDe));
  GlobalScope.insert(SYS);
}

void Sema::enterScope(ScopedDeclaration *Decl) {
  if (CurrentDecl)
    handleUnresolvedPointer(CurrentDecl);
  CurrentScope = Decl->getScope();
  CurrentDecl = Decl;
  llvm::outs() << "Enter scope " << CurrentScope << "\n";
}

void Sema::leaveScope() {
  assert(CurrentScope && "Can't leave non-existing scope");
  llvm::outs() << "Exit scope " << CurrentScope << "\n";
  CurrentDecl = CurrentDecl->getEnclosingDecl();
  if (CurrentDecl)
    CurrentScope = CurrentDecl->getScope();
  else
    CurrentScope = &PervasiveScope;
}

bool Sema::addToScope(Scope *Scope, Declaration *Decl) {
  if (!Scope->insert(Decl)) {
    Declaration *OtherDecl = Scope->lookup(Decl->getName(), false);
    Diags.report(Decl->getLoc(), diag::err_symbol_already_declared)
        << Decl->getName();
    Diags.report(OtherDecl->getLoc(), diag::note_symbol_already_declared)
        << OtherDecl->getName();
    return false;
  }
  return true;
}

bool Sema::addToCurrentScope(Declaration *Decl) {
  return addToScope(CurrentScope, Decl);
}

bool Sema::addToExitStmtList(ExitStatement *Exit) {
  if (IsInsideLoop) {
    ExitStmts.push_back(Exit);
    return true;
  }
  Diags.report(Exit->getLoc(), diag::err_exit_not_inside_loop);
  return false;
}

TypeDenoter *Sema::exprCompatible(TypeDenoter *Left, TypeDenoter *Right) {
  // ISO 10514:1994, Clause 6.4.1
  // Types are identical.
  if (Left == Right)
    return Left;
  // Whole number types.
  // FIXME: Handle sub ranges.
  if (isWholeNumberType(Left) && Right == ASTCtx.WholeNumberTyDe)
    return Left;
  if (Left == ASTCtx.WholeNumberTyDe && isWholeNumberType(Right))
    return Right;
  // Real number types.
  if (isRealType(Left) && Right == ASTCtx.RealNumberTyDe)
    return Left;
  if (Left == ASTCtx.RealNumberTyDe && isRealType(Right))
    return Right;
  // Complex number types.
  if (isComplexType(Left) && Right == ASTCtx.ComplexNumberTyDe)
    return Left;
  if (Left == ASTCtx.ComplexNumberTyDe && isComplexType(Right))
    return Right;
  return nullptr;
}

bool Sema::assignCompatible(TypeDenoter *Tv, TypeDenoter *Te) {
  // ISO 10514:1994, Clause 6.4.2
  // Tv is identical to the type Te and that type is not a formal type having an
  // open array structure.
  if (Tv == Te && !llvm::isa<OpenArrayFormalType>(Tv))
    return true;
  // FIXME: Tv is subrange of Te
  // FIXME: Tv is the unsigned type or a subrange of the unsigned type and Te is
  // the signed type or is the Z-type.
  if (Tv == ASTCtx.CardinalTyDe &&
      (Te == ASTCtx.IntegerTyDe || Te == ASTCtx.WholeNumberTyDe))
    return true;
  // FIXME: Tv is the signed type or a subrange of the signed type and Te is the
  // unsigned type or is the Z-type.
  if (Tv == ASTCtx.IntegerTyDe &&
      (Te == ASTCtx.CardinalTyDe || Te == ASTCtx.WholeNumberTyDe))
    return true;
  // Tv is a real number type and Te is the R-type.
  if (isRealType(Tv) && Te == ASTCtx.RealNumberTyDe)
    return true;
  // Tv is a complex number type and Te is the C -type.
  if (isComplexType(Tv) && Te == ASTCtx.ComplexNumberTyDe)
    return true;
  // Tv is a pointer type and Te is the nil type.
  if (llvm::isa<PointerType>(Tv) && Te == ASTCtx.NilTyDe)
    return true;
  // FIXME: Tv is a proper procedure type or a function procedure type, and the
  // expression designates a procedure value, or procedure constant value, of a
  // procedure that has the same structure as the procedure type Tv , and that
  // has been declared at declaration level 0.
  return false;
}

bool Sema::isUndeclared(llvm::StringRef Name) {
  return nullptr == CurrentScope->lookup(Name);
}

bool Sema::isModule(llvm::StringRef Name) {
  llvm::outs() << "Sema::isModule: " << Name << "\n";
  Declaration *Decl = CurrentScope->lookup(Name);
  return llvm::isa_and_nonnull<CompilationModule>(Decl) ||
         llvm::isa_and_nonnull<LocalModule>(Decl);
}

bool Sema::isClass(llvm::StringRef Name) {
  llvm::outs() << "Sema::isClass: " << Name << "\n";
  Declaration *Decl = CurrentScope->lookup(Name);
  return llvm::isa_and_nonnull<Class>(Decl);
}

void Sema::handleUnresolvedPointer(ScopedDeclaration *DeclScope) {
  while (!UnresolvedPointer.empty()) {
    auto &[Sc, PtrTy, Id] = UnresolvedPointer.back();
    if (Sc == DeclScope) {
      Declaration *D = DeclScope->getScope()->lookup(Id.getName(), false);
      if (Type *Ty = llvm::dyn_cast_or_null<Type>(D))
        PtrTy->setTyDen(Ty->getTypeDenoter());
      else {
        if (Ty)
          Diags.report(Id.getLoc(), diag::err_type_expected) << Id.getName();
        else
          Diags.report(Id.getLoc(), diag::err_undeclared_type) << Id.getName();
      }
      UnresolvedPointer.pop_back();
    } else
      break;
  }
}

void Sema::actOnImplementationModule(ImplementationModule *Mod,
                                     Identifier ModuleName,
                                     Expression *Protection,
                                     DeclarationList &Decls, Block &InitBlk,
                                     Block &FinalBlk, bool IsProgramModule) {
  if (Mod->getName() != ModuleName.getName()) {
    Diags.report(ModuleName.getLoc(), diag::err_module_identifier_not_equal)
        << Mod->getName() << ModuleName.getName();
  }
  Mod->setProtection(Protection);
  Mod->setDecls(Decls);
  Mod->setInitBlk(InitBlk);
  Mod->setFinalBlk(FinalBlk);
  Mod->setProgramModule(IsProgramModule);
}

void Sema::actOnDefinitionModule(DefinitionModule *Mod, Identifier ModuleName,
                                 DeclarationList &Decls) {
  if (Mod->getName() != ModuleName.getName()) {
    Diags.report(ModuleName.getLoc(), diag::err_module_identifier_not_equal)
        << Mod->getName() << ModuleName.getName();
  }
  Mod->setDecls(Decls);
}

void Sema::actOnRefiningDefinitionModule(
    RefiningDefinitionModule *Mod, Identifier ModuleName,
    ActualParameterList ActualModulParams) {
  if (Mod->getName() != ModuleName.getName()) {
    Diags.report(ModuleName.getLoc(), diag::err_module_identifier_not_equal)
        << Mod->getName() << ModuleName.getName();
  }
  Mod->setActualModulParams(ActualModulParams);
}

void Sema::actOnRefiningImplementationModule(
    RefiningImplementationModule *Mod, Identifier ModuleName,
    ActualParameterList ActualModulParams) {
  if (Mod->getName() != ModuleName.getName()) {
    Diags.report(ModuleName.getLoc(), diag::err_module_identifier_not_equal)
        << Mod->getName() << ModuleName.getName();
  }
  Mod->setActualModulParams(ActualModulParams);
}

LocalModule *Sema::actOnLocalModule(Identifier ModuleName) {
  llvm::outs() << "actOnLocalModule\n";
  Scope *ModuleScope = new Scope(PervasiveScope);
  LocalModule *Mod = new (ASTCtx) LocalModule(
      CurrentDecl, ModuleName.getLoc(), ModuleName.getName(), ModuleScope);
  Mod->setExportScope(new Scope());
  addToCurrentScope(Mod);
  return Mod;
}

void Sema::actOnLocalModule(LocalModule *Mod, Identifier ModuleName,
                            Expression *Protection, DeclarationList &Decls,
                            Block &InitBlk, Block &FinalBlk) {
  if (Mod->getName() != ModuleName.getName()) {
    Diags.report(ModuleName.getLoc(), diag::err_module_identifier_not_equal)
        << Mod->getName() << ModuleName.getName();
  }
  Mod->setProtection(Protection);
  Mod->setDecls(Decls);
  Mod->setInitBlk(InitBlk);
  Mod->setFinalBlk(FinalBlk);
}

// Ensures that all identifiers in the identifier list are unique.
// Emit an error when a duplicate is found.
void Sema::unique(IdentifierList &IdList, unsigned Error, unsigned Note) {
  llvm::DenseMap<llvm::StringRef, llvm::SMLoc> Set;
  IdList.erase(std::remove_if(IdList.begin(), IdList.end(),
                              [&](const Identifier &Id) {
                                auto [It, WasInserted] = Set.insert(
                                    std::pair(Id.getName(), Id.getLoc()));
                                if (!WasInserted) {
                                  Diags.report(Id.getLoc(), Error)
                                      << Id.getName();
                                  Diags.report(It->second, Note) << It->first;
                                }
                                return !WasInserted;
                              }),
               IdList.end());
}

// If Decl is a module, the return the module scope.
// Otherwise emits an error message, and returns nullptr.
Scope *Sema::getScopeOfModule(Declaration *Decl) {
  Scope *Sc = nullptr;
  if (auto *IM = llvm::dyn_cast_or_null<ImplementationModule>(Decl)) {
    Sc = IM->getScope();
  } else if (auto *DM = llvm::dyn_cast_or_null<DefinitionModule>(Decl)) {
    Sc = DM->getScope();
  } else if (auto *LM = llvm::dyn_cast_or_null<LocalModule>(Decl)) {
    Sc = LM->getScope();
  } else {
    Diags.report(Decl->getLoc(), diag::err_module_expected);
  }
  return Sc;
}

void Sema::actOnSimpleImport(ImportItemList &Imports, IdentifierList &IdList) {
  // ISO 10514:1994, Clause 6.1.8.2
  // The identifiers imported by a single import shall be distinct from each
  // other.
  unique(IdList, diag::err_symbol_already_in_import_list,
         diag::note_symbol_already_in_import_list);
  if (auto *LM = llvm::dyn_cast<LocalModule>(CurrentDecl)) {
    Scope *EnclosingScope = LM->getEnclosingDecl()->getScope();
    Scope *ModuleScope = LM->getScope();
    for (auto &Id : IdList) {
      if (Declaration *Decl = EnclosingScope->lookup(Id.getName())) {
        Imports.emplace_back(Id.getLoc(), Decl, nullptr);
        extendScopeOfDecl(ModuleScope, Decl);
      } else
        Diags.report(Id.getLoc(), diag::err_imported_symbol_undeclared)
            << Id.getName() << LM->getEnclosingDecl()->getName();
    }
  } else {
    assert(CurrentDecl->getEnclosingDecl() == nullptr &&
           "Not local module, and import not from outermost scope");
    Scope *SearchScope = &GlobalScope;
    Scope *ModuleScope = getScopeOfModule(CurrentDecl);
    if (ModuleScope) {
      for (auto &Id : IdList) {
        if (Declaration *Decl = SearchScope->lookup(Id.getName())) {
          Imports.emplace_back(Id.getLoc(), Decl, nullptr);
          extendScopeOfDecl(ModuleScope, Decl);
        } else {
          // TODO
          // Search file Id.getName().append(".def")
          // Parse file - must be definition or redefining module
          // add decl to module scope.
          llvm::report_fatal_error(
              "Import from outer scope not yet implemented");
        }
      }
    }
  }
}

void Sema::actOnUnqualifiedImport(ImportItemList &Imports,
                                  Identifier ModuleName,
                                  IdentifierList &IdList) {
  // ISO 10514:1994, Clause 6.1.8.3
  // The identifiers imported by an unqualified import shall be distinct from
  // each other.
  unique(IdList, diag::err_symbol_already_in_import_list,
         diag::note_symbol_already_in_import_list);
  if (auto *LM = llvm::dyn_cast<LocalModule>(CurrentDecl)) {
    Scope *EnclosingScope = LM->getEnclosingDecl()->getScope();
    Scope *ModuleScope = LM->getScope();
    Declaration *M = EnclosingScope->lookup(ModuleName.getName());
    if (Scope *SearchScope = getScopeOfModule(M)) {
      for (auto &Id : IdList) {
        if (Declaration *Decl = SearchScope->lookup(Id.getName())) {
          Imports.emplace_back(Id.getLoc(), Decl, M);
          extendScopeOfDecl(ModuleScope, Decl);
        } else
          Diags.report(Id.getLoc(), diag::err_imported_symbol_undeclared)
              << Id.getName() << ModuleName.getName();
      }
    }
  } else {
    assert(CurrentDecl->getEnclosingDecl() == nullptr &&
           "Not local module, and import not from outermost scope");
    Scope *SearchScope = &GlobalScope;
    if (Scope *ModuleScope = getScopeOfModule(CurrentDecl)) {
      for (auto &Id : IdList) {
        if (Declaration *Decl = SearchScope->lookup(Id.getName())) {
          Imports.emplace_back(Id.getLoc(), Decl, CurrentDecl);
          extendScopeOfDecl(ModuleScope, Decl);
        } else { // TODO
          // Search file Id.getName().append(".def")
          // Parse file - must be definition or redefining module
          // add decl to module scope.
          llvm::report_fatal_error(
              "Import from outer scope not yet implemented");
        }
      }
    }
  }
}

Procedure *Sema::actOnProcedure(Identifier ProcName) {
  Scope *ProcScope = new Scope(CurrentScope);
  Procedure *Proc = new (ASTCtx)
      Procedure(CurrentDecl, ProcName.getLoc(), ProcName.getName(), ProcScope);
  addToCurrentScope(Proc);
  return Proc;
}

void Sema::actOnProcedureHeading(DeclarationList &Decls, Procedure *Proc,
                                 FormalParameterList &Params,
                                 Type *ResultType) {
  Proc->setParams(Params);
  Proc->setResultType(ResultType);
  Decls.push_back(Proc);
}

void Sema::actOnProcedure(Procedure *Proc, Identifier ProcName,
                          const DeclarationList &ProcDecls, Block Body,
                          bool IsFunction) {
  if (Proc->getName() != ProcName.getName()) {
    Diags.report(ProcName.getLoc(), diag::err_proc_identifier_not_equal)
        << Proc->getName() << ProcName.getName();
  }
  Proc->setDecls(ProcDecls);
  Proc->setBody(Body);
}

void Sema::actOnForwardProcedure(DeclarationList &Decls, Procedure *Proc) {
  Proc->setForward(true);
  Decls.push_back(Proc);
}

void Sema::actOnConstant(DeclarationList &Decls, Identifier Name,
                         Expression *Expr) {
  llvm::outs() << "Sema::actOnConstant: Name = " << Name.getName() << "\n";
  Constant *Const = new (ASTCtx) Constant(
      CurrentDecl, Name.getLoc(), Name.getName(), Expr->getTypeDenoter(), Expr);
  addToCurrentScope(Const);
  Decls.push_back(Const);
}

void Sema::actOnType(DeclarationList &Decls, Identifier TypeName,
                     TypeDenoter *TyDen) {
  llvm::outs() << "Sema::actOnType: Name = " << TypeName.getName() << "\n";
  Type *Ty = new (ASTCtx)
      Type(CurrentDecl, TypeName.getLoc(), TypeName.getName(), TyDen);
  addToCurrentScope(Ty);
  Decls.push_back(Ty);
}

void Sema::actOnVariable(DeclarationList &Decls,
                         VariableIdentifierList &VarIdList,
                         TypeDenoter *TyDen) {
  llvm::outs() << "Sema::actOnVariable\n";
  assert(CurrentScope && "CurrentScope not set");
  // TODO This is too simple, because a local module can be inside a procedure.
  Variable::StorageType ST =
      llvm::isa<Procedure>(CurrentDecl) ? Variable::Stack : Variable::Module;
  // if (Type *Ty = dyn_cast<TypeDeclaration>(D)) {
  for (auto const &[Name, Addr] : VarIdList) {
    llvm::outs() << " -> Add variable " << Name.getName() << "\n";
    Variable *Var = new (ASTCtx)
        Variable(CurrentDecl, Name.getLoc(), Name.getName(), TyDen, Addr, ST);
    if (addToCurrentScope(Var))
      Decls.push_back(Var);
  }
  //} else if (!Ids.empty()) {
  //  llvm::SMLoc Loc = Ids.front().first;
  //  Diags.report(Loc, diag::err_vardecl_requires_type);
  //}
}

void Sema::actOnActualParameter(ActualParameterList &Params, Expression *Expr) {
  // An actual parameter can be an expression or a type.
  // TODO Evaluate later if this extraction is worth the trouble.
  ActualParameter Param = Expr;
  if (auto *Desig = llvm::dyn_cast_or_null<Designator>(Expr)) {
    if (llvm::isa_and_nonnull<Type>(Desig->getDecl()) &&
        Desig->getSelectors().size() == 0) {
      Param = llvm::cast<Type>(Desig->getDecl());
      ;
      delete Expr;
    }
  }
  Params.push_back(Param);
}

void Sema::actOnFormalParameter(FormalParameterList &Params,
                                const IdentifierList &IdentList,
                                bool IsCallByReference, TypeDenoter *FTy) {
  llvm::outs() << "Sema::actOnFormalParameter\n";
  for (auto const &Id : IdentList) {
    FormalParameter *Param = new (ASTCtx) FormalParameter(
        CurrentDecl, Id.getLoc(), Id.getName(), FTy, IsCallByReference);
    addToCurrentScope(Param);
    Params.push_back(Param);
  }
}

void Sema::actOnExportList(LocalModule *LM, IdentifierList &IdList,
                           bool IsQualified) {
  // ISO 10514:1994, Clause 6.1.9.1
  // The identifiers in the identifier list of an unqualified export shall be
  // distinct from each other.
  // ISO 10514:1994, Clause 6.1.9.2
  // The identifiers in the identifier list of a qualified export shall be
  // distinct from each other.
  unique(IdList, diag::err_symbol_already_in_export_list,
         diag::note_symbol_already_in_export_list);
  LM->setExports(IdList);
  LM->setQualified(IsQualified);
}

// Extends the visibility of declaration Decl to scope Sc.
void Sema::extendScopeOfDecl(Scope *Sc, Declaration *Decl) {
  addToScope(Sc, Decl);
  // ISO 10514:1994, Clause 6.1.10
  // The set of identifiers associated with the values of the enumeration
  // type are implicitly exported.
  if (Type *Ty = llvm::dyn_cast<Type>(Decl))
    if (EnumerationType *ET =
            llvm::dyn_cast<EnumerationType>(Ty->getTypeDenoter())) {
      for (auto *Const : ET->getMembers())
        addToScope(Sc, Const);
    }
}

void Sema::actOnBlockBegin() { handleUnresolvedPointer(CurrentDecl); }

void Sema::actOnModuleBlockEnd() {
  if (auto *LM = llvm::dyn_cast<LocalModule>(CurrentDecl)) {
    for (auto const &Id : LM->getExports()) {
      if (Declaration *Decl = CurrentScope->lookup(Id.getName(), false)) {
        extendScopeOfDecl(LM->getExportScope(), Decl);
        if (!LM->isQualified())
          extendScopeOfDecl(LM->getEnclosingDecl()->getScope(), Decl);
      } else
        Diags.report(Id.getLoc(), diag::err_exported_symbol_undeclared)
            << Id.getName() << LM->getName();
    }
    LM->getExportScope()->dump();
  }
}

Declaration *Sema::actOnQualifiedIdentifier(Declaration *ModOrClassDecl,
                                            Identifier Name) {
  llvm::outs() << "Sema::actOnQualifiedIdentifier: Name = " << Name.getName()
               << "\n";
  Scope *SearchScope = CurrentScope;
  if (ModOrClassDecl) {
    if (auto *LM = llvm::dyn_cast<LocalModule>(ModOrClassDecl))
      SearchScope = LM->getExportScope();
    else if (ASTCtx.getLangOpts().ISOObjects &&
             llvm::isa<Class>(ModOrClassDecl))
      llvm::report_fatal_error("Class lookup not yet implemented");
    else {
      llvm::report_fatal_error("Module lookup not yet implemented");
    }
  }
  Declaration *Decl = SearchScope->lookup(Name.getName());
  if (!Decl) {
    Diags.report(Name.getLoc(), diag::err_symbol_not_declared)
        << Name.getName();
    SearchScope->dump();
  }
  return Decl;
}

TypeDenoter *Sema::actOnTypeIdentifier(llvm::SMLoc Loc, Declaration *Decl) {
  if (auto *TypeDecl = llvm::dyn_cast_or_null<Type>(Decl)) {
    // ISO 10514:1994, Clause 6.3.1
    // Replace the type identifier with the type denoter.
    return TypeDecl->getTypeDenoter();
  }
  return nullptr;
}

TypeDenoter *Sema::actOnOrdinalTypeIdentifier(Declaration *Decl) {
  if (auto *TypeDecl = llvm::dyn_cast_or_null<Type>(Decl)) {
    // ISO 10514:1994, Clause 6.3.1
    // Replace the type identifier with the type denoter.
    TypeDenoter *TyDen = TypeDecl->getTypeDenoter();
    if (!isOrdinalType(TyDen)) {
      Diags.report(Decl->getLoc(), diag::err_ordinal_type_expected);
    }
    return TyDen;
  }
  if (Decl) {
    Diags.report(Decl->getLoc(), diag::err_type_expected) << Decl->getName();
  }
  return nullptr;
}

void Sema::actOnFixedFields(StringIndexMap &FieldMap, RecordFieldList &Fields,
                            const IdentifierList &IdList, TypeDenoter *TyDe) {
  // ISO 10514:1994, Clause 6.3.12:  All the field identiers of a record type
  // shall be distinct.
  // ISO 10514:1994, Clause 6.3.12.1: A group of fields specified as fixed
  // fields of a record type share the same field type.
  for (auto I = IdList.begin(), E = IdList.end(); I != E; ++I) {
    auto InsertRes = FieldMap.insert(std::pair(I->getName(), Fields.size()));
    if (!InsertRes.second) {
      Diags.report(I->getLoc(), diag::err_duplicate_field) << I->getName();
      Diags.report(IdList[InsertRes.first->second].getLoc(),
                   diag::note_previous_declaration);
    }
    Fields.emplace_back(I->getName(), TyDe);
  }
}

RecordType *Sema::actOnRecordType(StringIndexMap &FieldMap,
                                  RecordFieldList &Fields) {
  return new (ASTCtx) RecordType(FieldMap, Fields);
}

ArrayType *Sema::actOnArrayType(TypeDenoter *ComponentType,
                                const TypeDenoterList &IndexTypeList) {
  assert(!IndexTypeList.empty() && "Index type list must not be empty");
  for (auto I = IndexTypeList.rbegin(), E = IndexTypeList.rend(); I != E; ++I) {
    // The index type list contains only ordinal types.
    // This was already checked during parsing, no need to check again.
    assert(isOrdinalType(*I) && "Index type list contains non-ordinal type");
    // ISO 10514:1994, Clause 6.3.11
    ComponentType = new (ASTCtx) ArrayType(ComponentType, *I);
  }
  return llvm::cast<ArrayType>(ComponentType);
}

void Sema::actOnFormalParameterType(FormalParameterTypeList &ParameterTypes,
                                    llvm::SMLoc Loc, bool IsCallByReference,
                                    TypeDenoter *TyDe) {
  ParameterTypes.emplace_back(Loc, TyDe, IsCallByReference);
}

ProcedureType *
Sema::actOnProcedureType(Type *ResultType,
                         FormalParameterTypeList &ParameterTypes) {
  return new (ASTCtx) ProcedureType(ResultType, ParameterTypes);
}

TypeDenoter *Sema::actOnFormalType(Type *Ty, unsigned OpenArrayLevel) {
  TypeDenoter *FT = Ty->getTypeDenoter();
  while (OpenArrayLevel-- > 0)
    FT = new (ASTCtx) OpenArrayFormalType(FT);
  return FT;
}

PointerType *Sema::actOnPointerType(TypeDenoter *TyDen) {
  PointerType *PtrTy = new (ASTCtx) PointerType();
  PtrTy->setTyDen(TyDen);
  return PtrTy;
}

PointerType *Sema::actOnPointerType(Identifier Name) {
  PointerType *PtrTy = new (ASTCtx) PointerType();
  UnresolvedPointer.push_back(
      std::tuple<ScopedDeclaration *, PointerType *, Identifier>(CurrentDecl,
                                                                 PtrTy, Name));
  return PtrTy;
}

SubrangeType *Sema::actOnSubrangeType(Declaration *Decl, Expression *From,
                                      Expression *To) {
  Type *Ty = llvm::dyn_cast_or_null<Type>(Decl);
  if (Ty && Ty->getTypeDenoter() && !isOrdinalType(Ty->getTypeDenoter())) {
    Diags.report(Decl->getLoc(), diag::err_ordinal_type_expected);
  }
  // TODO Ty must be ordinal type.
  if (Decl && !Ty) {
    // Emit error message
  }
  return new (ASTCtx) SubrangeType(Ty, From, To);
}

EnumerationType *Sema::actOnEnumerationType(const IdentifierList &IdList) {
  EnumerationType *EnumTyDe = new (ASTCtx) EnumerationType();
  // ISO 10514:1994, Clause 6.3.4: The ordinal number of the value associated
  // with the i-th identifier of the list shall be the value (i-1).
  uint64_t Ord = 0;
  for (auto &Id : IdList) {
    llvm::APInt Value(64, Ord);
    // ISO 10514:1994, Clause 6.3.4: Each identifier ... shall be defined as a
    // constant identifier.
    Constant *Const =
        new (ASTCtx) Constant(CurrentDecl, Id.getLoc(), Id.getName(), EnumTyDe,
                              new (ASTCtx) IntegerLiteral(EnumTyDe, Value));
    ++Ord;
    EnumTyDe->getMembers().push_back(Const);
    addToCurrentScope(Const);
  }
  return EnumTyDe;
}

SetType *Sema::actOnSetType(TypeDenoter *BaseType, bool IsPacked) {
  // ISO 10514:1994, Clause 6.3.6
  // Check: Base type must be ordinal type identifier or
  // a new enumeration or subrange.
  if (!isOrdinalType(BaseType)) {
    Diags.report(llvm::SMLoc(), diag::err_ordinal_type_expected);
  }
  return new (ASTCtx) SetType(BaseType, IsPacked);
}

Type *Sema::actOnTypeIdentifier(Declaration *TypeDecl) {
  if (auto *Ty = llvm::dyn_cast_or_null<Type>(TypeDecl)) {
    return Ty;
  }
  Diags.report(TypeDecl->getLoc(), diag::err_type_expected)
      << TypeDecl->getName();
  return nullptr;
}

void Sema::actOnAssignmentStmt(StatementList &Stmts, llvm::SMLoc Loc,
                               Designator *Left, Expression *Right) {
  AssignmentStatement *Stmt =
      new (ASTCtx) AssignmentStatement(Loc, Left, Right);
  Stmts.push_back(Stmt);
}

void Sema::actOnProcedureCallStmt(StatementList &Stmts, llvm::SMLoc Loc,
                                  Designator *Proc,
                                  const ActualParameterList &ActualParameters) {
  ProcedureCallStatement *Stmt =
      new (ASTCtx) ProcedureCallStatement(Loc, Proc, ActualParameters);
  Stmts.push_back(Stmt);
}

void Sema::actOnIfStmt(StatementList &Stmts, GuardedStatementList &GuardedStmts,
                       StatementList &ElseStmts) {
  llvm::SMLoc Loc = GuardedStmts[0].getLoc();
  IfStatement *Stmt = new (ASTCtx) IfStatement(Loc);
  Stmt->setGuardedStmts(GuardedStmts);
  Stmt->setElseStmts(ElseStmts);
  Stmts.push_back(Stmt);
}

void Sema::actOnGuardedStmt(GuardedStatementList &GuardedStmts, llvm::SMLoc Loc,
                            Expression *Cond, StatementList &Stmts) {
  llvm::outs() << "actOnGuardedStmt\n";
  llvm::outs() << "Cond: " << Cond << "\n";
  llvm::outs() << "Stmts: " << Stmts.size() << "\n";
  llvm::outs() << "GuardedStmts: " << GuardedStmts.size() << "\n";
  if (Cond->getTypeDenoter() != ASTCtx.BooleanTyDe)
    Diags.report(Loc, diag::err_condition_requires_boolean_expression);
  GuardedStmts.emplace_back(Loc, Cond, Stmts);
}

void Sema::actOnCaseStmt(StatementList &Stmts, llvm::SMLoc Loc) {
  llvm::outs() << "actOnCaseStmt\n";
}

void Sema::actOnWhileStmt(StatementList &Stmts, llvm::SMLoc Loc,
                          Expression *Cond, StatementList &WhileStmts) {
  // ISO 10514:1994, Clause 6.6.10: A while statement boolean expression shall
  // be of the Boolean type.
  if (Cond->getTypeDenoter() != ASTCtx.BooleanTyDe)
    Diags.report(Loc, diag::err_condition_requires_boolean_expression);
  WhileStatement *Stmt = new (ASTCtx) WhileStatement(Loc, Cond, WhileStmts);
  Stmts.push_back(Stmt);
}

void Sema::actOnRepeatStmt(StatementList &Stmts, llvm::SMLoc Loc,
                           Expression *Cond, StatementList &RepeatStmts) {
  // ISO 10514:1994, Clause 6.6.11: A repeat statement boolean expression shall
  // be of the Boolean type.
  if (Cond->getTypeDenoter() != ASTCtx.BooleanTyDe)
    Diags.report(Loc, diag::err_condition_requires_boolean_expression);
  RepeatStatement *Stmt = new (ASTCtx) RepeatStatement(Loc, Cond, RepeatStmts);
  Stmts.push_back(Stmt);
}

void Sema::actOnLoopStmt(StatementList &Stmts, llvm::SMLoc Loc,
                         StatementList &LoopStmts) {
  // ISO 10514:1994, Clause 6.6.12.
  LoopStatement *Stmt = new (ASTCtx) LoopStatement(Loc, LoopStmts);
  Stmts.push_back(Stmt);
}

void Sema::actOnForStmt(StatementList &Stmts, llvm::SMLoc Loc,
                        Identifier ControlVariable, Expression *InitialValue,
                        Expression *FinalValue, Expression *StepSize,
                        const StatementList &ForStmts) {
  llvm::outs() << "actOnForStmt\n";
  // FIXME: Add the checkes for well-formed control variables.
  // ISO 10514:1994, Clause 6.6.15
  Declaration *Decl = CurrentScope->lookup(ControlVariable.getName());
  if (auto *Var = llvm::dyn_cast_or_null<Variable>(Decl)) {
    // ISO 10514:1994, Clause 6.6.15: The type of the control variable shall be
    // an ordinal type that is assignment-compatible with the type of the
    // initial value.
    // FIXME: Add the missing checks.
    if (!isOrdinalType(Var->getTypeDenoter()))
      Diags.report(Loc, diag::err_ordinal_type_expected);
    if (!assignCompatible(Var->getTypeDenoter(),
                          InitialValue->getTypeDenoter()))
      Diags.report(Loc, diag::err_expressions_are_not_assignable);
    if (!exprCompatible(Var->getTypeDenoter(), FinalValue->getTypeDenoter()))
      Diags.report(Loc, diag::err_expressions_are_not_compatible);
    if (StepSize) {
      if (!StepSize->isConst())
        Diags.report(Loc, diag::err_constant_expected);
      if (!isWholeNumberType(StepSize->getTypeDenoter()))
        Diags.report(Loc, diag::err_whole_number_type_required);
    } else {
      llvm::APInt Value(64, 1);
      StepSize = new (ASTCtx) IntegerLiteral(ASTCtx.CardinalTyDe, Value);
    }
    ForStatement *Stmt = new (ASTCtx)
        ForStatement(Loc, Var, InitialValue, FinalValue, StepSize, ForStmts);
    Stmts.push_back(Stmt);
  } else
    Diags.report(Loc, diag::err_simple_variable_required);
}

void Sema::actOnWithStmt(StatementList &Stmts, llvm::SMLoc Loc,
                         Designator *Desig, StatementList &WithStmts) {
  // ISO 10514:1994, Clause 6.6.7:  The record designator shall denote a record
  // variable or a record value.
  //  Within the statement sequence the field identifiers of the record variable
  //  or record value may be used without selection.
  if (!llvm::isa<RecordType>(Desig->getTypeDenoter()))
    Diags.report(Loc, diag::err_with_requires_record_type);
  WithStatement *Stmt = new (ASTCtx) WithStatement(Loc, WithStmts, Desig);
  Stmts.push_back(Stmt);
  // TODO Install record fields in scope.
}

void Sema::actOnExitStmt(StatementList &Stmts, llvm::SMLoc Loc) {
  // ISO 10514:1994, Clause 6.6.13: An exit statement may occur only within a
  // loop statement where it specifies the termination of that loop statement.
  ExitStatement *Stmt = new (ASTCtx) ExitStatement(Loc);
  Stmts.push_back(Stmt);
  addToExitStmtList(Stmt);
}

void Sema::actOnReturnStmt(StatementList &Stmts, llvm::SMLoc Loc,
                           Expression *E) {
  llvm::outs() << "actOnReturnStmt " << E << "\n";
  if (llvm::isa<CompilationModule>(CurrentDecl)) {
    if (E)
      Diags.report(Loc, diag::err_module_requires_simple_return);
  } else {
    auto *Proc = llvm::dyn_cast<Procedure>(CurrentDecl);
    auto *ResultType = Proc->getResultType();
    if (ResultType && !E)
      Diags.report(Loc, diag::err_function_requires_return_expression);
    else if (!ResultType && E)
      Diags.report(Loc, diag::err_procedure_requires_simple_return);
    else if (ResultType && E) {
      if (!assignCompatible(ResultType->getTypeDenoter(), E->getTypeDenoter()))
        Diags.report(Loc, diag::err_expressions_are_not_assignable);
    }
  }
  ReturnStatement *Stmt = new (ASTCtx) ReturnStatement(Loc, E);
  Stmts.push_back(Stmt);
}

void Sema::actOnRetryStmt(StatementList &Stmts, llvm::SMLoc Loc) {
  llvm::outs() << "actOnRetryStmt\n";
  RetryStatement *Stmt = new (ASTCtx) RetryStatement(Loc);
  Stmts.push_back(Stmt);
}

Expression *Sema::actOnExpression(Expression *Left, Expression *Right,
                                  const OperatorInfo &Op) {
  llvm::outs() << "actOnExpression\n";
  // Op is a relational operation.
  const bool IsConst = Left && Right && Left->isConst() && Right->isConst();
  return new (ASTCtx)
      InfixExpression(ASTCtx.BooleanTyDe, IsConst, Left, Right, Op);
}

Expression *Sema::actOnSimpleExpression(Expression *Left, Expression *Right,
                                        const OperatorInfo &Op) {
  llvm::outs() << "actOnSimpleExpression\n";
  // Op is a term operation.
  TypeDenoter *TyDe =
      exprCompatible(Left->getTypeDenoter(), Right->getTypeDenoter());
  if (!TyDe)
    Diags.report(Op.getLocation(), diag::err_expressions_are_not_compatible);
  const bool IsConst = Left && Right && Left->isConst() && Right->isConst();
  return new (ASTCtx) InfixExpression(TyDe, IsConst, Left, Right, Op);
}

Expression *Sema::actOnTerm(Expression *Left, Expression *Right,
                            const OperatorInfo &Op) {
  llvm::outs() << "actOnTerm\n";
  // Op is a factor operation.
  TypeDenoter *TyDe =
      exprCompatible(Left->getTypeDenoter(), Right->getTypeDenoter());
  if (!TyDe)
    Diags.report(Op.getLocation(), diag::err_expressions_are_not_compatible);
  const bool IsConst = Left && Right && Left->isConst() && Right->isConst();
  if (IsConst) {
  }
  return new (ASTCtx) InfixExpression(TyDe, IsConst, Left, Right, Op);
}

Expression *Sema::actOnNot(Expression *E, const OperatorInfo &Op) {
  llvm::outs() << "actOnNot\n";
  if (E->getTypeDenoter() != ASTCtx.BooleanTyDe) {
    Diags.report(Op.getLocation(), diag::err_not_requires_boolean_expression);
  }
  return new (ASTCtx) PrefixExpression(ASTCtx.BooleanTyDe, E->isConst(), E, Op);
}

Expression *Sema::actOnPrefixOperator(Expression *E, const OperatorInfo &Op) {
  llvm::outs() << "actOnPrefixOperator\n";
  if (Op.getKind() == tok::minus) {
    // The minus sign prefixing a simple expression is ambiguous.
    // According to the grammar, the whole expression is prefixed.
    // So - a + b is interpreted as -(a + b), which is not the expected (and
    // mathematically correct) result. Instead of fixing the grammar, the
    // original behaviour is retained, and a warning is emitted.
    // Warn about ambiguous minus prefix if not
    // - expression is an integer/real literal
    // - expresion operator is a multiplicative operator
    Diags.report(Op.getLocation(), diag::warn_ambigous_negation);
  }
  const bool IsConst = E && E->isConst();
  return new (ASTCtx) PrefixExpression(nullptr, IsConst, E, Op);
}

Expression *Sema::actOnIntegerLiteral(llvm::SMLoc Loc,
                                      llvm::StringRef LiteralData) {
  uint8_t Radix = 10;
  if (LiteralData.ends_with("B") || LiteralData.ends_with("H")) {
    Radix = LiteralData.ends_with("B") ? 8 : 16;
    LiteralData = LiteralData.drop_back();
  }
  llvm::APInt Value(64, LiteralData, Radix);

  return new (ASTCtx) IntegerLiteral(ASTCtx.WholeNumberTyDe, Value);
}

Expression *Sema::actOnRealLiteral(llvm::SMLoc Loc,
                                   llvm::StringRef LiteralData) {
  llvm::APFloat Value(llvm::APFloat::IEEEquad(), LiteralData);
  return new (ASTCtx) RealLiteral(ASTCtx.RealNumberTyDe, Value);
}

Expression *Sema::actOnStringLiteral(llvm::SMLoc Loc,
                                     llvm::StringRef LiteralData) {
  return new (ASTCtx) StringLiteral(
      ASTCtx.StringLiteralTyDe, LiteralData.substr(1, LiteralData.size() - 2));
}

Expression *Sema::actOnCharLiteral(llvm::SMLoc Loc,
                                   llvm::StringRef LiteralData) {
  assert(LiteralData.size() == 1 && "Unexpected length of string");
  return new (ASTCtx) CharLiteral(ASTCtx.CharTyDe, LiteralData[1]);
}

Designator *Sema::actOnDesignator(Declaration *QualId,
                                  const SelectorList &Selectors) {
  auto ActOnSelectorList = [this,
                            &Selectors](TypeDenoter *TyDenot) -> TypeDenoter * {
    if (Selectors.empty())
      return TyDenot;
    for (auto *Sel : Selectors) {
      if (auto *IndexSel = llvm::dyn_cast<IndexSelector>(Sel)) {
        TypeDenoter *IndexTy = nullptr;
        TypeDenoter *ComponentTy = nullptr;
        if (auto *ArrayTy = llvm::dyn_cast<ArrayType>(TyDenot)) {
          // ISO 10514:1994, Clause 6.7.2.
          IndexTy = ArrayTy->getIndexType();
          ComponentTy = ArrayTy->getComponentType();
        } else if (auto *ArrayTy =
                       llvm::dyn_cast<OpenArrayFormalType>(TyDenot)) {
          // FIXME: Where is this defined in the standard?
          IndexTy = ASTCtx.WholeNumberTyDe;
          ComponentTy = ArrayTy->getComponentType();
        } else {
          // FIXME Diagnostic.
          // TODO Return an error type?
          return TyDenot;
        }
        if (!assignCompatible(IndexTy,
                              IndexSel->getIndex()->getTypeDenoter())) {
          Diags.report(IndexSel->getLoc(),
                       diag::err_expressions_are_not_assignable);
          // TODO Return an error type?
          return TyDenot;
        }
        TyDenot = ComponentTy;
      } else if ([[maybe_unused]] auto *DerefSel =
                     llvm::dyn_cast<DereferenceSelector>(Sel)) {
        if (auto *PtrTy = llvm::dyn_cast<PointerType>(TyDenot)) {
          TyDenot = PtrTy->getTyDen();
        } else {
          // FIXME Diagnostic.
          // TODO Return an error type?
          return TyDenot;
        }
      } else if ([[maybe_unused]] auto *FieldSel =
                     llvm::dyn_cast<FieldSelector>(Sel)) {
        if (auto *RecTy = llvm::dyn_cast<RecordType>(TyDenot)) {
          // TODO Need a StringMap in RecordType.
          bool Found = false;
          for (auto FixedField : RecTy->getFields())
            if (FixedField.getName() == FieldSel->getName()) {
              TyDenot = FixedField.getTyDe();
              Found = true;
              break;
            }
          if (!Found) {
            // FIXME Diagnostic.
            // TODO Return an error type?
            return TyDenot;
          }
        } else {
          // FIXME Diagnostic.
          // TODO Return an error type?
          return TyDenot;
        }
      }
    }
    return TyDenot;
  };

  // TODO Compute if value / or variable
  // TODO Compute const or not
  bool IsConst = false;
  bool IsReference = false;
  TypeDenoter *TyDenot = nullptr;
  if (auto *Var = llvm::dyn_cast_or_null<Variable>(QualId)) {
    IsReference = true;
    TyDenot = ActOnSelectorList(Var->getTypeDenoter());
  } else if (auto *FParam = llvm::dyn_cast_or_null<FormalParameter>(QualId)) {
    IsReference = FParam->isCallByReference();
    TyDenot = ActOnSelectorList(FParam->getType());
  } else if (auto *Const = llvm::dyn_cast_or_null<Constant>(QualId)) {
    IsConst = true;
    TyDenot = ActOnSelectorList(Const->getTypeDenoter());
  } else if ([[maybe_unused]] auto *Proc =
                 llvm::dyn_cast_or_null<Procedure>(QualId)) {
    // Something todo for a procedure?
    // Create TypeDenoter for procedure?
  } else {
    // TODO Emit error message.
  }
  Designator *D =
      new (ASTCtx) Designator(TyDenot, IsConst, QualId, IsReference);
  D->setSelectors(Selectors);
  return D;
}

Expression *
Sema::actOnFunctionCall(Expression *DesignatorExpr,
                        const ActualParameterList &ActualParameters) {
  if (auto *Func = llvm::dyn_cast_or_null<Designator>(DesignatorExpr)) {
    // TODO Check parameter list
    return new (ASTCtx)
        FunctionCall(Func->getTypeDenoter(), false, Func, ActualParameters);
  }
  // TODO Emit error message.
  return nullptr;
}

Expression *
Sema::actOnValueConstructor(Declaration *QualId /*, ConstructorValues */) {
  // TODO Implement
  return new (ASTCtx) ValueConstructor(nullptr);
}

Expression *Sema::actOnOrdinalExpression(llvm::SMLoc Loc, Expression *E) {
  if (!isOrdinalType(E->getTypeDenoter())) {
    Diags.report(Loc, diag::err_ordinal_expressions_required);
    // TODO Return error expression with ordinal type?
  }
  return E;
}

void Sema::actOnIndexSelector(llvm::SMLoc Loc, SelectorList &Selectors,
                              Expression *E) {
  // ISO 10514:1994, Clause 6.7.2.
  assert(isOrdinalType(E->getTypeDenoter()) && "Ordinal expression expected");
  IndexSelector *Sel = new (ASTCtx) IndexSelector(Loc, E);
  Selectors.push_back(Sel);
}

void Sema::actOnFieldSelector(SelectorList &Selectors, Identifier Field) {
  // ISO 10514:1994, Clause 6.7.3.
  FieldSelector *Sel =
      new (ASTCtx) FieldSelector(Field.getLoc(), Field.getName());
  Selectors.push_back(Sel);
}

void Sema::actOnDereferenceSelector(llvm::SMLoc Loc, SelectorList &Selectors) {
  // ISO 10514:1994, Clause 6.7.4.
  DereferenceSelector *Sel = new (ASTCtx) DereferenceSelector(Loc);
  Selectors.push_back(Sel);
}
