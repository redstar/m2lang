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

void Sema::initialize() {
  CurrentScope = new Scope();
  CurrentDecl = nullptr;
  BitSetTypeDenoter = PervasiveType::create();
  CharTypeDenoter = PervasiveType::create();
  IntegerTypeDenoter = PervasiveType::create();
  CardinalTypeDenoter = PervasiveType::create();
  BooleanTypeDenoter = PervasiveType::create();
  RealTypeDenoter = PervasiveType::create();
  LongRealTypeDenoter = PervasiveType::create();
  ComplexTypeDenoter = PervasiveType::create();
  LongComplexTypeDenoter = PervasiveType::create();
  BitSetType = Type::create(CurrentDecl, SMLoc(), "BITSET", IntegerTypeDenoter);
  CharType = Type::create(CurrentDecl, SMLoc(), "CHAR", IntegerTypeDenoter);
  IntegerType =
      Type::create(CurrentDecl, SMLoc(), "INTEGER", IntegerTypeDenoter);
  CardinalType =
      Type::create(CurrentDecl, SMLoc(), "CARDINAL", CardinalTypeDenoter);
  BooleanType =
      Type::create(CurrentDecl, SMLoc(), "BOOLEAN", BooleanTypeDenoter);
  RealType = Type::create(CurrentDecl, SMLoc(), "REAL", IntegerTypeDenoter);
  LongRealType =
      Type::create(CurrentDecl, SMLoc(), "LONGREAL", IntegerTypeDenoter);
  ComplexType =
      Type::create(CurrentDecl, SMLoc(), "COMPLEX", IntegerTypeDenoter);
  LongComplexType =
      Type::create(CurrentDecl, SMLoc(), "LONGCOMPLEX", IntegerTypeDenoter);
  TrueLiteral = BooleanLiteral::create(true, BooleanTypeDenoter);
  FalseLiteral = BooleanLiteral::create(false, BooleanTypeDenoter);
  TrueConst =
      Constant::create(CurrentDecl, SMLoc(), "TRUE", BooleanType, TrueLiteral);
  FalseConst = Constant::create(CurrentDecl, SMLoc(), "FALSE", BooleanType,
                                FalseLiteral);
  CurrentScope->insert(CharType);
  CurrentScope->insert(IntegerType);
  CurrentScope->insert(CardinalType);
  CurrentScope->insert(BooleanType);
  CurrentScope->insert(RealType);
  CurrentScope->insert(LongRealType);
  CurrentScope->insert(ComplexType);
  CurrentScope->insert(LongComplexType);
  CurrentScope->insert(TrueConst);
  CurrentScope->insert(FalseConst);
}

void Sema::enterScope(Declaration *Decl) {
  CurrentScope = new Scope(CurrentScope);
  CurrentDecl = Decl;
}

void Sema::leaveScope() {
  assert(CurrentScope && "Can't leave non-existing scope");
  Scope *Parent = CurrentScope->getParent();
  delete CurrentScope;
  CurrentScope = Parent;
  CurrentDecl = CurrentDecl->getEnclosingDecl();
}

bool Sema::isUndeclared(StringRef Name) {
  return nullptr == CurrentScope->lookup(Name);
}

bool Sema::isModule(StringRef Name) {
  llvm::outs() << "Sema::isModule: " << Name << "\n";
  Declaration *Decl = CurrentScope->lookup(Name);
  return llvm::isa_and_nonnull<CompilationModule>(Decl) ||
         llvm::isa_and_nonnull<LocalModule>(Decl);
}

bool Sema::isClass(StringRef Name) {
  llvm::outs() << "Sema::isClass: " << Name << "\n";
  Declaration *Decl = CurrentScope->lookup(Name);
  return llvm::isa_and_nonnull<Class>(Decl);
}

void Sema::actOnImplementationModule(ImplementationModule *Mod,
                                     Identifier ModuleName,
                                     Expression *Protection,
                                     DeclarationList Decls, Block InitBlk,
                                     Block FinalBlk, bool IsProgramModule) {
  if (Mod->getName() != ModuleName.getName()) {
    Diags.report(ModuleName.getLoc(), diag::err_module_identifier_not_equal)
        << Mod->getName() << ModuleName.getName();
  }
  Mod->update(Protection, Decls, InitBlk, FinalBlk, IsProgramModule);
}

void Sema::actOnDefinitionModule(DefinitionModule *Mod, Identifier ModuleName,
                                 DeclarationList Decls) {
  if (Mod->getName() != ModuleName.getName()) {
    Diags.report(ModuleName.getLoc(), diag::err_module_identifier_not_equal)
        << Mod->getName() << ModuleName.getName();
  }
  Mod->update(Decls);
}

void Sema::actOnRefiningDefinitionModule(
    RefiningDefinitionModule *Mod, Identifier ModuleName,
    ActualParameterList ActualModulParams) {
  if (Mod->getName() != ModuleName.getName()) {
    Diags.report(ModuleName.getLoc(), diag::err_module_identifier_not_equal)
        << Mod->getName() << ModuleName.getName();
  }
  Mod->update(ActualModulParams);
}

void Sema::actOnRefiningImplementationModule(
    RefiningImplementationModule *Mod, Identifier ModuleName,
    ActualParameterList ActualModulParams) {
  if (Mod->getName() != ModuleName.getName()) {
    Diags.report(ModuleName.getLoc(), diag::err_module_identifier_not_equal)
        << Mod->getName() << ModuleName.getName();
  }
  Mod->update(ActualModulParams);
}

LocalModule *Sema::actOnLocalModule(Identifier ModuleName) {
  llvm::outs() << "actOnLocalModule\n";
  return LocalModule::create(CurrentDecl, ModuleName.getLoc(),
                             ModuleName.getName());
}

Procedure *Sema::actOnProcedure(Identifier ProcName) {
  Procedure *Proc =
      Procedure::create(CurrentDecl, ProcName.getLoc(), ProcName.getName());
  if (!CurrentScope->insert(Proc))
    Diags.report(ProcName.getLoc(), diag::err_symbol_already_declared)
        << ProcName.getName();
  return Proc;
}

void Sema::actOnProcedureHeading(DeclarationList &Decls, Procedure *Proc,
                                 FormalParameterList &Params,
                                 Type *ResultType) {
  Proc->update(Params, ResultType);
  Decls.push_back(Proc);
}

void Sema::actOnProcedure(Procedure *Proc, Identifier ProcName,
                          const DeclarationList &ProcDecls, Block Body,
                          bool IsFunction) {
  if (Proc->getName() != ProcName.getName()) {
    Diags.report(ProcName.getLoc(), diag::err_proc_identifier_not_equal)
        << Proc->getName() << ProcName.getName();
  }
  Proc->update(ProcDecls, Body);
}

void Sema::actOnForwardProcedure(DeclarationList &Decls, Procedure *Proc) {
  Proc->setForward();
  Decls.push_back(Proc);
}

void Sema::actOnConstant(DeclarationList &Decls, Identifier Name,
                         Expression *Expr) {
  llvm::outs() << "Sema::actOnConstant: Name = " << Name.getName() << "\n";
  Constant *Const = Constant::create(CurrentDecl, Name.getLoc(), Name.getName(),
                                     nullptr, Expr);
  if (!CurrentScope->insert(Const))
    Diags.report(Name.getLoc(), diag::err_symbol_already_declared)
        << Name.getName();
  Decls.push_back(Const);
}

void Sema::actOnType(DeclarationList &Decls, Identifier TypeName,
                     TypeDenoter *TyDen) {
  llvm::outs() << "Sema::actOnType: Name = " << TypeName.getName() << "\n";
  Type *Ty =
      Type::create(CurrentDecl, TypeName.getLoc(), TypeName.getName(), TyDen);
  if (!CurrentScope->insert(Ty))
    Diags.report(TypeName.getLoc(), diag::err_symbol_already_declared)
        << TypeName.getName();
  Decls.push_back(Ty);
}

void Sema::actOnVariable(DeclarationList &Decls,
                         VariableIdentifierList &VarIdList,
                         TypeDenoter *TyDen) {
  llvm::outs() << "Sema::actOnVariable\n";
  assert(CurrentScope && "CurrentScope not set");
  // if (Type *Ty = dyn_cast<TypeDeclaration>(D)) {
  for (auto I = VarIdList.begin(), E = VarIdList.end(); I != E; ++I) {
    Identifier Name = I->first;
    Expression *Addr = I->second;
    llvm::outs() << " -> Add variable " << Name.getName() << "\n";
    Variable *Var = Variable::create(CurrentDecl, Name.getLoc(), Name.getName(),
                                     TyDen, Addr);
    if (CurrentScope->insert(Var))
      Decls.push_back(Var);
    else
      Diags.report(Name.getLoc(), diag::err_symbol_already_declared)
          << Name.getName();
  }
  //} else if (!Ids.empty()) {
  //  SMLoc Loc = Ids.front().first;
  //  Diags.report(Loc, diag::err_vardecl_requires_type);
  //}
}

void Sema::actOnActualParameter(ActualParameterList &Params, Expression *Expr) {
  // An actual parameter can be an expression or a type.
  // TODO Evaluate later if this extraction is worth the trouble.
  ActualParameter Param = Expr;
  if (auto *Desig = llvm::dyn_cast_or_null<Designator>(Expr)) {
    if (llvm::isa_and_nonnull<Type>(Desig->getDecl()) &&
        Desig->getSelectorList().size() == 0) {
      Param = llvm::cast<Type>(Desig->getDecl());
      ;
      delete Expr;
    }
  }
  Params.push_back(Param);
}

void Sema::actOnFormalParameter(FormalParameterList &Params,
                                const IdentifierList &IdentList, bool IsVar,
                                const FormalType &FTy) {
  llvm::outs() << "Sema::actOnFormalParameter\n";
  Type *Ty = llvm::dyn_cast_or_null<Type>(FTy.getDecl());
  if (FTy.getDecl() && !Ty) {
    Diags.report(FTy.getDecl()->getLoc(), diag::err_type_expected);
  }
  unsigned OpenArrayLevel = FTy.getOpenArrayLevel();
  for (auto Id : IdentList) {
    FormalParameter *Param = FormalParameter::create(
        CurrentDecl, Id.getLoc(), Id.getName(), Ty, IsVar, OpenArrayLevel);
    if (!CurrentScope->insert(Param))
      Diags.report(Id.getLoc(), diag::err_symbol_already_declared)
          << Id.getName();
    Params.push_back(Param);
  }
}

Declaration *Sema::actOnModuleIdentifier(Declaration *ModDecl,
                                         Identifier Name) {
  if (ModDecl) {
    llvm_unreachable("Module lookup not yet implemented");
  }
  Declaration *Decl = CurrentScope->lookup(Name.getName());
  if (llvm::isa_and_nonnull<CompilationModule>(Decl) ||
      llvm::isa_and_nonnull<LocalModule>(Decl)) {
    return Decl;
  }
  Diags.report(Name.getLoc(), diag::err_symbol_not_declared) << Name.getName();
  return nullptr;
}

Declaration *Sema::actOnClassIdentifier(Declaration *ModDecl, Identifier Name) {
  llvm_unreachable("Module lookup not yet implemented");
}

Declaration *Sema::actOnQualifiedIdentifier(Declaration *ModOrClassDecl,
                                            Identifier Name) {
  llvm::outs() << "Sema::actOnQualifiedIdentifier: Name = " << Name.getName()
               << "\n";
  if (ModOrClassDecl) {
    llvm_unreachable("Module/class lookup not yet implemented");
  }
  Declaration *Decl = CurrentScope->lookup(Name.getName());
  if (!Decl) {
    Diags.report(Name.getLoc(), diag::err_symbol_not_declared)
        << Name.getName();
  }
  return Decl;
}

NamedType *Sema::actOnNamedType(SMLoc Loc, Declaration *Decl) {
  if (auto *TypeDecl = llvm::dyn_cast_or_null<Type>(Decl)) {
    return NamedType::create(TypeDecl);
  }
  return nullptr;
}

RecordType *Sema::actOnRecordType() { return nullptr; }

ArrayType *Sema::actOnArrayType(TypeDenoter *ComponentType,
                                const TypeDenoterList &IndexList) {
  // Check: Each entry of IndexList must be ordinal type identifier or
  // a new enumeration or subrange.
  return ArrayType::create(ComponentType, IndexList);
}

ProcedureType *Sema::actOnProcedureType(Type *ResultType) {
  return ProcedureType::create(ResultType);
}

PointerType *Sema::actOnPointerType(TypeDenoter *TyDen) {
  return PointerType::create(TyDen);
}

PointerType *Sema::actOnPointerType(const StringRef &Name) {
  return PointerType::create(Name);
}

SubrangeType *Sema::actOnSubrangeType(Declaration *Decl, Expression *From,
                                      Expression *To) {
  Type *Ty = llvm::dyn_cast_or_null<Type>(Decl);
  // TODO Ty must be ordinal type.
  if (Decl && !Ty) {
    // Emit error message
  }
  return SubrangeType::create(Ty, From, To);
}

EnumerationType *Sema::actOnEnumerationType() { return nullptr; }

SetType *Sema::actOnSetType(TypeDenoter *BaseType, bool IsPacked) {
  // Check: Base type must be ordinal type identifier or
  // a new enumeration or subrange.
  return SetType::create(BaseType, IsPacked);
}

Type *Sema::actOnTypeIdentifier(Declaration *TypeDecl) {
  if (auto *Ty = llvm::dyn_cast_or_null<Type>(TypeDecl)) {
    return Ty;
  }
  Diags.report(TypeDecl->getLoc(), diag::err_type_expected);
  return nullptr;
}

void Sema::actOnAssignmentStmt(StatementList &Stmts, Designator *Left,
                               Expression *Right) {
  AssignmentStatement *Stmt = AssignmentStatement::create(Left, Right);
  Stmts.push_back(Stmt);
}

void Sema::actOnProcedureCallStmt(StatementList &Stmts, Designator *Proc,
                                  const ActualParameterList &ActualParameters) {
  ProcedureCallStatement *Stmt =
      ProcedureCallStatement::create(Proc, ActualParameters);
  Stmts.push_back(Stmt);
}

void Sema::actOnIfStmt(StatementList &Stmts, Expression *Cond) {
  llvm::outs() << "actOnIfStmt\n";
}

void Sema::actOnCaseStmt(StatementList &Stmts) {
  llvm::outs() << "actOnCaseStmt\n";
}

void Sema::actOnWhileStmt(StatementList &Stmts, SMLoc Loc, Expression *Cond,
                          StatementList &WhileStmts) {
  llvm::outs() << "actOnWhileStmt\n";
  // type check
  // if (Cond->getType() != BooleanType) {}
  WhileStatement *Stmt = WhileStatement::create(Cond, WhileStmts, Loc);
  Stmts.push_back(Stmt);
}

void Sema::actOnRepeatStmt(StatementList &Stmts, SMLoc Loc, Expression *Cond,
                           StatementList &RepeatStmts) {
  llvm::outs() << "actOnRepeatStmt\n";
  RepeatStatement *Stmt = RepeatStatement::create(Cond, RepeatStmts, Loc);
  Stmts.push_back(Stmt);
}

void Sema::actOnLoopStmt(StatementList &Stmts, SMLoc Loc,
                         StatementList &LoopStmts) {
  llvm::outs() << "actOnLoopStmt\n";
  LoopStatement *Stmt = LoopStatement::create(LoopStmts, Loc);
  Stmts.push_back(Stmt);
}

void Sema::actOnForStmt(StatementList &Stmts, SMLoc Loc,
                        Identifier ControlVariable, Expression *InitialValue,
                        Expression *FinalValue, Expression *StepSize,
                        const StatementList &ForStmts) {
  llvm::outs() << "actOnForStmt\n";
  Declaration *Decl = CurrentScope->lookup(ControlVariable.getName());
  if (auto *Var = llvm::dyn_cast_or_null<Variable>(Decl)) {
    ForStatement *Stmt = ForStatement::create(Loc, Var, InitialValue,
                                              FinalValue, StepSize, ForStmts);
    Stmts.push_back(Stmt);
  }
  /* else error */
}

void Sema::actOnWithStmt(StatementList &Stmts, Designator *Desig,
                         StatementList &WithStmts) {
  llvm::outs() << "actOnWithStmt\n";
  WithStatement *Stmt = WithStatement::create();
  Stmts.push_back(Stmt);
}

void Sema::actOnExitStmt(StatementList &Stmts, SMLoc Loc) {
  llvm::outs() << "actOnExitStmt\n";
  ExitStatement *Stmt = ExitStatement::create(Loc);
  Stmts.push_back(Stmt);
}

void Sema::actOnReturnStmt(StatementList &Stmts, SMLoc Loc, Expression *E) {
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
      // Check if types are assignment compatible
    }
  }
  ReturnStatement *Stmt = ReturnStatement::create(E);
  Stmts.push_back(Stmt);
}

void Sema::actOnRetryStmt(StatementList &Stmts, SMLoc Loc) {
  llvm::outs() << "actOnRetryStmt\n";
  RetryStatement *Stmt = RetryStatement::create(Loc);
  Stmts.push_back(Stmt);
}

Expression *Sema::actOnExpression(Expression *Left, Expression *Right,
                                  const OperatorInfo &Op) {
  llvm::outs() << "actOnExpression\n";
  // Op is a relational operation.
  bool IsConst = Left && Right && Left->isConst() && Right->isConst();
  return InfixExpression::create(Left, Right, Op, nullptr, IsConst);
}

Expression *Sema::actOnSimpleExpression(Expression *Left, Expression *Right,
                                        const OperatorInfo &Op) {
  llvm::outs() << "actOnSimpleExpression\n";
  // Op is a term operation.
  bool IsConst = Left && Right && Left->isConst() && Right->isConst();
  return InfixExpression::create(Left, Right, Op, nullptr, IsConst);
}

Expression *Sema::actOnTerm(Expression *Left, Expression *Right,
                            const OperatorInfo &Op) {
  llvm::outs() << "actOnTerm\n";
  // Op is a factor operation.
  bool IsConst = Left && Right && Left->isConst() && Right->isConst();
  if (IsConst) {
  }
  return InfixExpression::create(Left, Right, Op, nullptr, IsConst);
}

Expression *Sema::actOnFactor(Expression *E, const OperatorInfo &Op) {
  llvm::outs() << "actOnFactor\n";
  return PrefixExpression::create(E, Op, nullptr, E->isConst());
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
  bool IsConst = E && E->isConst();
  return PrefixExpression::create(E, Op, nullptr, IsConst);
}

Expression *Sema::actOnIntegerLiteral(SMLoc Loc, StringRef LiteralData) {
  uint8_t Radix = 10;
  if (LiteralData.endswith("B") || LiteralData.endswith("H")) {
    Radix = LiteralData.endswith("B") ? 8 : 16;
    LiteralData = LiteralData.drop_back();
  }
  llvm::APInt Value(64, LiteralData, Radix);

  return IntegerLiteral::create(Value, nullptr);
}

Expression *Sema::actOnRealLiteral(SMLoc Loc, StringRef LiteralData) {
  // TODO Implement
  return nullptr;
}

Expression *Sema::actOnStringLiteral(SMLoc Loc, StringRef LiteralData) {
  // TODO Remove quotes
  return StringLiteral::create(LiteralData, nullptr);
}

Expression *Sema::actOnCharLiteral(SMLoc Loc, StringRef LiteralData) {
  // TODO Implement
  return nullptr;
}

Designator *Sema::actOnDesignator(Declaration *QualId,
                                  const SelectorList &Selectors) {
  // TODO Compute if value / or variable
  // TODO Compute const or not
  bool IsConst = false;
  bool IsVariable = false;
  TypeDenoter *TyDenot = nullptr;
  if (auto *Var = llvm::dyn_cast_or_null<Variable>(QualId)) {
    IsVariable = true;
    TyDenot = Var->getTypeDenoter();
  } else if (auto *Const = llvm::dyn_cast_or_null<Constant>(QualId)) {
    IsConst = true;
    TyDenot = Const->getTypeDenoter();
  } else if (auto *Proc = llvm::dyn_cast_or_null<Procedure>(QualId)) {
    // Something todo for a procedure?
    // Create TypeDenoter for procedure?
  } else {
    // TODO Emit error message.
  }
  return Designator::create(QualId, Selectors, TyDenot, IsVariable, IsConst);
}

Expression *
Sema::actOnFunctionCall(Expression *DesignatorExpr,
                        const ActualParameterList &ActualParameters) {
  if (auto *Func = llvm::dyn_cast_or_null<Designator>(DesignatorExpr)) {
    // TODO Check parameter list
    return FunctionCall::create(Func, ActualParameters, Func->getTypeDenoter(),
                                false);
  }
  // TODO Emit error message.
  return nullptr;
}

Expression *
Sema::actOnValueConstructor(Declaration *QualId /*, ConstructorValues */) {
  // TODO Implement
  return ValueConstructor::create(nullptr);
}
