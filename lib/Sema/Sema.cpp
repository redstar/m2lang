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
  IntegerType = Type::create(CurrentDecl, SMLoc(), "INTEGER", nullptr);
  CardinalType = Type::create(CurrentDecl, SMLoc(), "CARDINAL", nullptr);
  BooleanType = Type::create(CurrentDecl, SMLoc(), "BOOLEAN", nullptr);
  TrueLiteral = BooleanLiteral::create(true);
  FalseLiteral = BooleanLiteral::create(false);
  TrueConst =
      Constant::create(CurrentDecl, SMLoc(), "TRUE", BooleanType, TrueLiteral);
  FalseConst = Constant::create(CurrentDecl, SMLoc(), "FALSE", BooleanType,
                                FalseLiteral);
  CurrentScope->insert(IntegerType);
  CurrentScope->insert(CardinalType);
  CurrentScope->insert(BooleanType);
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

ProgramModule *Sema::actOnProgramModule(Identifier ModuleName) {
  //
  return ProgramModule::create(CurrentDecl, ModuleName.getLoc(),
                               ModuleName.getName());
}

void Sema::actOnProgramModule(ProgramModule *Mod, Identifier ModuleName,
                              DeclarationList Decls, Block InitBlk,
                              Block FinalBlk) {
  if (Mod->getName() != ModuleName.getName()) {
    Diags.report(ModuleName.getLoc(), diag::err_module_identifier_not_equal)
        << Mod->getName() << ModuleName.getName();
  }
  Mod->update(Decls, InitBlk, FinalBlk);
}

LocalModule *Sema::actOnLocalModule(Identifier ModuleName) {
  llvm::outs() << "actOnLocalModule\n";
  return LocalModule::create(CurrentDecl, ModuleName.getLoc(),
                             ModuleName.getName());
}

Procedure *Sema::actOnProcedure(Identifier ProcName) {
  llvm::outs() << "actOnProcedure\n";
  Procedure *Proc =
      Procedure::create(CurrentDecl, ProcName.getLoc(), ProcName.getName());
  if (!CurrentScope->insert(Proc))
    Diags.report(ProcName.getLoc(), diag::err_symbol_already_declared)
        << ProcName.getName();
  return Proc;
}

void Sema::actOnProcedure(Procedure *Proc, Identifier ProcName,
                          FormalParameterList Params, Declaration *ResultType,
                          DeclarationList Decls, Block Body, bool IsFunction) {
  if (Proc->getName() != ProcName.getName()) {
    Diags.report(ProcName.getLoc(), diag::err_proc_identifier_not_equal)
        << Proc->getName() << ProcName.getName();
  }
  Type *Ty = nullptr;
  if (auto *T = llvm::dyn_cast_or_null<Type>(ResultType))
    Ty = T;
  Proc->update(Params, Ty, Decls, Body);
}

void Sema::actOnForwardProcedure(Procedure *Proc) { Proc->setForward(); }

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

void Sema::actOnFormalParameter(FormalParameterList Params,
                                IdentifierList IdentList, bool IsVar,
                                Type *Ty) {
  llvm::outs() << "Sema::actOnFormalParameter\n";
  for (auto Id : IdentList) {
    FormalParameter *Param = FormalParameter::create(CurrentDecl, Id.getLoc(),
                                                     Id.getName(), Ty, IsVar);
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

void Sema::actOnAssignmentStmt(StatementList &Stmts, Designator *Left,
                               Expression *Right) {
  AssignmentStatement *Stmt = AssignmentStatement::create(Left, Right);
  Stmts.push_back(Stmt);
}

void Sema::actOnProcedureCallStmt(StatementList &Stmts, Designator *Proc,
                                  const ExpressionList &ActualParameters) {
  ProcedureCallStatement *Stmt =
      ProcedureCallStatement::create(Proc, ActualParameters);
  Stmts.push_back(Stmt);
}

Statement *Sema::actOnIfStmt(Expression *Cond) {
  llvm::outs() << "actOnIfStmt\n";
  return IfStatement::create(Cond);
}

Statement *Sema::actOnCaseStmt() {
  llvm::outs() << "actOnCaseStmt\n";
  return nullptr;
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

Statement *Sema::actOnForStmt() {
  llvm::outs() << "actOnForStmt\n";
  return nullptr;
}

Statement *Sema::actOnWithStmt() {
  llvm::outs() << "actOnWithStmt\n";
  return nullptr;
}

void Sema::actOnExitStmt(StatementList &Stmts, SMLoc Loc) {
  llvm::outs() << "actOnExitStmt\n";
  ExitStatement *Stmt = ExitStatement::create(Loc);
  Stmts.push_back(Stmt);
}

void Sema::actOnReturnStmt(StatementList &Stmts, Expression *E) {
  llvm::outs() << "actOnReturnStmt\n";
  // Check if enclosing procedure is a function.
  ReturnStatement *Stmt = ReturnStatement::create(E);
  Stmts.push_back(Stmt);
}

Statement *Sema::actOnRetryStmt(SMLoc Loc) {
  llvm::outs() << "actOnRetryStmt\n";
  return RetryStatement::create(Loc);
}

void Sema::actOnConstantExpression() {
  llvm::outs() << "actOnConstantExpression\n";
}

Expression *Sema::actOnExpression(Expression *Left, Expression *Right,
                                  const OperatorInfo &Op) {
  llvm::outs() << "actOnExpression\n";
  // Op is a relational operation.
  bool IsConst = Left && Right && Left->isConst() && Right->isConst();
  return InfixExpression::create(Left, Right, Op, IsConst);
}

Expression *Sema::actOnSimpleExpression(Expression *Left, Expression *Right,
                                        const OperatorInfo &Op) {
  llvm::outs() << "actOnSimpleExpression\n";
  // Op is a term operation.
  bool IsConst = Left && Right && Left->isConst() && Right->isConst();
  return InfixExpression::create(Left, Right, Op, IsConst);
}

Expression *Sema::actOnTerm(Expression *Left, Expression *Right,
                            const OperatorInfo &Op) {
  llvm::outs() << "actOnTerm\n";
  // Op is a factor operation.
  bool IsConst = Left && Right && Left->isConst() && Right->isConst();
  if (IsConst) {
  }
  return InfixExpression::create(Left, Right, Op, IsConst);
}

Expression *Sema::actOnFactor(Expression *E, const OperatorInfo &Op) {
  llvm::outs() << "actOnFactor\n";
  return PrefixExpression::create(E, Op, E->isConst());
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
  return PrefixExpression::create(E, Op, IsConst);
}

Expression *Sema::actOnIntegerLiteral(SMLoc Loc, StringRef LiteralData) {
  uint8_t Radix = 10;
  if (LiteralData.endswith("B") || LiteralData.endswith("H")) {
    Radix = LiteralData.endswith("B") ? 8 : 16;
    LiteralData = LiteralData.drop_back();
  }
  llvm::APInt Value(64, LiteralData, Radix);

  return IntegerLiteral::create(Value);
}

Expression *Sema::actOnRealLiteral(SMLoc Loc, StringRef LiteralData) {
  // TODO Implement
  return nullptr;
}

Expression *Sema::actOnStringLiteral(SMLoc Loc, StringRef LiteralData) {
  // TODO Remove quotes
  return StringLiteral::create(LiteralData);
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
  if (auto *Var = llvm::dyn_cast_or_null<Variable>(QualId)) {
    IsVariable = true;
  }
  else if (auto *Const = llvm::dyn_cast_or_null<Constant>(QualId)) {
    IsConst = true;
  }
  else if (auto *Proc = llvm::dyn_cast_or_null<Procedure>(QualId)) {
    // Something todo for a procedure?
  }
  else {
    // TODO Emit error message.
  }
  return Designator::create(QualId, Selectors, IsVariable, IsConst);
}

Expression *Sema::actOnFunctionCall(Expression *DesignatorExpr,
                                    const ExpressionList &ActualParameters) {
  if (auto *Func = llvm::dyn_cast_or_null<Designator>(DesignatorExpr)) {
    // TODO Check parameter list
    return FunctionCall::create(Func, ActualParameters, false);
  }
  // TODO Emit error message.
  return nullptr;
}

Expression *
Sema::actOnValueConstructor(Declaration *QualId /*, ConstructorValues */) {
  // TODO Implement
  return ValueConstructor::create(false);
}
