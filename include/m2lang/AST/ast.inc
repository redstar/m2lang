#ifdef AST_DECLARATION
class Statement;
class Expression;
class TypeDenoter;

class Block {
  llvm::SmallVector<Statement *, 4> Stmts;
  llvm::SmallVector<Statement *, 4> ExceptStmts;

public:
  Block(const llvm::SmallVector<Statement *, 4> &Stmts, const llvm::SmallVector<Statement *, 4> &ExceptStmts)
    : Stmts(Stmts), ExceptStmts(ExceptStmts) {}

  const llvm::SmallVector<Statement *, 4> &getStmts() {
    return Stmts;
  }

  const llvm::SmallVector<Statement *, 4> &getExceptStmts() {
    return ExceptStmts;
  }
};

class Declaration {
  friend class CompilationModule;
  friend class ImplementationModule;
  friend class DefinitionModule;
  friend class RefiningDefinitionModule;
  friend class RefiningImplementationModule;
  friend class Type;
  friend class Constant;
  friend class Variable;
  friend class FormalParameter;
  friend class Procedure;
  friend class LocalModule;
  friend class Class;
protected:
  const unsigned __Kind;
private:
  Declaration * EnclosingDecl;
  llvm::SMLoc Loc;
  llvm::StringRef Name;

protected:
  Declaration(unsigned __Kind, Declaration * EnclosingDecl, llvm::SMLoc Loc, llvm::StringRef Name)
    : __Kind(__Kind), EnclosingDecl(EnclosingDecl), Loc(Loc), Name(Name) {}
public:

  Declaration * getEnclosingDecl() {
    return EnclosingDecl;
  }

  llvm::SMLoc getLoc() {
    return Loc;
  }

  llvm::StringRef getName() {
    return Name;
  }
};

class CompilationModule : public Declaration {
  friend class ImplementationModule;
  friend class DefinitionModule;
  friend class RefiningDefinitionModule;
  friend class RefiningImplementationModule;
protected:
  const unsigned __Kind;
  CompilationModule(__Kind, Declaration * EnclosingDecl, llvm::SMLoc Loc, llvm::StringRef Name)
    : Declaration(__Kind, EnclosingDecl, Loc, Name) {}
public:
  static bool classof(const Declaration* T) {
    return T->__Kind >= 0 && T->__Kind <= 3;
  }
};

class ImplementationModule : public CompilationModule {
  llvm::SmallVector<Declaration *, 4> Decls;
  Block InitBlk;
  Block FinalBlk;
  Expression * Protection;
  bool IsUnsafeGuarded;
  bool IsProgramModule;

public:
  ImplementationModule(Declaration * EnclosingDecl, llvm::SMLoc Loc, llvm::StringRef Name)
    : CompilationModule(0, EnclosingDecl, Loc, Name) {}

  llvm::SmallVector<Declaration *, 4> &getDecls() {
    return Decls;
  }

  void setDecls(const llvm::SmallVector<Declaration *, 4> &Decls) {
    this->Decls = Decls;
  }

  Block getInitBlk() {
    return InitBlk;
  }

  void setInitBlk(Block InitBlk) {
    this->InitBlk = InitBlk;
  }

  Block getFinalBlk() {
    return FinalBlk;
  }

  void setFinalBlk(Block FinalBlk) {
    this->FinalBlk = FinalBlk;
  }

  Expression * getProtection() {
    return Protection;
  }

  void setProtection(Expression * Protection) {
    this->Protection = Protection;
  }

  bool isIsUnsafeGuarded() {
    return IsUnsafeGuarded;
  }

  void setIsUnsafeGuarded(bool IsUnsafeGuarded) {
    this->IsUnsafeGuarded = IsUnsafeGuarded;
  }

  bool isIsProgramModule() {
    return IsProgramModule;
  }

  void setIsProgramModule(bool IsProgramModule) {
    this->IsProgramModule = IsProgramModule;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 0;
  }
};

class DefinitionModule : public CompilationModule {
  llvm::SmallVector<Declaration *, 4> Decls;
  bool IsUnsafeGuarded;

public:
  DefinitionModule(Declaration * EnclosingDecl, llvm::SMLoc Loc, llvm::StringRef Name)
    : CompilationModule(1, EnclosingDecl, Loc, Name) {}

  llvm::SmallVector<Declaration *, 4> &getDecls() {
    return Decls;
  }

  void setDecls(const llvm::SmallVector<Declaration *, 4> &Decls) {
    this->Decls = Decls;
  }

  bool isIsUnsafeGuarded() {
    return IsUnsafeGuarded;
  }

  void setIsUnsafeGuarded(bool IsUnsafeGuarded) {
    this->IsUnsafeGuarded = IsUnsafeGuarded;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 1;
  }
};

class RefiningDefinitionModule : public CompilationModule {
  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> ActualModulParams;
  bool IsUnsafeGuarded;

public:
  RefiningDefinitionModule(Declaration * EnclosingDecl, llvm::SMLoc Loc, llvm::StringRef Name)
    : CompilationModule(2, EnclosingDecl, Loc, Name) {}

  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &getActualModulParams() {
    return ActualModulParams;
  }

  void setActualModulParams(const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &ActualModulParams) {
    this->ActualModulParams = ActualModulParams;
  }

  bool isIsUnsafeGuarded() {
    return IsUnsafeGuarded;
  }

  void setIsUnsafeGuarded(bool IsUnsafeGuarded) {
    this->IsUnsafeGuarded = IsUnsafeGuarded;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 2;
  }
};

class RefiningImplementationModule : public CompilationModule {
  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> ActualModulParams;
  bool IsUnsafeGuarded;

public:
  RefiningImplementationModule(Declaration * EnclosingDecl, llvm::SMLoc Loc, llvm::StringRef Name)
    : CompilationModule(3, EnclosingDecl, Loc, Name) {}

  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &getActualModulParams() {
    return ActualModulParams;
  }

  void setActualModulParams(const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &ActualModulParams) {
    this->ActualModulParams = ActualModulParams;
  }

  bool isIsUnsafeGuarded() {
    return IsUnsafeGuarded;
  }

  void setIsUnsafeGuarded(bool IsUnsafeGuarded) {
    this->IsUnsafeGuarded = IsUnsafeGuarded;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 3;
  }
};

class Type : public Declaration {
  TypeDenoter * Denoter;

public:
  Type(Declaration * EnclosingDecl, llvm::SMLoc Loc, llvm::StringRef Name, TypeDenoter * Denoter)
    : Declaration(4, EnclosingDecl, Loc, Name), Denoter(Denoter) {}

  TypeDenoter * getDenoter() {
    return Denoter;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 4;
  }
};

class Constant : public Declaration {
  TypeDenoter * Denoter;
  Expression * ConstExpr;

public:
  Constant(Declaration * EnclosingDecl, llvm::SMLoc Loc, llvm::StringRef Name, TypeDenoter * Denoter, Expression * ConstExpr)
    : Declaration(5, EnclosingDecl, Loc, Name), Denoter(Denoter), ConstExpr(ConstExpr) {}

  TypeDenoter * getDenoter() {
    return Denoter;
  }

  Expression * getConstExpr() {
    return ConstExpr;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 5;
  }
};

class Variable : public Declaration {
  TypeDenoter * Denoter;
  Expression * Addr;

public:
  Variable(Declaration * EnclosingDecl, llvm::SMLoc Loc, llvm::StringRef Name, TypeDenoter * Denoter, Expression * Addr)
    : Declaration(6, EnclosingDecl, Loc, Name), Denoter(Denoter), Addr(Addr) {}

  TypeDenoter * getDenoter() {
    return Denoter;
  }

  Expression * getAddr() {
    return Addr;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 6;
  }
};

class FormalParameter : public Declaration {
  TypeDenoter * Denoter;
  bool IsCallByReference;

public:
  FormalParameter(Declaration * EnclosingDecl, llvm::SMLoc Loc, llvm::StringRef Name, TypeDenoter * Denoter, bool IsCallByReference)
    : Declaration(7, EnclosingDecl, Loc, Name), Denoter(Denoter), IsCallByReference(IsCallByReference) {}

  TypeDenoter * getDenoter() {
    return Denoter;
  }

  bool isIsCallByReference() {
    return IsCallByReference;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 7;
  }
};

class Procedure : public Declaration {
  llvm::SmallVector<FormalParameter *, 4> Params;
  Type * ResultType;
  llvm::SmallVector<Declaration *, 4> Decls;
  Block Body;
  bool IsForward;

public:
  Procedure(Declaration * EnclosingDecl, llvm::SMLoc Loc, llvm::StringRef Name)
    : Declaration(8, EnclosingDecl, Loc, Name) {}

  llvm::SmallVector<FormalParameter *, 4> &getParams() {
    return Params;
  }

  void setParams(const llvm::SmallVector<FormalParameter *, 4> &Params) {
    this->Params = Params;
  }

  Type * getResultType() {
    return ResultType;
  }

  void setResultType(Type * ResultType) {
    this->ResultType = ResultType;
  }

  llvm::SmallVector<Declaration *, 4> &getDecls() {
    return Decls;
  }

  void setDecls(const llvm::SmallVector<Declaration *, 4> &Decls) {
    this->Decls = Decls;
  }

  Block getBody() {
    return Body;
  }

  void setBody(Block Body) {
    this->Body = Body;
  }

  bool isIsForward() {
    return IsForward;
  }

  void setIsForward(bool IsForward) {
    this->IsForward = IsForward;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 8;
  }
};

class LocalModule : public Declaration {
public:
  LocalModule(Declaration * EnclosingDecl, llvm::SMLoc Loc, llvm::StringRef Name)
    : Declaration(9, EnclosingDecl, Loc, Name) {}
  static bool classof(const Declaration* T) {
    return T->__Kind == 9;
  }
};

class Class : public Declaration {
public:
  Class(Declaration * EnclosingDecl, llvm::SMLoc Loc, llvm::StringRef Name)
    : Declaration(10, EnclosingDecl, Loc, Name) {}
  static bool classof(const Declaration* T) {
    return T->__Kind == 10;
  }
};

class TypeDenoter {
  friend class PervasiveType;
  friend class RecordType;
  friend class ArrayType;
  friend class ProcedureType;
  friend class OpenArrayFormalType;
  friend class PointerType;
  friend class SubrangeType;
  friend class EnumerationType;
  friend class SetType;
protected:
  const unsigned __Kind;
  TypeDenoter(unsigned __Kind)
    : __Kind(__Kind) {}
public:
};

class PervasiveType : public TypeDenoter {
  pervasive::PervasiveTypeKind TypeKind;

public:
  PervasiveType(pervasive::PervasiveTypeKind TypeKind)
    : TypeDenoter(0), TypeKind(TypeKind) {}

  pervasive::PervasiveTypeKind getTypeKind() {
    return TypeKind;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 0;
  }
};

class FixedRecordField {
  llvm::StringRef Name;
  TypeDenoter * TyDe;

public:
  FixedRecordField(llvm::StringRef Name, TypeDenoter * TyDe)
    : Name(Name), TyDe(TyDe) {}

  llvm::StringRef getName() {
    return Name;
  }

  TypeDenoter * getTyDe() {
    return TyDe;
  }
};

class RecordType : public TypeDenoter {
  llvm::SmallVector<FixedRecordField, 4> Fields;

public:
  RecordType(const llvm::SmallVector<FixedRecordField, 4> &Fields)
    : TypeDenoter(1), Fields(Fields) {}

  const llvm::SmallVector<FixedRecordField, 4> &getFields() {
    return Fields;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 1;
  }
};

class ArrayType : public TypeDenoter {
  TypeDenoter * ComponentType;
  TypeDenoter * IndexType;

public:
  ArrayType(TypeDenoter * ComponentType, TypeDenoter * IndexType)
    : TypeDenoter(2), ComponentType(ComponentType), IndexType(IndexType) {}

  TypeDenoter * getComponentType() {
    return ComponentType;
  }

  TypeDenoter * getIndexType() {
    return IndexType;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 2;
  }
};

class FormalParameterType {
  llvm::SMLoc Loc;
  TypeDenoter * FormalType;
  bool IsCallByReference;

public:
  FormalParameterType(llvm::SMLoc Loc, TypeDenoter * FormalType, bool IsCallByReference)
    : Loc(Loc), FormalType(FormalType), IsCallByReference(IsCallByReference) {}

  llvm::SMLoc getLoc() {
    return Loc;
  }

  TypeDenoter * getFormalType() {
    return FormalType;
  }

  bool isIsCallByReference() {
    return IsCallByReference;
  }
};

class ProcedureType : public TypeDenoter {
  Type * ResultType;
  llvm::SmallVector<FormalParameterType, 4> ParameterTypes;

public:
  ProcedureType(Type * ResultType, const llvm::SmallVector<FormalParameterType, 4> &ParameterTypes)
    : TypeDenoter(3), ResultType(ResultType), ParameterTypes(ParameterTypes) {}

  Type * getResultType() {
    return ResultType;
  }

  const llvm::SmallVector<FormalParameterType, 4> &getParameterTypes() {
    return ParameterTypes;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 3;
  }
};

class OpenArrayFormalType : public TypeDenoter {
  TypeDenoter * ComponentType;

public:
  OpenArrayFormalType(TypeDenoter * ComponentType)
    : TypeDenoter(4), ComponentType(ComponentType) {}

  TypeDenoter * getComponentType() {
    return ComponentType;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 4;
  }
};

class PointerType : public TypeDenoter {
  TypeDenoter * TyDen;
  llvm::StringRef Name;
  bool IsResolved;

public:
  PointerType()
    : TypeDenoter(5) {}

  TypeDenoter * getTyDen() {
    return TyDen;
  }

  void setTyDen(TypeDenoter * TyDen) {
    this->TyDen = TyDen;
  }

  llvm::StringRef getName() {
    return Name;
  }

  void setName(llvm::StringRef Name) {
    this->Name = Name;
  }

  bool isIsResolved() {
    return IsResolved;
  }

  void setIsResolved(bool IsResolved) {
    this->IsResolved = IsResolved;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 5;
  }
};

class SubrangeType : public TypeDenoter {
  Type * RangeType;
  Expression * From;
  Expression * To;

public:
  SubrangeType(Type * RangeType, Expression * From, Expression * To)
    : TypeDenoter(6), RangeType(RangeType), From(From), To(To) {}

  Type * getRangeType() {
    return RangeType;
  }

  Expression * getFrom() {
    return From;
  }

  Expression * getTo() {
    return To;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 6;
  }
};

class EnumerationType : public TypeDenoter {
  llvm::SmallVector<Constant *, 4> Members;

public:
  EnumerationType(const llvm::SmallVector<Constant *, 4> &Members)
    : TypeDenoter(7), Members(Members) {}

  const llvm::SmallVector<Constant *, 4> &getMembers() {
    return Members;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 7;
  }
};

class SetType : public TypeDenoter {
  Constant * BaseType;
  bool IsPacked;

public:
  SetType(Constant * BaseType, bool IsPacked)
    : TypeDenoter(8), BaseType(BaseType), IsPacked(IsPacked) {}

  Constant * getBaseType() {
    return BaseType;
  }

  bool isIsPacked() {
    return IsPacked;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 8;
  }
};

class OperatorInfo {
  llvm::SMLoc Loc;
  unsigned Kind;
  unsigned IsUnspecified;

public:
  OperatorInfo() {}

  llvm::SMLoc getLoc() {
    return Loc;
  }

  void setLoc(llvm::SMLoc Loc) {
    this->Loc = Loc;
  }

  unsigned getKind() {
    return Kind;
  }

  void setKind(unsigned Kind) {
    this->Kind = Kind;
  }

  unsigned getIsUnspecified() {
    return IsUnspecified;
  }

  void setIsUnspecified(unsigned IsUnspecified) {
    this->IsUnspecified = IsUnspecified;
  }
};

class Expression {
  friend class InfixExpression;
  friend class PrefixExpression;
  friend class IntegerLiteral;
  friend class RealLiteral;
  friend class StringLiteral;
  friend class CharLiteral;
  friend class BooleanLiteral;
  friend class NilValue;
  friend class Designator;
  friend class FunctionCall;
  friend class ValueConstructor;
protected:
  const unsigned __Kind;
private:
  TypeDenoter * Denoter;
  bool IsConst;

protected:
  Expression(unsigned __Kind, TypeDenoter * Denoter, bool IsConst)
    : __Kind(__Kind), Denoter(Denoter), IsConst(IsConst) {}
public:

  TypeDenoter * getDenoter() {
    return Denoter;
  }

  bool isIsConst() {
    return IsConst;
  }
};

class InfixExpression : public Expression {
  Expression * Left;
  Expression * Right;
  OperatorInfo Op;

public:
  InfixExpression(TypeDenoter * Denoter, bool IsConst, Expression * Left, Expression * Right, OperatorInfo Op)
    : Expression(0, Denoter, IsConst), Left(Left), Right(Right), Op(Op) {}

  Expression * getLeft() {
    return Left;
  }

  Expression * getRight() {
    return Right;
  }

  OperatorInfo getOp() {
    return Op;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 0;
  }
};

class PrefixExpression : public Expression {
  Expression * E;
  OperatorInfo Op;

public:
  PrefixExpression(TypeDenoter * Denoter, bool IsConst, Expression * E, OperatorInfo Op)
    : Expression(1, Denoter, IsConst), E(E), Op(Op) {}

  Expression * getE() {
    return E;
  }

  OperatorInfo getOp() {
    return Op;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 1;
  }
};

class IntegerLiteral : public Expression {
  llvm::APInt Value;

public:
  IntegerLiteral(TypeDenoter * Denoter, bool IsConst, llvm::APInt Value)
    : Expression(2, Denoter, IsConst), Value(Value) {}

  llvm::APInt getValue() {
    return Value;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 2;
  }
};

class RealLiteral : public Expression {
  llvm::APFloat Value;

public:
  RealLiteral(TypeDenoter * Denoter, bool IsConst, llvm::APFloat Value)
    : Expression(3, Denoter, IsConst), Value(Value) {}

  llvm::APFloat getValue() {
    return Value;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 3;
  }
};

class StringLiteral : public Expression {
  llvm::StringRef Value;

public:
  StringLiteral(TypeDenoter * Denoter, bool IsConst, llvm::StringRef Value)
    : Expression(4, Denoter, IsConst), Value(Value) {}

  llvm::StringRef getValue() {
    return Value;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 4;
  }
};

class CharLiteral : public Expression {
  unsigned Value;

public:
  CharLiteral(TypeDenoter * Denoter, bool IsConst, unsigned Value)
    : Expression(5, Denoter, IsConst), Value(Value) {}

  unsigned getValue() {
    return Value;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 5;
  }
};

class BooleanLiteral : public Expression {
  bool Value;

public:
  BooleanLiteral(TypeDenoter * Denoter, bool IsConst, bool Value)
    : Expression(6, Denoter, IsConst), Value(Value) {}

  bool isValue() {
    return Value;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 6;
  }
};

class NilValue : public Expression {
public:
  NilValue(TypeDenoter * Denoter, bool IsConst)
    : Expression(7, Denoter, IsConst) {}
  static bool classof(const Expression* T) {
    return T->__Kind == 7;
  }
};

class Selector {
  friend class IndexSelector;
  friend class FieldSelector;
  friend class DereferenceSelector;
protected:
  const unsigned __Kind;
private:
  TypeDenoter * TyDe;

protected:
  Selector(unsigned __Kind, TypeDenoter * TyDe)
    : __Kind(__Kind), TyDe(TyDe) {}
public:

  TypeDenoter * getTyDe() {
    return TyDe;
  }
};

class IndexSelector : public Selector {
  Expression * Index;

public:
  IndexSelector(TypeDenoter * TyDe, Expression * Index)
    : Selector(0, TyDe), Index(Index) {}

  Expression * getIndex() {
    return Index;
  }
  static bool classof(const Selector* T) {
    return T->__Kind == 0;
  }
};

class FieldSelector : public Selector {
public:
  FieldSelector(TypeDenoter * TyDe)
    : Selector(1, TyDe) {}
  static bool classof(const Selector* T) {
    return T->__Kind == 1;
  }
};

class DereferenceSelector : public Selector {
public:
  DereferenceSelector(TypeDenoter * TyDe)
    : Selector(2, TyDe) {}
  static bool classof(const Selector* T) {
    return T->__Kind == 2;
  }
};

class Designator : public Expression {
  Declaration * Decl;
  llvm::SmallVector<Selector *, 4> Selectors;
  bool IsReference;

public:
  Designator(TypeDenoter * Denoter, bool IsConst, Declaration * Decl, const llvm::SmallVector<Selector *, 4> &Selectors, bool IsReference)
    : Expression(8, Denoter, IsConst), Decl(Decl), Selectors(Selectors), IsReference(IsReference) {}

  Declaration * getDecl() {
    return Decl;
  }

  const llvm::SmallVector<Selector *, 4> &getSelectors() {
    return Selectors;
  }

  bool isIsReference() {
    return IsReference;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 8;
  }
};

class FunctionCall : public Expression {
  Designator * Desig;
  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> ActualParameters;

public:
  FunctionCall(TypeDenoter * Denoter, bool IsConst, Designator * Desig, const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &ActualParameters)
    : Expression(9, Denoter, IsConst), Desig(Desig), ActualParameters(ActualParameters) {}

  Designator * getDesig() {
    return Desig;
  }

  const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &getActualParameters() {
    return ActualParameters;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 9;
  }
};

class ValueConstructor : public Expression {
public:
  ValueConstructor(TypeDenoter * Denoter, bool IsConst)
    : Expression(10, Denoter, IsConst) {}
  static bool classof(const Expression* T) {
    return T->__Kind == 10;
  }
};

class Statement {
  friend class AssignmentStatement;
  friend class ProcedureCallStatement;
  friend class IfStatement;
  friend class CaseStatement;
  friend class WhileStatement;
  friend class RepeatStatement;
  friend class ForStatement;
  friend class LoopStatement;
  friend class WithStatement;
  friend class ExitStatement;
  friend class ReturnStatement;
  friend class RetryStatement;
protected:
  const unsigned __Kind;
private:
  llvm::SMLoc Loc;

protected:
  Statement(unsigned __Kind, llvm::SMLoc Loc)
    : __Kind(__Kind), Loc(Loc) {}
public:

  llvm::SMLoc getLoc() {
    return Loc;
  }
};

class AssignmentStatement : public Statement {
  Designator * Left;
  Expression * Right;

public:
  AssignmentStatement(llvm::SMLoc Loc, Designator * Left, Expression * Right)
    : Statement(0, Loc), Left(Left), Right(Right) {}

  Designator * getLeft() {
    return Left;
  }

  Expression * getRight() {
    return Right;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 0;
  }
};

class ProcedureCallStatement : public Statement {
  Designator * Proc;
  llvm::PointerUnion<Expression *, Type *> ActualParameters;

public:
  ProcedureCallStatement(llvm::SMLoc Loc, Designator * Proc, llvm::PointerUnion<Expression *, Type *> ActualParameters)
    : Statement(1, Loc), Proc(Proc), ActualParameters(ActualParameters) {}

  Designator * getProc() {
    return Proc;
  }

  llvm::PointerUnion<Expression *, Type *> getActualParameters() {
    return ActualParameters;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 1;
  }
};

class IfStatement : public Statement {
  Expression * Cond;
  llvm::SmallVector<Statement *, 4> Stmts;

public:
  IfStatement(llvm::SMLoc Loc, Expression * Cond, const llvm::SmallVector<Statement *, 4> &Stmts)
    : Statement(2, Loc), Cond(Cond), Stmts(Stmts) {}

  Expression * getCond() {
    return Cond;
  }

  const llvm::SmallVector<Statement *, 4> &getStmts() {
    return Stmts;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 2;
  }
};

class CaseStatement : public Statement {
public:
  CaseStatement(llvm::SMLoc Loc)
    : Statement(3, Loc) {}
  static bool classof(const Statement* T) {
    return T->__Kind == 3;
  }
};

class WhileStatement : public Statement {
  Expression * Cond;
  llvm::SmallVector<Statement *, 4> Stmts;

public:
  WhileStatement(llvm::SMLoc Loc, Expression * Cond, const llvm::SmallVector<Statement *, 4> &Stmts)
    : Statement(4, Loc), Cond(Cond), Stmts(Stmts) {}

  Expression * getCond() {
    return Cond;
  }

  const llvm::SmallVector<Statement *, 4> &getStmts() {
    return Stmts;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 4;
  }
};

class RepeatStatement : public Statement {
  Expression * Cond;
  llvm::SmallVector<Statement *, 4> Stmts;

public:
  RepeatStatement(llvm::SMLoc Loc, Expression * Cond, const llvm::SmallVector<Statement *, 4> &Stmts)
    : Statement(5, Loc), Cond(Cond), Stmts(Stmts) {}

  Expression * getCond() {
    return Cond;
  }

  const llvm::SmallVector<Statement *, 4> &getStmts() {
    return Stmts;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 5;
  }
};

class ForStatement : public Statement {
  Variable * ControlVariable;
  Expression * InitialValue;
  Expression * FinalValue;
  Expression * StepSize;
  llvm::SmallVector<Statement *, 4> ForStmts;

public:
  ForStatement(llvm::SMLoc Loc, Variable * ControlVariable, Expression * InitialValue, Expression * FinalValue, Expression * StepSize, const llvm::SmallVector<Statement *, 4> &ForStmts)
    : Statement(6, Loc), ControlVariable(ControlVariable), InitialValue(InitialValue), FinalValue(FinalValue), StepSize(StepSize), ForStmts(ForStmts) {}

  Variable * getControlVariable() {
    return ControlVariable;
  }

  Expression * getInitialValue() {
    return InitialValue;
  }

  Expression * getFinalValue() {
    return FinalValue;
  }

  Expression * getStepSize() {
    return StepSize;
  }

  const llvm::SmallVector<Statement *, 4> &getForStmts() {
    return ForStmts;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 6;
  }
};

class LoopStatement : public Statement {
  llvm::SmallVector<Statement *, 4> Stmts;

public:
  LoopStatement(llvm::SMLoc Loc, const llvm::SmallVector<Statement *, 4> &Stmts)
    : Statement(7, Loc), Stmts(Stmts) {}

  const llvm::SmallVector<Statement *, 4> &getStmts() {
    return Stmts;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 7;
  }
};

class WithStatement : public Statement {
  llvm::SmallVector<Statement *, 4> Stmts;

public:
  WithStatement(llvm::SMLoc Loc, const llvm::SmallVector<Statement *, 4> &Stmts)
    : Statement(8, Loc), Stmts(Stmts) {}

  const llvm::SmallVector<Statement *, 4> &getStmts() {
    return Stmts;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 8;
  }
};

class ExitStatement : public Statement {
public:
  ExitStatement(llvm::SMLoc Loc)
    : Statement(9, Loc) {}
  static bool classof(const Statement* T) {
    return T->__Kind == 9;
  }
};

class ReturnStatement : public Statement {
  Expression * RetVal;

public:
  ReturnStatement(llvm::SMLoc Loc, Expression * RetVal)
    : Statement(10, Loc), RetVal(RetVal) {}

  Expression * getRetVal() {
    return RetVal;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 10;
  }
};

class RetryStatement : public Statement {
public:
  RetryStatement(llvm::SMLoc Loc)
    : Statement(11, Loc) {}
  static bool classof(const Statement* T) {
    return T->__Kind == 11;
  }
};
#endif
#ifdef AST_DEFINITION
#endif
