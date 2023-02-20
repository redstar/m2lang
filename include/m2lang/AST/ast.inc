#ifdef AST_DECLARATION
#undef AST_DECLARATION
class Statement;
class Expression;
class TypeDenoter;

class Block {
  llvm::SmallVector<Statement *, 4> _Stmts;
  llvm::SmallVector<Statement *, 4> _ExceptStmts;

public:
  Block(const llvm::SmallVector<Statement *, 4> &_Stmts, const llvm::SmallVector<Statement *, 4> &_ExceptStmts)
    : _Stmts(_Stmts), _ExceptStmts(_ExceptStmts) {}
  Block() = default;

  const llvm::SmallVector<Statement *, 4> &getStmts() {
    return _Stmts;
  }

  const llvm::SmallVector<Statement *, 4> &getExceptStmts() {
    return _ExceptStmts;
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
  Declaration * _EnclosingDecl;
  llvm::SMLoc _Loc;
  llvm::StringRef _Name;

protected:
  Declaration(unsigned __Kind, Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name)
    : __Kind(__Kind), _EnclosingDecl(_EnclosingDecl), _Loc(_Loc), _Name(_Name) {}
public:

  Declaration * getEnclosingDecl() {
    return _EnclosingDecl;
  }

  llvm::SMLoc getLoc() {
    return _Loc;
  }

  llvm::StringRef getName() {
    return _Name;
  }
};

class CompilationModule : public Declaration {
protected:
  CompilationModule(unsigned __Kind, Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name)
    : Declaration(__Kind, _EnclosingDecl, _Loc, _Name) {}
public:
  static bool classof(const Declaration* T) {
    return T->__Kind >= 0 && T->__Kind <= 3;
  }
};

class ImplementationModule : public CompilationModule {
  llvm::SmallVector<Declaration *, 4> _Decls;
  Block _InitBlk;
  Block _FinalBlk;
  Expression * _Protection;
  bool _UnsafeGuarded;
  bool _ProgramModule;

public:
  ImplementationModule(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name, bool _UnsafeGuarded)
    : CompilationModule(0, _EnclosingDecl, _Loc, _Name), _UnsafeGuarded(_UnsafeGuarded) {}

  llvm::SmallVector<Declaration *, 4> &getDecls() {
    return _Decls;
  }

  void setDecls(const llvm::SmallVector<Declaration *, 4> &_Decls) {
    this->_Decls = _Decls;
  }

  Block getInitBlk() {
    return _InitBlk;
  }

  void setInitBlk(Block _InitBlk) {
    this->_InitBlk = _InitBlk;
  }

  Block getFinalBlk() {
    return _FinalBlk;
  }

  void setFinalBlk(Block _FinalBlk) {
    this->_FinalBlk = _FinalBlk;
  }

  Expression * getProtection() {
    return _Protection;
  }

  void setProtection(Expression * _Protection) {
    this->_Protection = _Protection;
  }

  bool isUnsafeGuarded() {
    return _UnsafeGuarded;
  }

  bool isProgramModule() {
    return _ProgramModule;
  }

  void setProgramModule(bool _ProgramModule) {
    this->_ProgramModule = _ProgramModule;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 0;
  }
};

class DefinitionModule : public CompilationModule {
  llvm::SmallVector<Declaration *, 4> _Decls;
  bool _UnsafeGuarded;

public:
  DefinitionModule(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name, bool _UnsafeGuarded)
    : CompilationModule(1, _EnclosingDecl, _Loc, _Name), _UnsafeGuarded(_UnsafeGuarded) {}

  llvm::SmallVector<Declaration *, 4> &getDecls() {
    return _Decls;
  }

  void setDecls(const llvm::SmallVector<Declaration *, 4> &_Decls) {
    this->_Decls = _Decls;
  }

  bool isUnsafeGuarded() {
    return _UnsafeGuarded;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 1;
  }
};

class RefiningDefinitionModule : public CompilationModule {
  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> _ActualModulParams;
  bool _UnsafeGuarded;

public:
  RefiningDefinitionModule(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name, bool _UnsafeGuarded)
    : CompilationModule(2, _EnclosingDecl, _Loc, _Name), _UnsafeGuarded(_UnsafeGuarded) {}

  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &getActualModulParams() {
    return _ActualModulParams;
  }

  void setActualModulParams(const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &_ActualModulParams) {
    this->_ActualModulParams = _ActualModulParams;
  }

  bool isUnsafeGuarded() {
    return _UnsafeGuarded;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 2;
  }
};

class RefiningImplementationModule : public CompilationModule {
  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> _ActualModulParams;
  bool _UnsafeGuarded;

public:
  RefiningImplementationModule(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name, bool _UnsafeGuarded)
    : CompilationModule(3, _EnclosingDecl, _Loc, _Name), _UnsafeGuarded(_UnsafeGuarded) {}

  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &getActualModulParams() {
    return _ActualModulParams;
  }

  void setActualModulParams(const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &_ActualModulParams) {
    this->_ActualModulParams = _ActualModulParams;
  }

  bool isUnsafeGuarded() {
    return _UnsafeGuarded;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 3;
  }
};

class Type : public Declaration {
  TypeDenoter * _TypeDenoter;

public:
  Type(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name, TypeDenoter * _TypeDenoter)
    : Declaration(4, _EnclosingDecl, _Loc, _Name), _TypeDenoter(_TypeDenoter) {}

  TypeDenoter * getTypeDenoter() {
    return _TypeDenoter;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 4;
  }
};

class Constant : public Declaration {
  TypeDenoter * _TypeDenoter;
  Expression * _ConstExpr;

public:
  Constant(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name, TypeDenoter * _TypeDenoter, Expression * _ConstExpr)
    : Declaration(5, _EnclosingDecl, _Loc, _Name), _TypeDenoter(_TypeDenoter), _ConstExpr(_ConstExpr) {}

  TypeDenoter * getTypeDenoter() {
    return _TypeDenoter;
  }

  Expression * getConstExpr() {
    return _ConstExpr;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 5;
  }
};

class Variable : public Declaration {
  TypeDenoter * _TypeDenoter;
  Expression * _Addr;

public:
  Variable(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name, TypeDenoter * _TypeDenoter, Expression * _Addr)
    : Declaration(6, _EnclosingDecl, _Loc, _Name), _TypeDenoter(_TypeDenoter), _Addr(_Addr) {}

  TypeDenoter * getTypeDenoter() {
    return _TypeDenoter;
  }

  Expression * getAddr() {
    return _Addr;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 6;
  }
};

class FormalParameter : public Declaration {
  TypeDenoter * _Type;
  bool _CallByReference;

public:
  FormalParameter(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name, TypeDenoter * _Type, bool _CallByReference)
    : Declaration(7, _EnclosingDecl, _Loc, _Name), _Type(_Type), _CallByReference(_CallByReference) {}

  TypeDenoter * getType() {
    return _Type;
  }

  bool isCallByReference() {
    return _CallByReference;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 7;
  }
};

class Procedure : public Declaration {
  llvm::SmallVector<FormalParameter *, 4> _Params;
  Type * _ResultType;
  llvm::SmallVector<Declaration *, 4> _Decls;
  Block _Body;
  bool _Forward;

public:
  Procedure(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name)
    : Declaration(8, _EnclosingDecl, _Loc, _Name) {}

  llvm::SmallVector<FormalParameter *, 4> &getParams() {
    return _Params;
  }

  void setParams(const llvm::SmallVector<FormalParameter *, 4> &_Params) {
    this->_Params = _Params;
  }

  Type * getResultType() {
    return _ResultType;
  }

  void setResultType(Type * _ResultType) {
    this->_ResultType = _ResultType;
  }

  llvm::SmallVector<Declaration *, 4> &getDecls() {
    return _Decls;
  }

  void setDecls(const llvm::SmallVector<Declaration *, 4> &_Decls) {
    this->_Decls = _Decls;
  }

  Block getBody() {
    return _Body;
  }

  void setBody(Block _Body) {
    this->_Body = _Body;
  }

  bool isForward() {
    return _Forward;
  }

  void setForward(bool _Forward) {
    this->_Forward = _Forward;
  }
  static bool classof(const Declaration* T) {
    return T->__Kind == 8;
  }
};

class LocalModule : public Declaration {
public:
  LocalModule(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name)
    : Declaration(9, _EnclosingDecl, _Loc, _Name) {}
  static bool classof(const Declaration* T) {
    return T->__Kind == 9;
  }
};

class Class : public Declaration {
public:
  Class(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name)
    : Declaration(10, _EnclosingDecl, _Loc, _Name) {}
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
  m2lang::pervasive::PervasiveTypeKind _TypeKind;

public:
  PervasiveType(m2lang::pervasive::PervasiveTypeKind _TypeKind)
    : TypeDenoter(0), _TypeKind(_TypeKind) {}

  m2lang::pervasive::PervasiveTypeKind getTypeKind() {
    return _TypeKind;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 0;
  }
};

class FixedRecordField {
  llvm::StringRef _Name;
  TypeDenoter * _TyDe;

public:
  FixedRecordField(llvm::StringRef _Name, TypeDenoter * _TyDe)
    : _Name(_Name), _TyDe(_TyDe) {}
  FixedRecordField() = default;

  llvm::StringRef getName() {
    return _Name;
  }

  TypeDenoter * getTyDe() {
    return _TyDe;
  }
};

class RecordType : public TypeDenoter {
  llvm::SmallVector<FixedRecordField, 4> _Fields;

public:
  RecordType(const llvm::SmallVector<FixedRecordField, 4> &_Fields)
    : TypeDenoter(1), _Fields(_Fields) {}

  const llvm::SmallVector<FixedRecordField, 4> &getFields() {
    return _Fields;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 1;
  }
};

class ArrayType : public TypeDenoter {
  TypeDenoter * _ComponentType;
  TypeDenoter * _IndexType;

public:
  ArrayType(TypeDenoter * _ComponentType, TypeDenoter * _IndexType)
    : TypeDenoter(2), _ComponentType(_ComponentType), _IndexType(_IndexType) {}

  TypeDenoter * getComponentType() {
    return _ComponentType;
  }

  TypeDenoter * getIndexType() {
    return _IndexType;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 2;
  }
};

class FormalParameterType {
  llvm::SMLoc _Loc;
  TypeDenoter * _FormalType;
  bool _IsCallByReference;

public:
  FormalParameterType(llvm::SMLoc _Loc, TypeDenoter * _FormalType, bool _IsCallByReference)
    : _Loc(_Loc), _FormalType(_FormalType), _IsCallByReference(_IsCallByReference) {}
  FormalParameterType() = default;

  llvm::SMLoc getLoc() {
    return _Loc;
  }

  TypeDenoter * getFormalType() {
    return _FormalType;
  }

  bool isIsCallByReference() {
    return _IsCallByReference;
  }
};

class ProcedureType : public TypeDenoter {
  Type * _ResultType;
  llvm::SmallVector<FormalParameterType, 4> _ParameterTypes;

public:
  ProcedureType(Type * _ResultType, const llvm::SmallVector<FormalParameterType, 4> &_ParameterTypes)
    : TypeDenoter(3), _ResultType(_ResultType), _ParameterTypes(_ParameterTypes) {}

  Type * getResultType() {
    return _ResultType;
  }

  const llvm::SmallVector<FormalParameterType, 4> &getParameterTypes() {
    return _ParameterTypes;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 3;
  }
};

class OpenArrayFormalType : public TypeDenoter {
  TypeDenoter * _ComponentType;

public:
  OpenArrayFormalType(TypeDenoter * _ComponentType)
    : TypeDenoter(4), _ComponentType(_ComponentType) {}

  TypeDenoter * getComponentType() {
    return _ComponentType;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 4;
  }
};

class PointerType : public TypeDenoter {
  TypeDenoter * _TyDen;
  llvm::StringRef _Name;
  bool _Resolved;

public:
  PointerType()
    : TypeDenoter(5) {}

  TypeDenoter * getTyDen() {
    return _TyDen;
  }

  void setTyDen(TypeDenoter * _TyDen) {
    this->_TyDen = _TyDen;
  }

  llvm::StringRef getName() {
    return _Name;
  }

  void setName(llvm::StringRef _Name) {
    this->_Name = _Name;
  }

  bool isResolved() {
    return _Resolved;
  }

  void setResolved(bool _Resolved) {
    this->_Resolved = _Resolved;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 5;
  }
};

class SubrangeType : public TypeDenoter {
  Type * _RangeType;
  Expression * _From;
  Expression * _To;

public:
  SubrangeType(Type * _RangeType, Expression * _From, Expression * _To)
    : TypeDenoter(6), _RangeType(_RangeType), _From(_From), _To(_To) {}

  Type * getRangeType() {
    return _RangeType;
  }

  Expression * getFrom() {
    return _From;
  }

  Expression * getTo() {
    return _To;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 6;
  }
};

class EnumerationType : public TypeDenoter {
  llvm::SmallVector<Constant *, 4> _Members;

public:
  EnumerationType()
    : TypeDenoter(7) {}

  llvm::SmallVector<Constant *, 4> &getMembers() {
    return _Members;
  }

  void setMembers(const llvm::SmallVector<Constant *, 4> &_Members) {
    this->_Members = _Members;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 7;
  }
};

class SetType : public TypeDenoter {
  TypeDenoter * _BaseType;
  bool _IsPacked;

public:
  SetType(TypeDenoter * _BaseType, bool _IsPacked)
    : TypeDenoter(8), _BaseType(_BaseType), _IsPacked(_IsPacked) {}

  TypeDenoter * getBaseType() {
    return _BaseType;
  }

  bool isIsPacked() {
    return _IsPacked;
  }
  static bool classof(const TypeDenoter* T) {
    return T->__Kind == 8;
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
  TypeDenoter * _TypeDenoter;
  bool _Const;

protected:
  Expression(unsigned __Kind, TypeDenoter * _TypeDenoter, bool _Const)
    : __Kind(__Kind), _TypeDenoter(_TypeDenoter), _Const(_Const) {}
public:

  TypeDenoter * getTypeDenoter() {
    return _TypeDenoter;
  }

  void setTypeDenoter(TypeDenoter * _TypeDenoter) {
    this->_TypeDenoter = _TypeDenoter;
  }

  bool isConst() {
    return _Const;
  }
};

class InfixExpression : public Expression {
  Expression * _LeftExpression;
  Expression * _RightExpression;
  m2lang::OperatorInfo _OperatorInfo;

public:
  InfixExpression(TypeDenoter * _TypeDenoter, bool _Const, Expression * _LeftExpression, Expression * _RightExpression, m2lang::OperatorInfo _OperatorInfo)
    : Expression(0, _TypeDenoter, _Const), _LeftExpression(_LeftExpression), _RightExpression(_RightExpression), _OperatorInfo(_OperatorInfo) {}

  Expression * getLeftExpression() {
    return _LeftExpression;
  }

  Expression * getRightExpression() {
    return _RightExpression;
  }

  m2lang::OperatorInfo getOperatorInfo() {
    return _OperatorInfo;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 0;
  }
};

class PrefixExpression : public Expression {
  Expression * _Expression;
  m2lang::OperatorInfo _OperatorInfo;

public:
  PrefixExpression(TypeDenoter * _TypeDenoter, bool _Const, Expression * _Expression, m2lang::OperatorInfo _OperatorInfo)
    : Expression(1, _TypeDenoter, _Const), _Expression(_Expression), _OperatorInfo(_OperatorInfo) {}

  Expression * getExpression() {
    return _Expression;
  }

  m2lang::OperatorInfo getOperatorInfo() {
    return _OperatorInfo;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 1;
  }
};

class IntegerLiteral : public Expression {
  llvm::APInt _Value;

public:
  IntegerLiteral(TypeDenoter * _TypeDenoter, llvm::APInt _Value)
    : Expression(2, _TypeDenoter, true), _Value(_Value) {}

  llvm::APInt getValue() {
    return _Value;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 2;
  }
};

class RealLiteral : public Expression {
  llvm::APFloat _Value;

public:
  RealLiteral(TypeDenoter * _TypeDenoter, llvm::APFloat _Value)
    : Expression(3, _TypeDenoter, true), _Value(_Value) {}

  llvm::APFloat getValue() {
    return _Value;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 3;
  }
};

class StringLiteral : public Expression {
  llvm::StringRef _Value;

public:
  StringLiteral(TypeDenoter * _TypeDenoter, llvm::StringRef _Value)
    : Expression(4, _TypeDenoter, true), _Value(_Value) {}

  llvm::StringRef getValue() {
    return _Value;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 4;
  }
};

class CharLiteral : public Expression {
  unsigned _Value;

public:
  CharLiteral(TypeDenoter * _TypeDenoter, unsigned _Value)
    : Expression(5, _TypeDenoter, true), _Value(_Value) {}

  unsigned getValue() {
    return _Value;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 5;
  }
};

class BooleanLiteral : public Expression {
  bool _Value;

public:
  BooleanLiteral(TypeDenoter * _TypeDenoter, bool _Value)
    : Expression(6, _TypeDenoter, true), _Value(_Value) {}

  bool isValue() {
    return _Value;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 6;
  }
};

class NilValue : public Expression {
public:
  NilValue(TypeDenoter * _TypeDenoter)
    : Expression(7, _TypeDenoter, true) {}
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
  TypeDenoter * _TyDe;

protected:
  Selector(unsigned __Kind, TypeDenoter * _TyDe)
    : __Kind(__Kind), _TyDe(_TyDe) {}
public:

  TypeDenoter * getTyDe() {
    return _TyDe;
  }
};

class IndexSelector : public Selector {
  Expression * _Index;

public:
  IndexSelector(TypeDenoter * _TyDe, Expression * _Index)
    : Selector(0, _TyDe), _Index(_Index) {}

  Expression * getIndex() {
    return _Index;
  }
  static bool classof(const Selector* T) {
    return T->__Kind == 0;
  }
};

class FieldSelector : public Selector {
public:
  FieldSelector(TypeDenoter * _TyDe)
    : Selector(1, _TyDe) {}
  static bool classof(const Selector* T) {
    return T->__Kind == 1;
  }
};

class DereferenceSelector : public Selector {
public:
  DereferenceSelector(TypeDenoter * _TyDe)
    : Selector(2, _TyDe) {}
  static bool classof(const Selector* T) {
    return T->__Kind == 2;
  }
};

class Designator : public Expression {
  Declaration * _Decl;
  bool _IsReference;
  llvm::SmallVector<Selector *, 4> _Selectors;

public:
  Designator(TypeDenoter * _TypeDenoter, bool _Const, Declaration * _Decl, bool _IsReference)
    : Expression(8, _TypeDenoter, _Const), _Decl(_Decl), _IsReference(_IsReference) {}

  Declaration * getDecl() {
    return _Decl;
  }

  bool isIsReference() {
    return _IsReference;
  }

  llvm::SmallVector<Selector *, 4> &getSelectors() {
    return _Selectors;
  }

  void setSelectors(const llvm::SmallVector<Selector *, 4> &_Selectors) {
    this->_Selectors = _Selectors;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 8;
  }
};

class FunctionCall : public Expression {
  Designator * _Desig;
  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> _ActualParameters;

public:
  FunctionCall(TypeDenoter * _TypeDenoter, bool _Const, Designator * _Desig, const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &_ActualParameters)
    : Expression(9, _TypeDenoter, _Const), _Desig(_Desig), _ActualParameters(_ActualParameters) {}

  Designator * getDesig() {
    return _Desig;
  }

  const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &getActualParameters() {
    return _ActualParameters;
  }
  static bool classof(const Expression* T) {
    return T->__Kind == 9;
  }
};

class ValueConstructor : public Expression {
public:
  ValueConstructor(TypeDenoter * _TypeDenoter)
    : Expression(10, _TypeDenoter, true) {}
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
  llvm::SMLoc _Loc;

protected:
  Statement(unsigned __Kind, llvm::SMLoc _Loc)
    : __Kind(__Kind), _Loc(_Loc) {}
public:

  llvm::SMLoc getLoc() {
    return _Loc;
  }
};

class AssignmentStatement : public Statement {
  Designator * _Designator;
  Expression * _Expression;

public:
  AssignmentStatement(llvm::SMLoc _Loc, Designator * _Designator, Expression * _Expression)
    : Statement(0, _Loc), _Designator(_Designator), _Expression(_Expression) {}

  Designator * getDesignator() {
    return _Designator;
  }

  Expression * getExpression() {
    return _Expression;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 0;
  }
};

class ProcedureCallStatement : public Statement {
  Designator * _Proc;
  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> _ActualParameters;

public:
  ProcedureCallStatement(llvm::SMLoc _Loc, Designator * _Proc, const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &_ActualParameters)
    : Statement(1, _Loc), _Proc(_Proc), _ActualParameters(_ActualParameters) {}

  Designator * getProc() {
    return _Proc;
  }

  const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &getActualParameters() {
    return _ActualParameters;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 1;
  }
};

class GuardedStatement {
  llvm::SMLoc _Loc;
  Expression * _Cond;
  llvm::SmallVector<Statement *, 4> _Stmts;

public:
  GuardedStatement(llvm::SMLoc _Loc, Expression * _Cond, const llvm::SmallVector<Statement *, 4> &_Stmts)
    : _Loc(_Loc), _Cond(_Cond), _Stmts(_Stmts) {}
  GuardedStatement() = default;

  llvm::SMLoc getLoc() {
    return _Loc;
  }

  Expression * getCond() {
    return _Cond;
  }

  const llvm::SmallVector<Statement *, 4> &getStmts() {
    return _Stmts;
  }
};

class IfStatement : public Statement {
  llvm::SmallVector<GuardedStatement, 4> _GuardedStmts;
  llvm::SmallVector<Statement *, 4> _ElseStmts;

public:
  IfStatement(llvm::SMLoc _Loc)
    : Statement(2, _Loc) {}

  llvm::SmallVector<GuardedStatement, 4> &getGuardedStmts() {
    return _GuardedStmts;
  }

  void setGuardedStmts(const llvm::SmallVector<GuardedStatement, 4> &_GuardedStmts) {
    this->_GuardedStmts = _GuardedStmts;
  }

  llvm::SmallVector<Statement *, 4> &getElseStmts() {
    return _ElseStmts;
  }

  void setElseStmts(const llvm::SmallVector<Statement *, 4> &_ElseStmts) {
    this->_ElseStmts = _ElseStmts;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 2;
  }
};

class CaseStatement : public Statement {
public:
  CaseStatement(llvm::SMLoc _Loc)
    : Statement(3, _Loc) {}
  static bool classof(const Statement* T) {
    return T->__Kind == 3;
  }
};

class WhileStatement : public Statement {
  Expression * _Cond;
  llvm::SmallVector<Statement *, 4> _Stmts;

public:
  WhileStatement(llvm::SMLoc _Loc, Expression * _Cond, const llvm::SmallVector<Statement *, 4> &_Stmts)
    : Statement(4, _Loc), _Cond(_Cond), _Stmts(_Stmts) {}

  Expression * getCond() {
    return _Cond;
  }

  const llvm::SmallVector<Statement *, 4> &getStmts() {
    return _Stmts;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 4;
  }
};

class RepeatStatement : public Statement {
  Expression * _Cond;
  llvm::SmallVector<Statement *, 4> _Stmts;

public:
  RepeatStatement(llvm::SMLoc _Loc, Expression * _Cond, const llvm::SmallVector<Statement *, 4> &_Stmts)
    : Statement(5, _Loc), _Cond(_Cond), _Stmts(_Stmts) {}

  Expression * getCond() {
    return _Cond;
  }

  const llvm::SmallVector<Statement *, 4> &getStmts() {
    return _Stmts;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 5;
  }
};

class ForStatement : public Statement {
  Variable * _ControlVariable;
  Expression * _InitialValue;
  Expression * _FinalValue;
  Expression * _StepSize;
  llvm::SmallVector<Statement *, 4> _ForStmts;

public:
  ForStatement(llvm::SMLoc _Loc, Variable * _ControlVariable, Expression * _InitialValue, Expression * _FinalValue, Expression * _StepSize, const llvm::SmallVector<Statement *, 4> &_ForStmts)
    : Statement(6, _Loc), _ControlVariable(_ControlVariable), _InitialValue(_InitialValue), _FinalValue(_FinalValue), _StepSize(_StepSize), _ForStmts(_ForStmts) {}

  Variable * getControlVariable() {
    return _ControlVariable;
  }

  Expression * getInitialValue() {
    return _InitialValue;
  }

  Expression * getFinalValue() {
    return _FinalValue;
  }

  Expression * getStepSize() {
    return _StepSize;
  }

  const llvm::SmallVector<Statement *, 4> &getForStmts() {
    return _ForStmts;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 6;
  }
};

class LoopStatement : public Statement {
  llvm::SmallVector<Statement *, 4> _Stmts;

public:
  LoopStatement(llvm::SMLoc _Loc, const llvm::SmallVector<Statement *, 4> &_Stmts)
    : Statement(7, _Loc), _Stmts(_Stmts) {}

  const llvm::SmallVector<Statement *, 4> &getStmts() {
    return _Stmts;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 7;
  }
};

class WithStatement : public Statement {
  llvm::SmallVector<Statement *, 4> _Stmts;

public:
  WithStatement(llvm::SMLoc _Loc, const llvm::SmallVector<Statement *, 4> &_Stmts)
    : Statement(8, _Loc), _Stmts(_Stmts) {}

  const llvm::SmallVector<Statement *, 4> &getStmts() {
    return _Stmts;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 8;
  }
};

class ExitStatement : public Statement {
public:
  ExitStatement(llvm::SMLoc _Loc)
    : Statement(9, _Loc) {}
  static bool classof(const Statement* T) {
    return T->__Kind == 9;
  }
};

class ReturnStatement : public Statement {
  Expression * _RetVal;

public:
  ReturnStatement(llvm::SMLoc _Loc, Expression * _RetVal)
    : Statement(10, _Loc), _RetVal(_RetVal) {}

  Expression * getRetVal() {
    return _RetVal;
  }
  static bool classof(const Statement* T) {
    return T->__Kind == 10;
  }
};

class RetryStatement : public Statement {
public:
  RetryStatement(llvm::SMLoc _Loc)
    : Statement(11, _Loc) {}
  static bool classof(const Statement* T) {
    return T->__Kind == 11;
  }
};
#endif
#ifdef AST_DEFINITION
#undef AST_DEFINITION
#endif
