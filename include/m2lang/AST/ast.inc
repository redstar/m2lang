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

  const llvm::SmallVector<Statement *, 4> &getStmts() const {
    return _Stmts;
  }

  const llvm::SmallVector<Statement *, 4> &getExceptStmts() const {
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
public:
  enum class __KindType : unsigned {
    K_ImplementationModule,
    K_DefinitionModule,
    K_RefiningDefinitionModule,
    K_RefiningImplementationModule,
    K_Type,
    K_Constant,
    K_Variable,
    K_FormalParameter,
    K_Procedure,
    K_LocalModule,
    K_Class,
    Last = K_Class
  };
protected:
  const __KindType __Kind;
private:
  Declaration * _EnclosingDecl;
  llvm::SMLoc _Loc;
  llvm::StringRef _Name;

protected:
  Declaration(__KindType __Kind, Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name)
    : __Kind(__Kind), _EnclosingDecl(_EnclosingDecl), _Loc(_Loc), _Name(_Name) {}
public:

  Declaration * getEnclosingDecl() const {
    return _EnclosingDecl;
  }

  llvm::SMLoc getLoc() const {
    return _Loc;
  }

  llvm::StringRef getName() const {
    return _Name;
  }

  __KindType kind() const { return __Kind; }
};

class CompilationModule : public Declaration {
protected:
  CompilationModule(__KindType __Kind, Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name)
    : Declaration(__Kind, _EnclosingDecl, _Loc, _Name) {}
public:

  static bool classof(const Declaration* T) {
    return T->__Kind >= __KindType::K_ImplementationModule && T->__Kind <= __KindType::K_RefiningImplementationModule;
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
    : CompilationModule(__KindType::K_ImplementationModule, _EnclosingDecl, _Loc, _Name), _UnsafeGuarded(_UnsafeGuarded) {}

  llvm::SmallVector<Declaration *, 4> &getDecls() {
    return _Decls;
  }

  void setDecls(const llvm::SmallVector<Declaration *, 4> &_Decls) {
    this->_Decls = _Decls;
  }

  Block getInitBlk() const {
    return _InitBlk;
  }

  void setInitBlk(Block _InitBlk) {
    this->_InitBlk = _InitBlk;
  }

  Block getFinalBlk() const {
    return _FinalBlk;
  }

  void setFinalBlk(Block _FinalBlk) {
    this->_FinalBlk = _FinalBlk;
  }

  Expression * getProtection() const {
    return _Protection;
  }

  void setProtection(Expression * _Protection) {
    this->_Protection = _Protection;
  }

  bool isUnsafeGuarded() const {
    return _UnsafeGuarded;
  }

  bool isProgramModule() const {
    return _ProgramModule;
  }

  void setProgramModule(bool _ProgramModule) {
    this->_ProgramModule = _ProgramModule;
  }

  static bool classof(const Declaration* T) {
    return T->__Kind == __KindType::K_ImplementationModule;
  }
};

class DefinitionModule : public CompilationModule {
  llvm::SmallVector<Declaration *, 4> _Decls;
  bool _UnsafeGuarded;

public:
  DefinitionModule(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name, bool _UnsafeGuarded)
    : CompilationModule(__KindType::K_DefinitionModule, _EnclosingDecl, _Loc, _Name), _UnsafeGuarded(_UnsafeGuarded) {}

  llvm::SmallVector<Declaration *, 4> &getDecls() {
    return _Decls;
  }

  void setDecls(const llvm::SmallVector<Declaration *, 4> &_Decls) {
    this->_Decls = _Decls;
  }

  bool isUnsafeGuarded() const {
    return _UnsafeGuarded;
  }

  static bool classof(const Declaration* T) {
    return T->__Kind == __KindType::K_DefinitionModule;
  }
};

class RefiningDefinitionModule : public CompilationModule {
  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> _ActualModulParams;
  bool _UnsafeGuarded;

public:
  RefiningDefinitionModule(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name, bool _UnsafeGuarded)
    : CompilationModule(__KindType::K_RefiningDefinitionModule, _EnclosingDecl, _Loc, _Name), _UnsafeGuarded(_UnsafeGuarded) {}

  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &getActualModulParams() {
    return _ActualModulParams;
  }

  void setActualModulParams(const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &_ActualModulParams) {
    this->_ActualModulParams = _ActualModulParams;
  }

  bool isUnsafeGuarded() const {
    return _UnsafeGuarded;
  }

  static bool classof(const Declaration* T) {
    return T->__Kind == __KindType::K_RefiningDefinitionModule;
  }
};

class RefiningImplementationModule : public CompilationModule {
  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> _ActualModulParams;
  bool _UnsafeGuarded;

public:
  RefiningImplementationModule(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name, bool _UnsafeGuarded)
    : CompilationModule(__KindType::K_RefiningImplementationModule, _EnclosingDecl, _Loc, _Name), _UnsafeGuarded(_UnsafeGuarded) {}

  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &getActualModulParams() {
    return _ActualModulParams;
  }

  void setActualModulParams(const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &_ActualModulParams) {
    this->_ActualModulParams = _ActualModulParams;
  }

  bool isUnsafeGuarded() const {
    return _UnsafeGuarded;
  }

  static bool classof(const Declaration* T) {
    return T->__Kind == __KindType::K_RefiningImplementationModule;
  }
};

class Type : public Declaration {
  TypeDenoter * _TypeDenoter;

public:
  Type(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name, TypeDenoter * _TypeDenoter)
    : Declaration(__KindType::K_Type, _EnclosingDecl, _Loc, _Name), _TypeDenoter(_TypeDenoter) {}

  TypeDenoter * getTypeDenoter() const {
    return _TypeDenoter;
  }

  static bool classof(const Declaration* T) {
    return T->__Kind == __KindType::K_Type;
  }
};

class Constant : public Declaration {
  TypeDenoter * _TypeDenoter;
  Expression * _ConstExpr;

public:
  Constant(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name, TypeDenoter * _TypeDenoter, Expression * _ConstExpr)
    : Declaration(__KindType::K_Constant, _EnclosingDecl, _Loc, _Name), _TypeDenoter(_TypeDenoter), _ConstExpr(_ConstExpr) {}

  TypeDenoter * getTypeDenoter() const {
    return _TypeDenoter;
  }

  Expression * getConstExpr() const {
    return _ConstExpr;
  }

  static bool classof(const Declaration* T) {
    return T->__Kind == __KindType::K_Constant;
  }
};

class Variable : public Declaration {
  TypeDenoter * _TypeDenoter;
  Expression * _Addr;

public:
  Variable(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name, TypeDenoter * _TypeDenoter, Expression * _Addr)
    : Declaration(__KindType::K_Variable, _EnclosingDecl, _Loc, _Name), _TypeDenoter(_TypeDenoter), _Addr(_Addr) {}

  TypeDenoter * getTypeDenoter() const {
    return _TypeDenoter;
  }

  Expression * getAddr() const {
    return _Addr;
  }

  static bool classof(const Declaration* T) {
    return T->__Kind == __KindType::K_Variable;
  }
};

class FormalParameter : public Declaration {
  TypeDenoter * _Type;
  bool _CallByReference;

public:
  FormalParameter(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name, TypeDenoter * _Type, bool _CallByReference)
    : Declaration(__KindType::K_FormalParameter, _EnclosingDecl, _Loc, _Name), _Type(_Type), _CallByReference(_CallByReference) {}

  TypeDenoter * getType() const {
    return _Type;
  }

  bool isCallByReference() const {
    return _CallByReference;
  }

  static bool classof(const Declaration* T) {
    return T->__Kind == __KindType::K_FormalParameter;
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
    : Declaration(__KindType::K_Procedure, _EnclosingDecl, _Loc, _Name) {}

  llvm::SmallVector<FormalParameter *, 4> &getParams() {
    return _Params;
  }

  void setParams(const llvm::SmallVector<FormalParameter *, 4> &_Params) {
    this->_Params = _Params;
  }

  Type * getResultType() const {
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

  Block getBody() const {
    return _Body;
  }

  void setBody(Block _Body) {
    this->_Body = _Body;
  }

  bool isForward() const {
    return _Forward;
  }

  void setForward(bool _Forward) {
    this->_Forward = _Forward;
  }

  static bool classof(const Declaration* T) {
    return T->__Kind == __KindType::K_Procedure;
  }
};

class LocalModule : public Declaration {
public:
  LocalModule(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name)
    : Declaration(__KindType::K_LocalModule, _EnclosingDecl, _Loc, _Name) {}

  static bool classof(const Declaration* T) {
    return T->__Kind == __KindType::K_LocalModule;
  }
};

class Class : public Declaration {
public:
  Class(Declaration * _EnclosingDecl, llvm::SMLoc _Loc, llvm::StringRef _Name)
    : Declaration(__KindType::K_Class, _EnclosingDecl, _Loc, _Name) {}

  static bool classof(const Declaration* T) {
    return T->__Kind == __KindType::K_Class;
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
public:
  enum class __KindType : unsigned {
    K_PervasiveType,
    K_RecordType,
    K_ArrayType,
    K_ProcedureType,
    K_OpenArrayFormalType,
    K_PointerType,
    K_SubrangeType,
    K_EnumerationType,
    K_SetType,
    Last = K_SetType
  };
protected:
  const __KindType __Kind;
  TypeDenoter(__KindType __Kind)
    : __Kind(__Kind) {}
public:

  __KindType kind() const { return __Kind; }
};

class PervasiveType : public TypeDenoter {
  m2lang::pervasive::PervasiveTypeKind _TypeKind;

public:
  PervasiveType(m2lang::pervasive::PervasiveTypeKind _TypeKind)
    : TypeDenoter(__KindType::K_PervasiveType), _TypeKind(_TypeKind) {}

  m2lang::pervasive::PervasiveTypeKind getTypeKind() const {
    return _TypeKind;
  }

  static bool classof(const TypeDenoter* T) {
    return T->__Kind == __KindType::K_PervasiveType;
  }
};

class FixedRecordField {
  llvm::StringRef _Name;
  TypeDenoter * _TyDe;

public:
  FixedRecordField(llvm::StringRef _Name, TypeDenoter * _TyDe)
    : _Name(_Name), _TyDe(_TyDe) {}
  FixedRecordField() = default;

  llvm::StringRef getName() const {
    return _Name;
  }

  TypeDenoter * getTyDe() const {
    return _TyDe;
  }
};

class RecordType : public TypeDenoter {
  llvm::SmallVector<FixedRecordField, 4> _Fields;

public:
  RecordType(const llvm::SmallVector<FixedRecordField, 4> &_Fields)
    : TypeDenoter(__KindType::K_RecordType), _Fields(_Fields) {}

  const llvm::SmallVector<FixedRecordField, 4> &getFields() const {
    return _Fields;
  }

  static bool classof(const TypeDenoter* T) {
    return T->__Kind == __KindType::K_RecordType;
  }
};

class ArrayType : public TypeDenoter {
  TypeDenoter * _ComponentType;
  TypeDenoter * _IndexType;

public:
  ArrayType(TypeDenoter * _ComponentType, TypeDenoter * _IndexType)
    : TypeDenoter(__KindType::K_ArrayType), _ComponentType(_ComponentType), _IndexType(_IndexType) {}

  TypeDenoter * getComponentType() const {
    return _ComponentType;
  }

  TypeDenoter * getIndexType() const {
    return _IndexType;
  }

  static bool classof(const TypeDenoter* T) {
    return T->__Kind == __KindType::K_ArrayType;
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

  llvm::SMLoc getLoc() const {
    return _Loc;
  }

  TypeDenoter * getFormalType() const {
    return _FormalType;
  }

  bool isIsCallByReference() const {
    return _IsCallByReference;
  }
};

class ProcedureType : public TypeDenoter {
  Type * _ResultType;
  llvm::SmallVector<FormalParameterType, 4> _ParameterTypes;

public:
  ProcedureType(Type * _ResultType, const llvm::SmallVector<FormalParameterType, 4> &_ParameterTypes)
    : TypeDenoter(__KindType::K_ProcedureType), _ResultType(_ResultType), _ParameterTypes(_ParameterTypes) {}

  Type * getResultType() const {
    return _ResultType;
  }

  const llvm::SmallVector<FormalParameterType, 4> &getParameterTypes() const {
    return _ParameterTypes;
  }

  static bool classof(const TypeDenoter* T) {
    return T->__Kind == __KindType::K_ProcedureType;
  }
};

class OpenArrayFormalType : public TypeDenoter {
  TypeDenoter * _ComponentType;

public:
  OpenArrayFormalType(TypeDenoter * _ComponentType)
    : TypeDenoter(__KindType::K_OpenArrayFormalType), _ComponentType(_ComponentType) {}

  TypeDenoter * getComponentType() const {
    return _ComponentType;
  }

  static bool classof(const TypeDenoter* T) {
    return T->__Kind == __KindType::K_OpenArrayFormalType;
  }
};

class PointerType : public TypeDenoter {
  TypeDenoter * _TyDen;
  llvm::StringRef _Name;
  bool _Resolved;

public:
  PointerType()
    : TypeDenoter(__KindType::K_PointerType) {}

  TypeDenoter * getTyDen() const {
    return _TyDen;
  }

  void setTyDen(TypeDenoter * _TyDen) {
    this->_TyDen = _TyDen;
  }

  llvm::StringRef getName() const {
    return _Name;
  }

  void setName(llvm::StringRef _Name) {
    this->_Name = _Name;
  }

  bool isResolved() const {
    return _Resolved;
  }

  void setResolved(bool _Resolved) {
    this->_Resolved = _Resolved;
  }

  static bool classof(const TypeDenoter* T) {
    return T->__Kind == __KindType::K_PointerType;
  }
};

class SubrangeType : public TypeDenoter {
  Type * _RangeType;
  Expression * _From;
  Expression * _To;

public:
  SubrangeType(Type * _RangeType, Expression * _From, Expression * _To)
    : TypeDenoter(__KindType::K_SubrangeType), _RangeType(_RangeType), _From(_From), _To(_To) {}

  Type * getRangeType() const {
    return _RangeType;
  }

  Expression * getFrom() const {
    return _From;
  }

  Expression * getTo() const {
    return _To;
  }

  static bool classof(const TypeDenoter* T) {
    return T->__Kind == __KindType::K_SubrangeType;
  }
};

class EnumerationType : public TypeDenoter {
  llvm::SmallVector<Constant *, 4> _Members;

public:
  EnumerationType()
    : TypeDenoter(__KindType::K_EnumerationType) {}

  llvm::SmallVector<Constant *, 4> &getMembers() {
    return _Members;
  }

  void setMembers(const llvm::SmallVector<Constant *, 4> &_Members) {
    this->_Members = _Members;
  }

  static bool classof(const TypeDenoter* T) {
    return T->__Kind == __KindType::K_EnumerationType;
  }
};

class SetType : public TypeDenoter {
  TypeDenoter * _BaseType;
  bool _IsPacked;

public:
  SetType(TypeDenoter * _BaseType, bool _IsPacked)
    : TypeDenoter(__KindType::K_SetType), _BaseType(_BaseType), _IsPacked(_IsPacked) {}

  TypeDenoter * getBaseType() const {
    return _BaseType;
  }

  bool isIsPacked() const {
    return _IsPacked;
  }

  static bool classof(const TypeDenoter* T) {
    return T->__Kind == __KindType::K_SetType;
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
public:
  enum class __KindType : unsigned {
    K_InfixExpression,
    K_PrefixExpression,
    K_IntegerLiteral,
    K_RealLiteral,
    K_StringLiteral,
    K_CharLiteral,
    K_BooleanLiteral,
    K_NilValue,
    K_Designator,
    K_FunctionCall,
    K_ValueConstructor,
    Last = K_ValueConstructor
  };
protected:
  const __KindType __Kind;
private:
  TypeDenoter * _TypeDenoter;
  bool _Const;

protected:
  Expression(__KindType __Kind, TypeDenoter * _TypeDenoter, bool _Const)
    : __Kind(__Kind), _TypeDenoter(_TypeDenoter), _Const(_Const) {}
public:

  TypeDenoter * getTypeDenoter() const {
    return _TypeDenoter;
  }

  void setTypeDenoter(TypeDenoter * _TypeDenoter) {
    this->_TypeDenoter = _TypeDenoter;
  }

  bool isConst() const {
    return _Const;
  }

  __KindType kind() const { return __Kind; }
};

class InfixExpression : public Expression {
  Expression * _LeftExpression;
  Expression * _RightExpression;
  m2lang::OperatorInfo _OperatorInfo;

public:
  InfixExpression(TypeDenoter * _TypeDenoter, bool _Const, Expression * _LeftExpression, Expression * _RightExpression, m2lang::OperatorInfo _OperatorInfo)
    : Expression(__KindType::K_InfixExpression, _TypeDenoter, _Const), _LeftExpression(_LeftExpression), _RightExpression(_RightExpression), _OperatorInfo(_OperatorInfo) {}

  Expression * getLeftExpression() const {
    return _LeftExpression;
  }

  Expression * getRightExpression() const {
    return _RightExpression;
  }

  m2lang::OperatorInfo getOperatorInfo() const {
    return _OperatorInfo;
  }

  static bool classof(const Expression* T) {
    return T->__Kind == __KindType::K_InfixExpression;
  }
};

class PrefixExpression : public Expression {
  Expression * _Expression;
  m2lang::OperatorInfo _OperatorInfo;

public:
  PrefixExpression(TypeDenoter * _TypeDenoter, bool _Const, Expression * _Expression, m2lang::OperatorInfo _OperatorInfo)
    : Expression(__KindType::K_PrefixExpression, _TypeDenoter, _Const), _Expression(_Expression), _OperatorInfo(_OperatorInfo) {}

  Expression * getExpression() const {
    return _Expression;
  }

  m2lang::OperatorInfo getOperatorInfo() const {
    return _OperatorInfo;
  }

  static bool classof(const Expression* T) {
    return T->__Kind == __KindType::K_PrefixExpression;
  }
};

class IntegerLiteral : public Expression {
  llvm::APInt _Value;

public:
  IntegerLiteral(TypeDenoter * _TypeDenoter, llvm::APInt _Value)
    : Expression(__KindType::K_IntegerLiteral, _TypeDenoter, true), _Value(_Value) {}

  llvm::APInt getValue() const {
    return _Value;
  }

  static bool classof(const Expression* T) {
    return T->__Kind == __KindType::K_IntegerLiteral;
  }
};

class RealLiteral : public Expression {
  llvm::APFloat _Value;

public:
  RealLiteral(TypeDenoter * _TypeDenoter, llvm::APFloat _Value)
    : Expression(__KindType::K_RealLiteral, _TypeDenoter, true), _Value(_Value) {}

  llvm::APFloat getValue() const {
    return _Value;
  }

  static bool classof(const Expression* T) {
    return T->__Kind == __KindType::K_RealLiteral;
  }
};

class StringLiteral : public Expression {
  llvm::StringRef _Value;

public:
  StringLiteral(TypeDenoter * _TypeDenoter, llvm::StringRef _Value)
    : Expression(__KindType::K_StringLiteral, _TypeDenoter, true), _Value(_Value) {}

  llvm::StringRef getValue() const {
    return _Value;
  }

  static bool classof(const Expression* T) {
    return T->__Kind == __KindType::K_StringLiteral;
  }
};

class CharLiteral : public Expression {
  unsigned _Value;

public:
  CharLiteral(TypeDenoter * _TypeDenoter, unsigned _Value)
    : Expression(__KindType::K_CharLiteral, _TypeDenoter, true), _Value(_Value) {}

  unsigned getValue() const {
    return _Value;
  }

  static bool classof(const Expression* T) {
    return T->__Kind == __KindType::K_CharLiteral;
  }
};

class BooleanLiteral : public Expression {
  bool _Value;

public:
  BooleanLiteral(TypeDenoter * _TypeDenoter, bool _Value)
    : Expression(__KindType::K_BooleanLiteral, _TypeDenoter, true), _Value(_Value) {}

  bool isValue() const {
    return _Value;
  }

  static bool classof(const Expression* T) {
    return T->__Kind == __KindType::K_BooleanLiteral;
  }
};

class NilValue : public Expression {
public:
  NilValue(TypeDenoter * _TypeDenoter)
    : Expression(__KindType::K_NilValue, _TypeDenoter, true) {}

  static bool classof(const Expression* T) {
    return T->__Kind == __KindType::K_NilValue;
  }
};

class Selector {
  friend class IndexSelector;
  friend class FieldSelector;
  friend class DereferenceSelector;
public:
  enum class __KindType : unsigned {
    K_IndexSelector,
    K_FieldSelector,
    K_DereferenceSelector,
    Last = K_DereferenceSelector
  };
protected:
  const __KindType __Kind;
private:
  TypeDenoter * _TyDe;

protected:
  Selector(__KindType __Kind, TypeDenoter * _TyDe)
    : __Kind(__Kind), _TyDe(_TyDe) {}
public:

  TypeDenoter * getTyDe() const {
    return _TyDe;
  }

  __KindType kind() const { return __Kind; }
};

class IndexSelector : public Selector {
  Expression * _Index;

public:
  IndexSelector(TypeDenoter * _TyDe, Expression * _Index)
    : Selector(__KindType::K_IndexSelector, _TyDe), _Index(_Index) {}

  Expression * getIndex() const {
    return _Index;
  }

  static bool classof(const Selector* T) {
    return T->__Kind == __KindType::K_IndexSelector;
  }
};

class FieldSelector : public Selector {
public:
  FieldSelector(TypeDenoter * _TyDe)
    : Selector(__KindType::K_FieldSelector, _TyDe) {}

  static bool classof(const Selector* T) {
    return T->__Kind == __KindType::K_FieldSelector;
  }
};

class DereferenceSelector : public Selector {
public:
  DereferenceSelector(TypeDenoter * _TyDe)
    : Selector(__KindType::K_DereferenceSelector, _TyDe) {}

  static bool classof(const Selector* T) {
    return T->__Kind == __KindType::K_DereferenceSelector;
  }
};

class Designator : public Expression {
  Declaration * _Decl;
  bool _IsReference;
  llvm::SmallVector<Selector *, 4> _Selectors;

public:
  Designator(TypeDenoter * _TypeDenoter, bool _Const, Declaration * _Decl, bool _IsReference)
    : Expression(__KindType::K_Designator, _TypeDenoter, _Const), _Decl(_Decl), _IsReference(_IsReference) {}

  Declaration * getDecl() const {
    return _Decl;
  }

  bool isIsReference() const {
    return _IsReference;
  }

  llvm::SmallVector<Selector *, 4> &getSelectors() {
    return _Selectors;
  }

  void setSelectors(const llvm::SmallVector<Selector *, 4> &_Selectors) {
    this->_Selectors = _Selectors;
  }

  static bool classof(const Expression* T) {
    return T->__Kind == __KindType::K_Designator;
  }
};

class FunctionCall : public Expression {
  Designator * _Desig;
  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> _ActualParameters;

public:
  FunctionCall(TypeDenoter * _TypeDenoter, bool _Const, Designator * _Desig, const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &_ActualParameters)
    : Expression(__KindType::K_FunctionCall, _TypeDenoter, _Const), _Desig(_Desig), _ActualParameters(_ActualParameters) {}

  Designator * getDesig() const {
    return _Desig;
  }

  const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &getActualParameters() const {
    return _ActualParameters;
  }

  static bool classof(const Expression* T) {
    return T->__Kind == __KindType::K_FunctionCall;
  }
};

class ValueConstructor : public Expression {
public:
  ValueConstructor(TypeDenoter * _TypeDenoter)
    : Expression(__KindType::K_ValueConstructor, _TypeDenoter, true) {}

  static bool classof(const Expression* T) {
    return T->__Kind == __KindType::K_ValueConstructor;
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
public:
  enum class __KindType : unsigned {
    K_AssignmentStatement,
    K_ProcedureCallStatement,
    K_IfStatement,
    K_CaseStatement,
    K_WhileStatement,
    K_RepeatStatement,
    K_ForStatement,
    K_LoopStatement,
    K_WithStatement,
    K_ExitStatement,
    K_ReturnStatement,
    K_RetryStatement,
    Last = K_RetryStatement
  };
protected:
  const __KindType __Kind;
private:
  llvm::SMLoc _Loc;

protected:
  Statement(__KindType __Kind, llvm::SMLoc _Loc)
    : __Kind(__Kind), _Loc(_Loc) {}
public:

  llvm::SMLoc getLoc() const {
    return _Loc;
  }

  __KindType kind() const { return __Kind; }
};

class AssignmentStatement : public Statement {
  Designator * _Designator;
  Expression * _Expression;

public:
  AssignmentStatement(llvm::SMLoc _Loc, Designator * _Designator, Expression * _Expression)
    : Statement(__KindType::K_AssignmentStatement, _Loc), _Designator(_Designator), _Expression(_Expression) {}

  Designator * getDesignator() const {
    return _Designator;
  }

  Expression * getExpression() const {
    return _Expression;
  }

  static bool classof(const Statement* T) {
    return T->__Kind == __KindType::K_AssignmentStatement;
  }
};

class ProcedureCallStatement : public Statement {
  Designator * _Proc;
  llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> _ActualParameters;

public:
  ProcedureCallStatement(llvm::SMLoc _Loc, Designator * _Proc, const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &_ActualParameters)
    : Statement(__KindType::K_ProcedureCallStatement, _Loc), _Proc(_Proc), _ActualParameters(_ActualParameters) {}

  Designator * getProc() const {
    return _Proc;
  }

  const llvm::SmallVector<llvm::PointerUnion<Expression *, Type *>, 4> &getActualParameters() const {
    return _ActualParameters;
  }

  static bool classof(const Statement* T) {
    return T->__Kind == __KindType::K_ProcedureCallStatement;
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

  llvm::SMLoc getLoc() const {
    return _Loc;
  }

  Expression * getCond() const {
    return _Cond;
  }

  const llvm::SmallVector<Statement *, 4> &getStmts() const {
    return _Stmts;
  }
};

class IfStatement : public Statement {
  llvm::SmallVector<GuardedStatement, 4> _GuardedStmts;
  llvm::SmallVector<Statement *, 4> _ElseStmts;

public:
  IfStatement(llvm::SMLoc _Loc)
    : Statement(__KindType::K_IfStatement, _Loc) {}

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
    return T->__Kind == __KindType::K_IfStatement;
  }
};

class CaseStatement : public Statement {
public:
  CaseStatement(llvm::SMLoc _Loc)
    : Statement(__KindType::K_CaseStatement, _Loc) {}

  static bool classof(const Statement* T) {
    return T->__Kind == __KindType::K_CaseStatement;
  }
};

class WhileStatement : public Statement {
  Expression * _Cond;
  llvm::SmallVector<Statement *, 4> _Stmts;

public:
  WhileStatement(llvm::SMLoc _Loc, Expression * _Cond, const llvm::SmallVector<Statement *, 4> &_Stmts)
    : Statement(__KindType::K_WhileStatement, _Loc), _Cond(_Cond), _Stmts(_Stmts) {}

  Expression * getCond() const {
    return _Cond;
  }

  const llvm::SmallVector<Statement *, 4> &getStmts() const {
    return _Stmts;
  }

  static bool classof(const Statement* T) {
    return T->__Kind == __KindType::K_WhileStatement;
  }
};

class RepeatStatement : public Statement {
  Expression * _Cond;
  llvm::SmallVector<Statement *, 4> _Stmts;

public:
  RepeatStatement(llvm::SMLoc _Loc, Expression * _Cond, const llvm::SmallVector<Statement *, 4> &_Stmts)
    : Statement(__KindType::K_RepeatStatement, _Loc), _Cond(_Cond), _Stmts(_Stmts) {}

  Expression * getCond() const {
    return _Cond;
  }

  const llvm::SmallVector<Statement *, 4> &getStmts() const {
    return _Stmts;
  }

  static bool classof(const Statement* T) {
    return T->__Kind == __KindType::K_RepeatStatement;
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
    : Statement(__KindType::K_ForStatement, _Loc), _ControlVariable(_ControlVariable), _InitialValue(_InitialValue), _FinalValue(_FinalValue), _StepSize(_StepSize), _ForStmts(_ForStmts) {}

  Variable * getControlVariable() const {
    return _ControlVariable;
  }

  Expression * getInitialValue() const {
    return _InitialValue;
  }

  Expression * getFinalValue() const {
    return _FinalValue;
  }

  Expression * getStepSize() const {
    return _StepSize;
  }

  const llvm::SmallVector<Statement *, 4> &getForStmts() const {
    return _ForStmts;
  }

  static bool classof(const Statement* T) {
    return T->__Kind == __KindType::K_ForStatement;
  }
};

class LoopStatement : public Statement {
  llvm::SmallVector<Statement *, 4> _Stmts;

public:
  LoopStatement(llvm::SMLoc _Loc, const llvm::SmallVector<Statement *, 4> &_Stmts)
    : Statement(__KindType::K_LoopStatement, _Loc), _Stmts(_Stmts) {}

  const llvm::SmallVector<Statement *, 4> &getStmts() const {
    return _Stmts;
  }

  static bool classof(const Statement* T) {
    return T->__Kind == __KindType::K_LoopStatement;
  }
};

class WithStatement : public Statement {
  llvm::SmallVector<Statement *, 4> _Stmts;

public:
  WithStatement(llvm::SMLoc _Loc, const llvm::SmallVector<Statement *, 4> &_Stmts)
    : Statement(__KindType::K_WithStatement, _Loc), _Stmts(_Stmts) {}

  const llvm::SmallVector<Statement *, 4> &getStmts() const {
    return _Stmts;
  }

  static bool classof(const Statement* T) {
    return T->__Kind == __KindType::K_WithStatement;
  }
};

class ExitStatement : public Statement {
public:
  ExitStatement(llvm::SMLoc _Loc)
    : Statement(__KindType::K_ExitStatement, _Loc) {}

  static bool classof(const Statement* T) {
    return T->__Kind == __KindType::K_ExitStatement;
  }
};

class ReturnStatement : public Statement {
  Expression * _RetVal;

public:
  ReturnStatement(llvm::SMLoc _Loc, Expression * _RetVal)
    : Statement(__KindType::K_ReturnStatement, _Loc), _RetVal(_RetVal) {}

  Expression * getRetVal() const {
    return _RetVal;
  }

  static bool classof(const Statement* T) {
    return T->__Kind == __KindType::K_ReturnStatement;
  }
};

class RetryStatement : public Statement {
public:
  RetryStatement(llvm::SMLoc _Loc)
    : Statement(__KindType::K_RetryStatement, _Loc) {}

  static bool classof(const Statement* T) {
    return T->__Kind == __KindType::K_RetryStatement;
  }
};
#ifdef AST_DISPATCHER
#undef AST_DISPATCHER
namespace dispatcher {
template <typename T>
class DeclarationDispatcher {
  typedef void (T::*Func)(Declaration *);
  Func Table[static_cast<unsigned>(Declaration::__KindType::Last)+1];

public:
  DeclarationDispatcher(
    void (T::*ImplementationModuleFn)(ImplementationModule *),
    void (T::*DefinitionModuleFn)(DefinitionModule *),
    void (T::*RefiningDefinitionModuleFn)(RefiningDefinitionModule *),
    void (T::*RefiningImplementationModuleFn)(RefiningImplementationModule *),
    void (T::*TypeFn)(Type *),
    void (T::*ConstantFn)(Constant *),
    void (T::*VariableFn)(Variable *),
    void (T::*FormalParameterFn)(FormalParameter *),
    void (T::*ProcedureFn)(Procedure *),
    void (T::*LocalModuleFn)(LocalModule *),
    void (T::*ClassFn)(Class *)) {
    Table[static_cast<unsigned>(Declaration::__KindType::K_ImplementationModule)] = reinterpret_cast<Func>(ImplementationModuleFn);
    Table[static_cast<unsigned>(Declaration::__KindType::K_DefinitionModule)] = reinterpret_cast<Func>(DefinitionModuleFn);
    Table[static_cast<unsigned>(Declaration::__KindType::K_RefiningDefinitionModule)] = reinterpret_cast<Func>(RefiningDefinitionModuleFn);
    Table[static_cast<unsigned>(Declaration::__KindType::K_RefiningImplementationModule)] = reinterpret_cast<Func>(RefiningImplementationModuleFn);
    Table[static_cast<unsigned>(Declaration::__KindType::K_Type)] = reinterpret_cast<Func>(TypeFn);
    Table[static_cast<unsigned>(Declaration::__KindType::K_Constant)] = reinterpret_cast<Func>(ConstantFn);
    Table[static_cast<unsigned>(Declaration::__KindType::K_Variable)] = reinterpret_cast<Func>(VariableFn);
    Table[static_cast<unsigned>(Declaration::__KindType::K_FormalParameter)] = reinterpret_cast<Func>(FormalParameterFn);
    Table[static_cast<unsigned>(Declaration::__KindType::K_Procedure)] = reinterpret_cast<Func>(ProcedureFn);
    Table[static_cast<unsigned>(Declaration::__KindType::K_LocalModule)] = reinterpret_cast<Func>(LocalModuleFn);
    Table[static_cast<unsigned>(Declaration::__KindType::K_Class)] = reinterpret_cast<Func>(ClassFn);
  }
  void operator()(T *Obj, Declaration *Arg) const {
    (Obj->*Table[static_cast<unsigned>(Arg->kind())])(Arg);
  }
};

template <typename T>
class TypeDenoterDispatcher {
  typedef void (T::*Func)(TypeDenoter *);
  Func Table[static_cast<unsigned>(TypeDenoter::__KindType::Last)+1];

public:
  TypeDenoterDispatcher(
    void (T::*PervasiveTypeFn)(PervasiveType *),
    void (T::*RecordTypeFn)(RecordType *),
    void (T::*ArrayTypeFn)(ArrayType *),
    void (T::*ProcedureTypeFn)(ProcedureType *),
    void (T::*OpenArrayFormalTypeFn)(OpenArrayFormalType *),
    void (T::*PointerTypeFn)(PointerType *),
    void (T::*SubrangeTypeFn)(SubrangeType *),
    void (T::*EnumerationTypeFn)(EnumerationType *),
    void (T::*SetTypeFn)(SetType *)) {
    Table[static_cast<unsigned>(TypeDenoter::__KindType::K_PervasiveType)] = reinterpret_cast<Func>(PervasiveTypeFn);
    Table[static_cast<unsigned>(TypeDenoter::__KindType::K_RecordType)] = reinterpret_cast<Func>(RecordTypeFn);
    Table[static_cast<unsigned>(TypeDenoter::__KindType::K_ArrayType)] = reinterpret_cast<Func>(ArrayTypeFn);
    Table[static_cast<unsigned>(TypeDenoter::__KindType::K_ProcedureType)] = reinterpret_cast<Func>(ProcedureTypeFn);
    Table[static_cast<unsigned>(TypeDenoter::__KindType::K_OpenArrayFormalType)] = reinterpret_cast<Func>(OpenArrayFormalTypeFn);
    Table[static_cast<unsigned>(TypeDenoter::__KindType::K_PointerType)] = reinterpret_cast<Func>(PointerTypeFn);
    Table[static_cast<unsigned>(TypeDenoter::__KindType::K_SubrangeType)] = reinterpret_cast<Func>(SubrangeTypeFn);
    Table[static_cast<unsigned>(TypeDenoter::__KindType::K_EnumerationType)] = reinterpret_cast<Func>(EnumerationTypeFn);
    Table[static_cast<unsigned>(TypeDenoter::__KindType::K_SetType)] = reinterpret_cast<Func>(SetTypeFn);
  }
  void operator()(T *Obj, TypeDenoter *Arg) const {
    (Obj->*Table[static_cast<unsigned>(Arg->kind())])(Arg);
  }
};

template <typename T>
class ExpressionDispatcher {
  typedef void (T::*Func)(Expression *);
  Func Table[static_cast<unsigned>(Expression::__KindType::Last)+1];

public:
  ExpressionDispatcher(
    void (T::*InfixExpressionFn)(InfixExpression *),
    void (T::*PrefixExpressionFn)(PrefixExpression *),
    void (T::*IntegerLiteralFn)(IntegerLiteral *),
    void (T::*RealLiteralFn)(RealLiteral *),
    void (T::*StringLiteralFn)(StringLiteral *),
    void (T::*CharLiteralFn)(CharLiteral *),
    void (T::*BooleanLiteralFn)(BooleanLiteral *),
    void (T::*NilValueFn)(NilValue *),
    void (T::*DesignatorFn)(Designator *),
    void (T::*FunctionCallFn)(FunctionCall *),
    void (T::*ValueConstructorFn)(ValueConstructor *)) {
    Table[static_cast<unsigned>(Expression::__KindType::K_InfixExpression)] = reinterpret_cast<Func>(InfixExpressionFn);
    Table[static_cast<unsigned>(Expression::__KindType::K_PrefixExpression)] = reinterpret_cast<Func>(PrefixExpressionFn);
    Table[static_cast<unsigned>(Expression::__KindType::K_IntegerLiteral)] = reinterpret_cast<Func>(IntegerLiteralFn);
    Table[static_cast<unsigned>(Expression::__KindType::K_RealLiteral)] = reinterpret_cast<Func>(RealLiteralFn);
    Table[static_cast<unsigned>(Expression::__KindType::K_StringLiteral)] = reinterpret_cast<Func>(StringLiteralFn);
    Table[static_cast<unsigned>(Expression::__KindType::K_CharLiteral)] = reinterpret_cast<Func>(CharLiteralFn);
    Table[static_cast<unsigned>(Expression::__KindType::K_BooleanLiteral)] = reinterpret_cast<Func>(BooleanLiteralFn);
    Table[static_cast<unsigned>(Expression::__KindType::K_NilValue)] = reinterpret_cast<Func>(NilValueFn);
    Table[static_cast<unsigned>(Expression::__KindType::K_Designator)] = reinterpret_cast<Func>(DesignatorFn);
    Table[static_cast<unsigned>(Expression::__KindType::K_FunctionCall)] = reinterpret_cast<Func>(FunctionCallFn);
    Table[static_cast<unsigned>(Expression::__KindType::K_ValueConstructor)] = reinterpret_cast<Func>(ValueConstructorFn);
  }
  void operator()(T *Obj, Expression *Arg) const {
    (Obj->*Table[static_cast<unsigned>(Arg->kind())])(Arg);
  }
};

template <typename T>
class SelectorDispatcher {
  typedef void (T::*Func)(Selector *);
  Func Table[static_cast<unsigned>(Selector::__KindType::Last)+1];

public:
  SelectorDispatcher(
    void (T::*IndexSelectorFn)(IndexSelector *),
    void (T::*FieldSelectorFn)(FieldSelector *),
    void (T::*DereferenceSelectorFn)(DereferenceSelector *)) {
    Table[static_cast<unsigned>(Selector::__KindType::K_IndexSelector)] = reinterpret_cast<Func>(IndexSelectorFn);
    Table[static_cast<unsigned>(Selector::__KindType::K_FieldSelector)] = reinterpret_cast<Func>(FieldSelectorFn);
    Table[static_cast<unsigned>(Selector::__KindType::K_DereferenceSelector)] = reinterpret_cast<Func>(DereferenceSelectorFn);
  }
  void operator()(T *Obj, Selector *Arg) const {
    (Obj->*Table[static_cast<unsigned>(Arg->kind())])(Arg);
  }
};

template <typename T>
class StatementDispatcher {
  typedef void (T::*Func)(Statement *);
  Func Table[static_cast<unsigned>(Statement::__KindType::Last)+1];

public:
  StatementDispatcher(
    void (T::*AssignmentStatementFn)(AssignmentStatement *),
    void (T::*ProcedureCallStatementFn)(ProcedureCallStatement *),
    void (T::*IfStatementFn)(IfStatement *),
    void (T::*CaseStatementFn)(CaseStatement *),
    void (T::*WhileStatementFn)(WhileStatement *),
    void (T::*RepeatStatementFn)(RepeatStatement *),
    void (T::*ForStatementFn)(ForStatement *),
    void (T::*LoopStatementFn)(LoopStatement *),
    void (T::*WithStatementFn)(WithStatement *),
    void (T::*ExitStatementFn)(ExitStatement *),
    void (T::*ReturnStatementFn)(ReturnStatement *),
    void (T::*RetryStatementFn)(RetryStatement *)) {
    Table[static_cast<unsigned>(Statement::__KindType::K_AssignmentStatement)] = reinterpret_cast<Func>(AssignmentStatementFn);
    Table[static_cast<unsigned>(Statement::__KindType::K_ProcedureCallStatement)] = reinterpret_cast<Func>(ProcedureCallStatementFn);
    Table[static_cast<unsigned>(Statement::__KindType::K_IfStatement)] = reinterpret_cast<Func>(IfStatementFn);
    Table[static_cast<unsigned>(Statement::__KindType::K_CaseStatement)] = reinterpret_cast<Func>(CaseStatementFn);
    Table[static_cast<unsigned>(Statement::__KindType::K_WhileStatement)] = reinterpret_cast<Func>(WhileStatementFn);
    Table[static_cast<unsigned>(Statement::__KindType::K_RepeatStatement)] = reinterpret_cast<Func>(RepeatStatementFn);
    Table[static_cast<unsigned>(Statement::__KindType::K_ForStatement)] = reinterpret_cast<Func>(ForStatementFn);
    Table[static_cast<unsigned>(Statement::__KindType::K_LoopStatement)] = reinterpret_cast<Func>(LoopStatementFn);
    Table[static_cast<unsigned>(Statement::__KindType::K_WithStatement)] = reinterpret_cast<Func>(WithStatementFn);
    Table[static_cast<unsigned>(Statement::__KindType::K_ExitStatement)] = reinterpret_cast<Func>(ExitStatementFn);
    Table[static_cast<unsigned>(Statement::__KindType::K_ReturnStatement)] = reinterpret_cast<Func>(ReturnStatementFn);
    Table[static_cast<unsigned>(Statement::__KindType::K_RetryStatement)] = reinterpret_cast<Func>(RetryStatementFn);
  }
  void operator()(T *Obj, Statement *Arg) const {
    (Obj->*Table[static_cast<unsigned>(Arg->kind())])(Arg);
  }
};

}; // namespace dispatcher
#endif
#endif
#ifdef AST_DEFINITION
#undef AST_DEFINITION
#endif
