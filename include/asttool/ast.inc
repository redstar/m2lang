#ifdef AST_DECLARATION
#undef AST_DECLARATION
class Class;

class Identifier {
  llvm::SMLoc _Loc;
  llvm::StringRef _String;

public:
  Identifier(llvm::SMLoc _Loc, llvm::StringRef _String)
    : _Loc(_Loc), _String(_String) {}
  Identifier() = default;

  llvm::SMLoc getLoc() const {
    return _Loc;
  }

  llvm::StringRef getString() const {
    return _String;
  }
};

class Member {
  friend class Enum;
  friend class Field;
public:
  enum class __KindType : unsigned {
    K_Enum,
    K_Field,
    Last = K_Field
  };
protected:
  const __KindType __Kind;
private:
  Identifier _Name;

protected:
  Member(__KindType __Kind, Identifier _Name)
    : __Kind(__Kind), _Name(_Name) {}
public:

  Identifier getName() const {
    return _Name;
  }

  __KindType kind() const { return __Kind; }
};

class Enum : public Member {
  llvm::StringRef _Code;

public:
  Enum(Identifier _Name, llvm::StringRef _Code)
    : Member(__KindType::K_Enum, _Name), _Code(_Code) {}

  llvm::StringRef getCode() const {
    return _Code;
  }

  static bool classof(const Member* T) {
    return T->__Kind == __KindType::K_Enum;
  }
};

class Field : public Member {
public:
  enum Property {
    In = 0x01, Out = 0x02
  };
  enum InitFlags {
    None = 0x00, Code = 0x01, Default = 0x03
  };
private:
  unsigned _Properties;
  unsigned _Initializer;
  llvm::StringRef _TypeName;
  bool _TypeIsList;
  llvm::StringRef _Code;

public:
  Field(Identifier _Name, unsigned _Properties, unsigned _Initializer, llvm::StringRef _TypeName, bool _TypeIsList, llvm::StringRef _Code)
    : Member(__KindType::K_Field, _Name), _Properties(_Properties), _Initializer(_Initializer), _TypeName(_TypeName), _TypeIsList(_TypeIsList), _Code(_Code) {}

  unsigned getProperties() const {
    return _Properties;
  }

  unsigned getInitializer() const {
    return _Initializer;
  }

  llvm::StringRef getTypeName() const {
    return _TypeName;
  }

  bool isTypeIsList() const {
    return _TypeIsList;
  }

  llvm::StringRef getCode() const {
    return _Code;
  }

  static bool classof(const Member* T) {
    return T->__Kind == __KindType::K_Field;
  }
};

class Declaration {
  friend class Let;
  friend class Class;
public:
  enum class __KindType : unsigned {
    K_Let,
    K_Class,
    Last = K_Class
  };
protected:
  const __KindType __Kind;
private:
  Identifier _Name;

protected:
  Declaration(__KindType __Kind, Identifier _Name)
    : __Kind(__Kind), _Name(_Name) {}
public:

  Identifier getName() const {
    return _Name;
  }

  __KindType kind() const { return __Kind; }
};

class Let : public Declaration {
  Class * _Class;
  Field * _Field;
  llvm::StringRef _Code;
  bool _Default;

public:
  Let(Identifier _Name, Class * _Class, Field * _Field, llvm::StringRef _Code, bool _Default)
    : Declaration(__KindType::K_Let, _Name), _Class(_Class), _Field(_Field), _Code(_Code), _Default(_Default) {}

  Class * getClass() const {
    return _Class;
  }

  Field * getField() const {
    return _Field;
  }

  llvm::StringRef getCode() const {
    return _Code;
  }

  bool isDefault() const {
    return _Default;
  }

  static bool classof(const Declaration* T) {
    return T->__Kind == __KindType::K_Let;
  }
};

class Class : public Declaration {
public:
  enum ClassType {
    Plain, Base, Node
  };
private:
  ClassType _Type;
  Class * _SuperClass;
  llvm::SmallVector<Member *, 4> _Members;
  llvm::SmallVector<Let *, 4> _LetDefaults;
  llvm::SmallVector<Class *, 4> _SubClasses;
  bool _Defined = false;

public:
  Class(Identifier _Name, ClassType _Type, Class * _SuperClass, const llvm::SmallVector<Member *, 4> &_Members, const llvm::SmallVector<Let *, 4> &_LetDefaults)
    : Declaration(__KindType::K_Class, _Name), _Type(_Type), _SuperClass(_SuperClass), _Members(_Members), _LetDefaults(_LetDefaults) {}

  ClassType getType() const {
    return _Type;
  }

  Class * getSuperClass() const {
    return _SuperClass;
  }

  const llvm::SmallVector<Member *, 4> &getMembers() const {
    return _Members;
  }

  const llvm::SmallVector<Let *, 4> &getLetDefaults() const {
    return _LetDefaults;
  }

  llvm::SmallVector<Class *, 4> &getSubClasses() {
    return _SubClasses;
  }

  void setSubClasses(const llvm::SmallVector<Class *, 4> &_SubClasses) {
    this->_SubClasses = _SubClasses;
  }

  bool isDefined() const {
    return _Defined;
  }

  void setDefined(bool _Defined) {
    this->_Defined = _Defined;
  }

  static bool classof(const Declaration* T) {
    return T->__Kind == __KindType::K_Class;
  }
};
#endif
#ifdef AST_DEFINITION
#undef AST_DEFINITION
#endif
