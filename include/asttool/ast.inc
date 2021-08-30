#ifdef AST_DECLARATION
class Class;

class Member {
  friend class Enum;
  friend class Field;
protected:
  const unsigned __Kind;
private:
  llvm::SMLoc _Loc;
  llvm::StringRef _Name;

protected:
  Member(unsigned __Kind, llvm::SMLoc _Loc, llvm::StringRef _Name)
    : __Kind(__Kind), _Loc(_Loc), _Name(_Name) {}
public:

  llvm::SMLoc getLoc() {
    return _Loc;
  }

  llvm::StringRef getName() {
    return _Name;
  }
};

class Enum : public Member {
  llvm::StringRef _Code;

public:
  Enum(llvm::SMLoc _Loc, llvm::StringRef _Name, llvm::StringRef _Code)
    : Member(0, _Loc, _Name), _Code(_Code) {}

  llvm::StringRef getCode() {
    return _Code;
  }
  static bool classof(const Member* T) {
    return T->__Kind == 0;
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
  Field(llvm::SMLoc _Loc, llvm::StringRef _Name, unsigned _Properties, unsigned _Initializer, llvm::StringRef _TypeName, bool _TypeIsList, llvm::StringRef _Code)
    : Member(1, _Loc, _Name), _Properties(_Properties), _Initializer(_Initializer), _TypeName(_TypeName), _TypeIsList(_TypeIsList), _Code(_Code) {}

  unsigned getProperties() {
    return _Properties;
  }

  unsigned getInitializer() {
    return _Initializer;
  }

  llvm::StringRef getTypeName() {
    return _TypeName;
  }

  bool isTypeIsList() {
    return _TypeIsList;
  }

  llvm::StringRef getCode() {
    return _Code;
  }
  static bool classof(const Member* T) {
    return T->__Kind == 1;
  }
};

class Let {
  llvm::SMLoc _Loc;
  Class * _Class;
  Field * _Field;
  llvm::StringRef _Code;
  bool _Default;

public:
  Let(llvm::SMLoc _Loc, Class * _Class, Field * _Field, llvm::StringRef _Code, bool _Default)
    : _Loc(_Loc), _Class(_Class), _Field(_Field), _Code(_Code), _Default(_Default) {}

  llvm::SMLoc getLoc() {
    return _Loc;
  }

  Class * getClass() {
    return _Class;
  }

  Field * getField() {
    return _Field;
  }

  llvm::StringRef getCode() {
    return _Code;
  }

  bool isDefault() {
    return _Default;
  }
};

class Class {
public:
  enum ClassType {
    Plain, Base, Node
  };
private:
  ClassType _Type;
  llvm::SMLoc _Loc;
  llvm::StringRef _Name;
  Class * _SuperClass;
  llvm::SmallVector<Member *, 4> _Members;
  llvm::SmallVector<Let *, 4> _LetDefaults;
  llvm::SmallVector<Class *, 4> _SubClasses;

public:
  Class(ClassType _Type, llvm::SMLoc _Loc, llvm::StringRef _Name, Class * _SuperClass, const llvm::SmallVector<Member *, 4> &_Members, const llvm::SmallVector<Let *, 4> &_LetDefaults)
    : _Type(_Type), _Loc(_Loc), _Name(_Name), _SuperClass(_SuperClass), _Members(_Members), _LetDefaults(_LetDefaults) {}

  ClassType getType() {
    return _Type;
  }

  llvm::SMLoc getLoc() {
    return _Loc;
  }

  llvm::StringRef getName() {
    return _Name;
  }

  Class * getSuperClass() {
    return _SuperClass;
  }

  const llvm::SmallVector<Member *, 4> &getMembers() {
    return _Members;
  }

  const llvm::SmallVector<Let *, 4> &getLetDefaults() {
    return _LetDefaults;
  }

  llvm::SmallVector<Class *, 4> &getSubClasses() {
    return _SubClasses;
  }

  void setSubClasses(const llvm::SmallVector<Class *, 4> &_SubClasses) {
    this->_SubClasses = _SubClasses;
  }
};
#endif
#ifdef AST_DEFINITION
#endif
