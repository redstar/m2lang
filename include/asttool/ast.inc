#ifdef AST_DECLARATION
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
private:
  unsigned _Properties;
  llvm::StringRef _TypeName;
  bool _TypeIsList;

public:
  Field(llvm::SMLoc _Loc, llvm::StringRef _Name, unsigned _Properties, llvm::StringRef _TypeName, bool _TypeIsList)
    : Member(1, _Loc, _Name), _Properties(_Properties), _TypeName(_TypeName), _TypeIsList(_TypeIsList) {}

  unsigned getProperties() {
    return _Properties;
  }

  llvm::StringRef getTypeName() {
    return _TypeName;
  }

  bool isTypeIsList() {
    return _TypeIsList;
  }
  static bool classof(const Member* T) {
    return T->__Kind == 1;
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
  llvm::SmallVector<Class *, 4> _SubClasses;

public:
  Class(ClassType _Type, llvm::SMLoc _Loc, llvm::StringRef _Name, Class * _SuperClass, const llvm::SmallVector<Member *, 4> &_Members)
    : _Type(_Type), _Loc(_Loc), _Name(_Name), _SuperClass(_SuperClass), _Members(_Members) {}

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
