#ifdef AST_DECLARATION
class Member {
  friend class Enum;
  friend class Field;
protected:
  const unsigned __Kind;
private:
  llvm::SMLoc Loc;
  llvm::StringRef Name;

protected:
  Member(unsigned __Kind, llvm::SMLoc Loc, llvm::StringRef Name)
    : __Kind(__Kind), Loc(Loc), Name(Name) {}
public:

  llvm::SMLoc getLoc() {
    return Loc;
  }

  llvm::StringRef getName() {
    return Name;
  }
};

class Enum : public Member {
  llvm::StringRef Code;

public:
  Enum(llvm::SMLoc Loc, llvm::StringRef Name, llvm::StringRef Code)
    : Member(0, Loc, Name), Code(Code) {}

  llvm::StringRef getCode() {
    return Code;
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
  unsigned Properties;
  llvm::StringRef TypeName;
  bool TypeIsList;

public:
  Field(llvm::SMLoc Loc, llvm::StringRef Name, unsigned Properties, llvm::StringRef TypeName, bool TypeIsList)
    : Member(1, Loc, Name), Properties(Properties), TypeName(TypeName), TypeIsList(TypeIsList) {}

  unsigned getProperties() {
    return Properties;
  }

  llvm::StringRef getTypeName() {
    return TypeName;
  }

  bool isTypeIsList() {
    return TypeIsList;
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
  ClassType Type;
  llvm::SMLoc Loc;
  llvm::StringRef Name;
  llvm::StringRef SuperClass;
  llvm::SmallVector<Member *, 4> Members;
  llvm::SmallVector<Class *, 4> SubClasses;

public:
  Class(ClassType Type, llvm::SMLoc Loc, llvm::StringRef Name, llvm::StringRef SuperClass, const llvm::SmallVector<Member *, 4> & Members)
    : Type(Type), Loc(Loc), Name(Name), SuperClass(SuperClass), Members(Members) {}

  ClassType getType() {
    return Type;
  }

  llvm::SMLoc getLoc() {
    return Loc;
  }

  llvm::StringRef getName() {
    return Name;
  }

  llvm::StringRef getSuperClass() {
    return SuperClass;
  }

  const llvm::SmallVector<Member *, 4> & getMembers() {
    return Members;
  }

  llvm::SmallVector<Class *, 4> getSubClasses() {
    return SubClasses;
  }

  void setSubClasses(const llvm::SmallVector<Class *, 4> & SubClasses) {
    this->SubClasses = SubClasses;
  }
};
#endif
#ifdef AST_DEFINITION
#endif
