#ifdef AST_DECLARATION
class Member {
  friend class Enum;
  friend class Field;
protected:
  const unsigned __Kind;
private:
  llvm::SMLoc Loc;

protected:
  Member(unsigned __Kind, llvm::SMLoc Loc)
    : __Kind(__Kind), Loc(Loc) {}
public:

  llvm::SMLoc getLoc() {
    return Loc;
  }
};

class Enum : public Member {
  llvm::StringRef Name;
  llvm::StringRef Code;

public:
  Enum(llvm::SMLoc Loc, llvm::StringRef Name, llvm::StringRef Code)
    : Member(0, Loc), Name(Name), Code(Code) {}

  llvm::StringRef getName() {
    return Name;
  }

  llvm::StringRef getCode() {
    return Code;
  }
  static bool classof(Member* T) {
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
  llvm::StringRef Name;
  llvm::StringRef TypeName;
  bool TypeIsList;

public:
  Field(llvm::SMLoc Loc, unsigned Properties, llvm::StringRef Name, llvm::StringRef TypeName, bool TypeIsList)
    : Member(1, Loc), Properties(Properties), Name(Name), TypeName(TypeName), TypeIsList(TypeIsList) {}

  unsigned getProperties() {
    return Properties;
  }

  llvm::StringRef getName() {
    return Name;
  }

  llvm::StringRef getTypeName() {
    return TypeName;
  }

  bool isTypeIsList() {
    return TypeIsList;
  }
  static bool classof(Member* T) {
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
  llvm::SmallVector<Class *, 4> Subclasses;

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

  const llvm::SmallVector<Class *, 4> & getSubclasses() {
    return Subclasses;
  }

  void setSubclasses(const llvm::SmallVector<Class *, 4> & Subclasses) {
    this->Subclasses = Subclasses;
  }
};
#endif
#ifdef AST_DEFINITION
#endif
