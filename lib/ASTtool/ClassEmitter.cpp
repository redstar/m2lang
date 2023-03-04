//===--- EmitClass.cpp - ASTtool class source emitter -----------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the ClassEmitter helper class.
///
//===----------------------------------------------------------------------===//

#include "asttool/ClassEmitter.h"
#include "asttool/ASTDefinition.h"
#include "asttool/Class.h"
#include "asttool/VarStore.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/raw_ostream.h"
#include <optional>

using namespace asttool;

namespace {
class ClassEmitter {
  ASTDefinition &ASTDef;

  // Text fragments
  std::string GuardDeclaration;
  std::string GuardDefinition;
  std::string GuardDeclarationDispatch;
  llvm::StringRef ListType;
  llvm::StringRef ConstListType;
  llvm::StringRef KindMember;
  llvm::StringRef KindMemberPrefix;
  llvm::StringRef KindType;
  llvm::StringRef KindBaseType;
  llvm::StringRef Prefix;

  enum Prot { Public, Protected, Private };

public:
  ClassEmitter(ASTDefinition &ASTDef, const VarStore &Vars) : ASTDef(ASTDef) {
    initialize(Vars);
  }
  void run(llvm::raw_ostream &OS);

private:
  void initialize(const VarStore &Vars);
  void buildCtor(Class *C, std::optional<llvm::StringRef> KindVal,
                 llvm::SmallVectorImpl<char> &Args,
                 llvm::SmallVectorImpl<char> &Init);
  void emitClass(llvm::raw_ostream &OS, Class *C);
  void emitProt(llvm::raw_ostream &OS, Prot &Current, Prot Requested);
  void emitFriend(llvm::raw_ostream &OS, Class *C);
  void emitForwardDecls(llvm::raw_ostream &OS);
  void emitRTTIKind(llvm::raw_ostream &OS, Class *C);
  void emitDispatcher(llvm::raw_ostream &OS, Class *C);

  Class *getBaseClass(Class *C);
  Class *getRightMostChild(Class *C);
  std::string getTypename(Field *F, bool Const = false);
  std::string getFieldname(Field *F);
  std::string getKindMember(llvm::StringRef Name);
  llvm::StringRef getRef(Field *F);
};
} // namespace

void ClassEmitter::run(llvm::raw_ostream &OS) {
  OS << "#ifdef " << GuardDeclaration << "\n";
  OS << "#undef " << GuardDeclaration << "\n";
  emitForwardDecls(OS);
  bool First = true;
  for (auto V : ASTDef.getClasses()) {
    if (!First)
      OS << "\n";
    else
      First = false;
    emitClass(OS, V.second);
  }
  OS << "#ifdef " << GuardDeclarationDispatch << "\n";
  OS << "#undef " << GuardDeclarationDispatch << "\n";
  OS << "namespace dispatcher {\n";
  for (auto V : ASTDef.getClasses()) {
    emitDispatcher(OS, V.second);
  }
  OS << "}; // namespace dispatcher\n";
  OS << "#endif\n";
  OS << "#endif\n";
  OS << "#ifdef " << GuardDefinition << "\n";
  OS << "#undef " << GuardDefinition << "\n";
  OS << "#endif\n";
}

void ClassEmitter::initialize(const VarStore &Vars) {
  GuardDeclaration = "AST";
  GuardDefinition = GuardDeclaration;
  GuardDeclaration.append("_DECLARATION");
  GuardDefinition.append("_DEFINITION");
  GuardDeclarationDispatch = "AST_DISPATCHER";
  ListType = "llvm::SmallVector<{0}, 4>";
  ConstListType = "const llvm::SmallVector<{0}, 4>";
  KindMember = Vars.getVar(var::ApiRTTIMember);
  KindMemberPrefix = Vars.getVar(var::ApiRTTIMemberPrefix);
  KindType = Vars.getVar(var::ApiRTTIType);
  KindBaseType = Vars.getVar(var::ApiRTTIBaseType);
  Prefix = Vars.getVar(var::ApiPrefix);
}

/*
 * Build the argument and initializer list for the constructor.
 *
 * Handles the following cases:
 * - If the class has a super class, then a kind is passed to the super class
 * constructor. The value of KindVal decides if the constructor has a Kind
 * argument or not.
 * - A base class or...
 */
void ClassEmitter::buildCtor(Class *C, std::optional<llvm::StringRef> KindVal,
                             llvm::SmallVectorImpl<char> &Args,
                             llvm::SmallVectorImpl<char> &Init) {

  auto Append = [](llvm::SmallVectorImpl<char> &Vec, llvm::StringRef Str) {
    Vec.append(Str.begin(), Str.end());
  };

  if (C->getSuperClass()) {
    Class *SC = C->getSuperClass();
    if (!KindVal)
      llvm::Twine(KindType).concat(" ").concat(KindMember).toVector(Args);
    Append(Init, SC->getName().getString());
    Append(Init, "(");
    if (KindVal)
      Append(Init, *KindVal);
    else
      Append(Init, KindMember);

    // Initialize defaults defined by let statements.
    llvm::DenseMap<llvm::StringRef, Let *> Defaults;
    auto AddDefaults = [&](Class *C) {
      for (Let *Def : C->getLetDefaults())
        Defaults.insert(std::pair<llvm::StringRef, Let *>(
            Def->getField()->getName().getString(), Def));
    };
    AddDefaults(C);

    // Collect all super classes and let defaults.
    llvm::SmallVector<Class *, 8> SuperClasses;
    while (SC) {
      SuperClasses.push_back(SC);
      AddDefaults(SC);
      SC = SC->getSuperClass();
    }

    while (!SuperClasses.empty()) {
      SC = SuperClasses.pop_back_val();
      for (auto *M : SC->getMembers()) {
        if (auto *F = llvm::dyn_cast<Field>(M)) {
          if (F->getProperties() & Field::In) {
            Let *Default = Defaults.lookup(F->getName().getString());
            // Does field get a default value somewhere in the hierarchy?
            if (Default && Default->getClass() != C->getSuperClass())
              continue;

            if (Init.size())
              Append(Init, ", ");

            if (Default && Default->getClass() == C->getSuperClass()) {
              if (Default->isDefault())
                llvm::Twine(getTypename(F)).concat("()").toVector(Init);
              else
                llvm::Twine(Default->getCode()).toVector(Init);
            } else {
              if (Args.size())
                Append(Args, ", ");

              llvm::Twine(getTypename(F, true))
                  .concat(getRef(F))
                  .concat(getFieldname(F))
                  .toVector(Args);
              Append(Init, getFieldname(F));
            }
          }
        }
      }
    }
    Append(Init, ")");
  } else if (C->getType() == Class::Base ||
             (C->getType() == Class::Node && !KindVal &&
              !C->getSubClasses().empty())) {
    llvm::Twine(KindType).concat(" ").concat(KindMember).toVector(Args);
    llvm::Twine(KindMember)
        .concat("(")
        .concat(KindMember)
        .concat(")")
        .toVector(Init);
  } else if (C->getType() == Class::Node && KindVal) {
    llvm::Twine(KindMember)
        .concat("(")
        .concat(*KindVal)
        .concat(")")
        .toVector(Init);
  }

  for (auto *M : C->getMembers()) {
    if (auto *F = llvm::dyn_cast<Field>(M)) {
      if (F->getProperties() & Field::In) {
        if (Args.size())
          Append(Args, ", ");
        if (Init.size())
          Append(Init, ", ");
        llvm::Twine(getTypename(F, true))
            .concat(getRef(F))
            .concat(getFieldname(F))
            .toVector(Args);
        llvm::Twine(getFieldname(F))
            .concat("(")
            .concat(getFieldname(F))
            .concat(")")
            .toVector(Init);
      }
    }
  }
}

void ClassEmitter::emitClass(llvm::raw_ostream &OS, Class *C) {
  bool IsDerived = C->getSuperClass();
  bool IsBase = C->getType() == Class::Base;
  bool HasSubclasses = !C->getSubClasses().empty();
  bool NeedsKind = (IsBase || HasSubclasses) && !IsDerived;

  // Emit class definition.
  OS << "class " << C->getName().getString();
  if (IsDerived)
    OS << " : public " << C->getSuperClass()->getName().getString();
  OS << " {\n";
  Prot P = Private;
  if (NeedsKind) {
    emitFriend(OS, C);
    emitProt(OS, P, Public);
    emitRTTIKind(OS, C);
    emitProt(OS, P, Protected);
    OS << "  const " << KindType << " " << KindMember << ";\n";
  }
  for (auto *M : C->getMembers()) {
    if (auto *F = llvm::dyn_cast<Field>(M)) {
      emitProt(OS, P, Private);
      OS << "  " << getTypename(F);
      OS << " " << getFieldname(F);
      switch (F->getInitializer()) {
      case Field::None:
        break;
      case Field::Code:
        OS << " = " << F->getCode();
        break;
      case Field::Default:
        OS << " = " << getTypename(F) << "()";
        break;
      }
      OS << ";\n";
    } else if (auto *E = llvm::dyn_cast<Enum>(M)) {
      emitProt(OS, P, Public);
      OS << "  enum " << E->getName().getString() << " {\n";
      OS << "    " << E->getCode() << "\n";
      OS << "  };\n";
    } else
      llvm_unreachable("Unknown member type");
  }
  if (!C->getMembers().empty())
    OS << "\n";

  // Emit constructors.
  if (IsBase || HasSubclasses) {
    llvm::SmallString<64> Args, Init;
    buildCtor(C, std::nullopt, Args, Init);
    emitProt(OS, P, Protected);
    OS << "  " << C->getName().getString() << "(" << Args << ")";
    if (Init.size())
      OS << "\n    : " << Init;
    OS << " {}\n";
  }
  if (!IsBase) {
    llvm::SmallString<64> Args, Init;
    buildCtor(C, getKindMember(C->getName().getString()), Args, Init);
    emitProt(OS, P, Public);
    OS << "  " << C->getName().getString() << "(" << Args << ")";
    if (Init.size())
      OS << "\n    : " << Init;
    OS << " {}\n";

    // Emit a default constructor for plain classes.
    if (C->getType() == Class::Plain && !Args.empty())
      OS << "  " << C->getName().getString() << "() = default;\n";
  }
  emitProt(OS, P, Public);
  for (auto *M : C->getMembers()) {
    if (auto *F = llvm::dyn_cast<Field>(M)) {
      OS << "\n  " << getTypename(F, F->getProperties() & Field::In)
         << getRef(F) << (F->getTypeName() == "bool" ? "is" : "get")
         << F->getName().getString() << "() "
         << (F->getProperties() & Field::In || !F->isTypeIsList() ? "const "
                                                                  : "")
         << "{\n";
      OS << "    return " << getFieldname(F) << ";\n";
      OS << "  }\n";
      if (F->getProperties() != Field::In) {
        OS << "\n  void set" << F->getName().getString() << "("
           << getTypename(F, true) << getRef(F) << getFieldname(F) << ") {\n";
        OS << "    this->" << getFieldname(F) << " = " << getFieldname(F)
           << ";\n";
        OS << "  }\n";
      }
    }
  }
  if (NeedsKind) {
    OS << "\n  " << KindType << " kind() const { return " << KindMember
       << "; }\n";
  }
  if (IsDerived) {
    OS << "\n  static bool classof(const "
       << getBaseClass(C)->getName().getString() << "* T) {\n";
    if (!HasSubclasses)
      OS << "    return T->" << KindMember
         << " == " << getKindMember(C->getName().getString()) << ";\n";
    else {
      llvm::StringRef Low =
          (IsBase ? C->getSubClasses()[0] : C)->getName().getString();
      llvm::StringRef High = getRightMostChild(C)->getName().getString();
      if (Low == High)
        OS << "    return T->" << KindMember << " == " << getKindMember(Low)
           << ";\n";
      else
        OS << "    return T->" << KindMember << " >= " << getKindMember(Low)
           << " && T->" << KindMember << " <= " << getKindMember(High) << ";\n";
    }
    OS << "  }\n";
  }
  OS << "};\n";
}

void ClassEmitter::emitProt(llvm::raw_ostream &OS, Prot &Current,
                            Prot Requested) {
  if (Current != Requested) {
    switch (Requested) {
    case Public:
      OS << "public:\n";
      break;
    case Protected:
      OS << "protected:\n";
      break;
    case Private:
      OS << "private:\n";
      break;
    }
    Current = Requested;
  }
}

void ClassEmitter::emitFriend(llvm::raw_ostream &OS, Class *C) {
  for (auto *Sub : C->getSubClasses()) {
    OS << "  friend class " << Sub->getName().getString() << ";\n";
    emitFriend(OS, Sub);
  }
}

namespace {
// Range to generate the members of the Kind enumeration in the correct order.
// Iteration order is depth first, left-to-right.
class KindMemberRange {
  Class *BaseClass;

public:
  class iterator {
    llvm::SmallVector<Class *, 16> Stack;
    Class *C = nullptr;

  public:
    iterator() {}
    iterator(Class *BaseClass) {
      Stack.push_back(BaseClass);
      ++*this;
    }
    iterator &operator++() {
      while (!Stack.empty()) {
        C = Stack.pop_back_val();
        assert(C->getType() == Class::Node ||
               C->getType() == Class::Base && "Unexpected node type");
        for (Class *Sub : llvm::reverse(C->getSubClasses()))
          Stack.push_back(Sub);
        if (C->getType() == Class::Node) {
          // Do not generate an enum member for base classes. They are meant to
          // serve as abstract classes.
          return *this;
        }
      }
      C = nullptr;
      return *this;
    }
    Class *operator*() const { return C; }
    bool operator!=(const iterator &Other) const { return this->C != Other.C; }
  };

  KindMemberRange(Class *BaseClass) : BaseClass(BaseClass) {}
  iterator begin() { return iterator(BaseClass); }
  iterator end() { return iterator(); }
};
} // namespace

// Emit the members for the RTTI enumeration.
// The order is important, because of the possible generated range checks.
void ClassEmitter::emitRTTIKind(llvm::raw_ostream &OS, Class *BaseClass) {
  OS << "  enum class " << KindType << " : " << KindBaseType << " {\n";
  Class *Last = nullptr;
  for (auto *C : KindMemberRange(BaseClass)) {
    OS << "    " << KindMemberPrefix << C->getName().getString() << ",\n";
    Last = C;
  }
  if (Last)
    OS << "    Last = " << KindMemberPrefix << Last->getName().getString()
       << "\n";
  OS << "  };\n";
}

void ClassEmitter::emitDispatcher(llvm::raw_ostream &OS, Class *BaseClass) {
  bool IsDerived = BaseClass->getSuperClass();
  bool IsBase = BaseClass->getType() == Class::Base;
  bool HasSubclasses = !BaseClass->getSubClasses().empty();
  bool NeedsKind = (IsBase || HasSubclasses) && !IsDerived;
  if (!NeedsKind)
    return;

  llvm::StringRef BaseClassName = BaseClass->getName().getString();
  OS << "template <typename T, typename R = void>\n"
     << "class " << BaseClassName << "Dispatcher {\n"
     << "  typedef R (T::*Func)(" << BaseClassName << " *);\n"
     << "  Func Table[static_cast<unsigned>(" << BaseClassName << "::" << KindType
     << "::Last)+1];\n\n"
     << "public:\n"
     << "  " << BaseClassName << "Dispatcher(\n";
  bool First = true;
  for (auto *C : KindMemberRange(BaseClass)) {
    if (First)
      First = false;
    else
      OS << ",\n";
    llvm::StringRef Name = C->getName().getString();
    OS << "    R (T::*" << Name << "Fn)(" << Name << " *)";
  }
  OS << ") {\n";
  for (auto *C : KindMemberRange(BaseClass)) {
    llvm::StringRef Name = C->getName().getString();
    OS << "    Table[static_cast<unsigned>(" << BaseClassName << "::" << KindType
       << "::" << KindMemberPrefix << Name << ")] = reinterpret_cast<Func>(" << Name
       << "Fn);\n";
  }
  OS << "  }\n";
  OS << "  template<class U = R>\n"
     << "  std::enable_if_t<std::is_void_v<U>, U> operator()(T *Obj, "
     << BaseClassName << " *Arg) const {\n"
     << "    (Obj->*Table[static_cast<unsigned>(Arg->kind())])(Arg);\n"
     << "  }\n";
  OS << "  template<class U = R>\n"
     << "  std::enable_if_t<!std::is_void_v<U>, U> operator()(T *Obj, "
     << BaseClassName << " *Arg) const {\n"
     << "    return (Obj->*Table[static_cast<unsigned>(Arg->kind())])(Arg);\n"
     << "  }\n";
  OS << "};\n\n";
}

void ClassEmitter::emitForwardDecls(llvm::raw_ostream &OS) {
  bool Emitted = false;
  llvm::DenseSet<Class *> SeenClasses;
  for (auto NC : ASTDef.getClasses()) {
    Class *C = NC.second;
    SeenClasses.insert(C);
    for (auto *M : C->getMembers()) {
      if (auto *F = llvm::dyn_cast<Field>(M)) {
        Class *FieldType = ASTDef.getClasses().lookup(F->getTypeName());
        if (FieldType && SeenClasses.find(FieldType) == SeenClasses.end()) {
          OS << "class " << FieldType->getName().getString() << ";\n";
          SeenClasses.insert(FieldType);
          Emitted = true;
        }
      }
    }
  }
  if (Emitted)
    OS << "\n";
}

Class *ClassEmitter::getBaseClass(Class *C) {
  while (C->getSuperClass())
    C = C->getSuperClass();
  return C;
}

Class *ClassEmitter::getRightMostChild(Class *C) {
  while (C->getSubClasses().size())
    C = C->getSubClasses().back();
  return C;
}

std::string ClassEmitter::getTypename(Field *F, bool Const) {
  llvm::SmallString<16> MemberType;
  llvm::StringRef TypeName;
  auto *It = ASTDef.getClasses().find(F->getTypeName());
  if (It != ASTDef.getClasses().end()) {
    MemberType.append(F->getTypeName());
    if (It->second->getType() != Class::Plain)
      MemberType.append(" *");
    TypeName = MemberType.str();
  } else {
    TypeName = ASTDef.getTypedefs()[F->getTypeName()];
    // Must be name of enum if not found.
    // TODO Is there a need to define a lookup rule?
    if (TypeName.empty())
      TypeName = F->getTypeName();
  }
  const char *Fmt = F->isTypeIsList()
                        ? (Const ? ConstListType.data() : ListType.data())
                        : "{0}";
  return llvm::formatv(Fmt, TypeName).str();
}

std::string ClassEmitter::getFieldname(Field *F) {
  return llvm::formatv("{0}{1}", Prefix, F->getName().getString()).str();
}

std::string ClassEmitter::getKindMember(llvm::StringRef Name) {
  return llvm::Twine(KindType)
      .concat("::")
      .concat(KindMemberPrefix)
      .concat(Name)
      .str();
}

llvm::StringRef ClassEmitter::getRef(Field *F) {
  if (F->isTypeIsList())
    return " &";
  return " ";
}

namespace asttool {
void emitClass(ASTDefinition &ASTDef, const VarStore &Vars,
               llvm::raw_ostream &OS) {
  ClassEmitter(ASTDef, Vars).run(OS);
}
} // namespace asttool
