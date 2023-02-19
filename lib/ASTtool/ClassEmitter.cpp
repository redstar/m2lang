//===--- EmitClass.cpp - ASTtool class source emitter -----------------*- C++
//-*-===//
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
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/raw_ostream.h"

using namespace asttool;

namespace {
class ClassEmitter {
  ASTDefinition &ASTDef;

  // Mapping of class to kind value. The kind value is stored bitwise negated,
  // with 0 indicating no value.
  llvm::DenseMap<Class *, unsigned> KindValues;

  // Text fragments
  std::string GuardDeclaration;
  std::string GuardDefinition;
  llvm::StringRef ListType;
  llvm::StringRef ConstListType;
  llvm::StringRef KindMember;
  llvm::StringRef KindType;
  llvm::StringRef Prefix;

  enum Prot { Public, Protected, Private };

public:
  ClassEmitter(ASTDefinition &ASTDef) : ASTDef(ASTDef) { initialize(); }
  void run(llvm::raw_ostream &OS);

private:
  void initialize();
  void caculateKindValues();
  void caculateKindValues(Class *C, unsigned &Last);
  void buildCtor(Class *C, unsigned KindVal, llvm::SmallVectorImpl<char> &Args,
                 llvm::SmallVectorImpl<char> &Init);
  void emitClass(llvm::raw_ostream &OS, Class *C);
  void emitProt(llvm::raw_ostream &OS, Prot &Current, Prot Requested);
  void emitFriend(llvm::raw_ostream &OS, Class *C);
  void emitForwardDecls(llvm::raw_ostream &OS);

  Class *getBaseClass(Class *C);
  std::string getTypename(Field *F, bool Const = false);
  std::string getFieldname(Field *F);
  llvm::StringRef getRef(Field *F);
};
} // namespace

void ClassEmitter::run(llvm::raw_ostream &OS) {
  OS << "#ifdef " << GuardDeclaration << "\n";
#if 0
  emitTokenSetType(OS);
  emitFollowSets(OS, true);
  emitSupportFunc(OS, true);
  for (Node *N : G.nodes()) {
    if (auto NT = llvm::dyn_cast<Nonterminal>(N))
      if (NT != G.syntheticStartSymbol())
        emitRule(OS, NT, true);
  }
#endif
  emitForwardDecls(OS);
  bool First = true;
  for (auto V : ASTDef.getClasses()) {
    if (!First)
      OS << "\n";
    else
      First = false;
    emitClass(OS, V.second);
  }
  OS << "#endif\n";
  OS << "#ifdef " << GuardDefinition << "\n";
#if 0
  emitFollowSets(OS);
  emitSupportFunc(OS);
  for (Node *N : G.nodes()) {
    if (auto NT = llvm::dyn_cast<Nonterminal>(N))
      if (NT != G.syntheticStartSymbol())
        emitRule(OS, NT);
  }
#endif
  OS << "#endif\n";
}

void ClassEmitter::initialize() {
  GuardDeclaration = llvm::StringRef("AST").upper(); // ParserClass;
  GuardDefinition = GuardDeclaration;
  GuardDeclaration.append("_DECLARATION");
  GuardDefinition.append("_DEFINITION");
  ListType = "llvm::SmallVector<{0}, 4>";
  ConstListType = "const llvm::SmallVector<{0}, 4>";
  KindMember = "__Kind";
  KindType = "unsigned";
  Prefix = "_";
  caculateKindValues();
}

void ClassEmitter::caculateKindValues() {
  KindValues.grow(ASTDef.getClasses().size());
  for (auto NC : ASTDef.getClasses())
    KindValues[NC.second] = static_cast<unsigned>(0);
  for (auto NC : ASTDef.getClasses()) {
    Class *C = NC.second;
    if (!C->getSuperClass() &&
        (C->getType() == Class::Base ||
         (C->getType() == Class::Node && !C->getSubClasses().empty()))) {
      unsigned Last = 0;
      caculateKindValues(C, Last);
    }
  }
#if 0
  llvm::errs() << "KindValues\n";
  for (auto NC : ASTDef.getClasses()) {
    Class *C = NC.second;
    unsigned Kind = KindValues[C];
    std::string V;
    if (Kind)
      V = llvm::utostr(static_cast<uint64_t>(~Kind));
    else
      V = "undef";
    llvm::errs() << C->getName() << " " << V << "\n";
  }
#endif
}

void ClassEmitter::caculateKindValues(Class *C, unsigned &Last) {
  assert(C->getType() != Class::Plain && "Unexpected plain class type");
  if (C->getType() == Class::Node) {
    KindValues[C] = ~Last;
    ++Last;
  }
  for (auto Sub : C->getSubClasses()) {
    caculateKindValues(Sub, Last);
  }
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
void ClassEmitter::buildCtor(Class *C, unsigned KindVal,
                             llvm::SmallVectorImpl<char> &Args,
                             llvm::SmallVectorImpl<char> &Init) {

  auto append = [](llvm::SmallVectorImpl<char> &Vec, llvm::StringRef Str) {
    Vec.append(Str.begin(), Str.end());
  };

  if (C->getSuperClass()) {
    Class *SC = C->getSuperClass();
    if (!KindVal)
      llvm::Twine(KindType).concat(" ").concat(KindMember).toVector(Args);
    append(Init, SC->getName());
    append(Init, "(");
    if (KindVal)
      append(Init, llvm::utostr(static_cast<uint64_t>(~KindVal)));
    else
      append(Init, KindMember);

    // Initialize defaults defined by let statements.
    llvm::DenseMap<llvm::StringRef, Let *> Defaults;
    auto addDefaults = [&](Class *C) {
      for (Let *Def : C->getLetDefaults())
        Defaults.insert(
            std::pair<llvm::StringRef, Let *>(Def->getField()->getName(), Def));
    };
    addDefaults(C);

    // Collect all super classes and let defaults.
    llvm::SmallVector<Class *, 8> SuperClasses;
    while (SC) {
      SuperClasses.push_back(SC);
      addDefaults(SC);
      SC = SC->getSuperClass();
    }

    while (!SuperClasses.empty()) {
      SC = SuperClasses.pop_back_val();
      for (auto *M : SC->getMembers()) {
        if (auto *F = llvm::dyn_cast<Field>(M)) {
          if (F->getProperties() & Field::In) {
            Let *Default = Defaults.lookup(F->getName());
            // Does field get a default value somewhere in the hierarchy?
            if (Default && Default->getClass() != C->getSuperClass())
              continue;

            if (Init.size())
              append(Init, ", ");

            if (Default && Default->getClass() == C->getSuperClass()) {
              if (Default->isDefault())
                llvm::Twine(getTypename(F)).concat("()").toVector(Init);
              else
                llvm::Twine(Default->getCode()).toVector(Init);
            } else {
              if (Args.size())
                append(Args, ", ");

              llvm::Twine(getTypename(F, true))
                  .concat(getRef(F))
                  .concat(getFieldname(F))
                  .toVector(Args);
              append(Init, getFieldname(F));
            }
          }
        }
      }
    }
    append(Init, ")");
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
        .concat(llvm::utostr(static_cast<uint64_t>(~KindVal)))
        .concat(")")
        .toVector(Init);
  }

  for (auto *M : C->getMembers()) {
    if (auto *F = llvm::dyn_cast<Field>(M)) {
      if (F->getProperties() & Field::In) {
        if (Args.size())
          append(Args, ", ");
        if (Init.size())
          append(Init, ", ");
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
  OS << "class " << C->getName();
  if (IsDerived)
    OS << " : public " << C->getSuperClass()->getName();
  OS << " {\n";
  Prot P = Private;
  if (NeedsKind) {
    emitFriend(OS, C);
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
      OS << "  enum " << E->getName() << " {\n";
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
    buildCtor(C, 0, Args, Init);
    emitProt(OS, P, Protected);
    OS << "  " << C->getName() << "(" << Args << ")";
    if (Init.size())
      OS << "\n    : " << Init;
    OS << " {}\n";
  }
  if (!IsBase) {
    llvm::SmallString<64> Args, Init;
    buildCtor(C, KindValues[C], Args, Init);
    emitProt(OS, P, Public);
    OS << "  " << C->getName() << "(" << Args << ")";
    if (Init.size())
      OS << "\n    : " << Init;
    OS << " {}\n";

    // Emit a default constructor for plain classes.
    if (C->getType() == Class::Plain && !Args.empty())
      OS << "  " << C->getName() << "() = default;\n";
  }
  emitProt(OS, P, Public);
  for (auto *M : C->getMembers()) {
    if (auto *F = llvm::dyn_cast<Field>(M)) {
      OS << "\n  " << getTypename(F, F->getProperties() & Field::In) << getRef(F)
         << (F->getTypeName() == "bool" ? "is" : "get") << F->getName()
         << "() {\n";
      OS << "    return " << getFieldname(F) << ";\n";
      OS << "  }\n";
      if (F->getProperties() != Field::In) {
        OS << "\n  void set" << F->getName() << "(" << getTypename(F, true)
           << getRef(F) << getFieldname(F) << ") {\n";
        OS << "    this->" << getFieldname(F) << " = " << getFieldname(F) << ";\n";
        OS << "  }\n";
      }
    }
  }
  if (IsDerived) {
    OS << "  static bool classof(const " << getBaseClass(C)->getName() << "* T) {\n";
    if (!HasSubclasses)
      OS << "    return T->" << KindMember << " == " << ~KindValues[C] << ";\n";
    else {
      unsigned Low = ~KindValues[IsBase ? C->getSubClasses()[0] : C];
      unsigned High = ~KindValues[C->getSubClasses().back()];
      assert(~Low && ~High && "Kind value must not be undefined");
      if (Low == High)
        OS << "    return T->" << KindMember << " == " << Low << ";\n";
      else
        OS << "    return T->" << KindMember << " >= " << Low << " && T->"
           << KindMember << " <= " << High << ";\n";
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
  for (auto Sub : C->getSubClasses()) {
    OS << "  friend class " << Sub->getName() << ";\n";
    emitFriend(OS, Sub);
  }
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
          OS << "class " << FieldType->getName() << ";\n";
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

std::string ClassEmitter::getTypename(Field *F, bool Const) {
  llvm::SmallString<16> MemberType;
  llvm::StringRef TypeName;
  auto IT = ASTDef.getClasses().find(F->getTypeName());
  if (IT != ASTDef.getClasses().end()) {
    MemberType.append(F->getTypeName());
    if (IT->second->getType() != Class::Plain)
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
  return llvm::formatv("{0}{1}", Prefix, F->getName()).str();
}

llvm::StringRef ClassEmitter::getRef(Field *F) {
  if (F->isTypeIsList())
    return " &";
  return " ";
}

namespace asttool {
void EmitClass(ASTDefinition &ASTDef, llvm::raw_ostream &OS) {
  ClassEmitter(ASTDef).run(OS);
}
} // namespace asttool
