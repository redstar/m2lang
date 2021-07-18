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
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/raw_ostream.h"

using namespace asttool;

namespace {
class ClassEmitter {
  ASTDefinition &ASTDef;

  // Text fragments
  std::string GuardDeclaration;
  std::string GuardDefinition;
  llvm::StringRef ListType;
  llvm::StringRef ConstListType;

  enum Prot { Public, Protected, Private };

public:
  ClassEmitter(ASTDefinition &ASTDef) : ASTDef(ASTDef) { initialize(); }
  void run(llvm::raw_ostream &OS);

private:
  void initialize();
  void emitClass(llvm::raw_ostream &OS, Class *C);
  void emitProt(llvm::raw_ostream &OS, Prot &Current, Prot Requested);

  Class *getBaseClass(Class *C);
  std::string getTypename(Field *F, bool Const = false);
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
  ConstListType = "const llvm::SmallVector<{0}, 4> &";
}

void ClassEmitter::emitClass(llvm::raw_ostream &OS, Class *C) {
  bool IsDerived = C->getType() == Class::Node && !C->getSuperClass().empty();
  OS << "class " << C->getName();
  if (IsDerived)
    OS << " : public " << C->getSuperClass();
  OS << " {\n";
  Prot P = Private;
  if (C->getType() == Class::Base) {
    emitProt(OS, P, Protected);
    OS << "  const unsigned __Kind;\n";
  }
  for (auto *M : C->getMembers()) {
    if (auto *F = llvm::dyn_cast<Field>(M)) {
      emitProt(OS, P, Private);
      OS << "  " << getTypename(F);
      OS << " " << F->getName() << ";\n";
    } else if (auto *E = llvm::dyn_cast<Enum>(M)) {
      emitProt(OS, P, Public);
      OS << "  enum " << E->getName() << " {\n";
      OS << "    " << E->getCode() << "\n";
      OS << "  };\n";
    } else
      llvm_unreachable("Unknown member type");
  }
  OS << "\n";
  emitProt(OS, P, Public);
  llvm::SmallString<64> Args, Init;
  for (auto *M : C->getMembers()) {
    if (auto *F = llvm::dyn_cast<Field>(M)) {
      if (F->getProperties() & Field::In) {
        if (Args.size()) {
          Args.append(", ");
          Init.append(", ");
        }
        Args.append(getTypename(F, true) /*.str()*/);
        Args.append(" ");
        Args.append(F->getName());
        Init.append(F->getName());
        Init.append("(");
        Init.append(F->getName());
        Init.append(")");
      }
    }
  }
  emitProt(OS, P, Public);
  OS << "  " << C->getName() << "(" << Args << ")";
  if (Init.size())
    OS << "\n    : " << Init;
  OS << " {}\n";
  for (auto *M : C->getMembers()) {
    if (auto *F = llvm::dyn_cast<Field>(M)) {
      OS << "  " << getTypename(F, true) << " "
         << (F->getTypeName() == "bool" ? "is" : "get") << F->getName()
         << "() {\n";
      OS << "    return " << F->getName() << ";\n";
      OS << "  }\n";
      if (!(F->getProperties() & Field::In)) {
        OS << "  void set" << F->getName() << "(" << getTypename(F, true) << " "
           << F->getName() << ") {\n";
        OS << "    this." << F->getName() << " " << F->getName() << ";\n";
        OS << "  }\n";
      }
    }
  }
  if (IsDerived) {
    OS << "  static bool classof(" << getBaseClass(C)->getName() << "* T) {\n";
    OS << "    T->__Kind == ???;\n";
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

Class *ClassEmitter::getBaseClass(Class *C) {
  while (!C->getSuperClass().empty()) {
    C = ASTDef.getClasses()[C->getSuperClass()];
  }
  return C;
}

std::string ClassEmitter::getTypename(Field *F, bool Const) {
  llvm::SmallString<16> MemberType;
  llvm::StringRef TypeName;
  if (ASTDef.getClasses().find(F->getTypeName()) != ASTDef.getClasses().end()) {
    MemberType.append(F->getTypeName());
    MemberType.append(" *");
    TypeName = MemberType.str();
  } else {
    TypeName = ASTDef.getTypedefs()[F->getTypeName()];
    // Must be name of enum if not found.
    // TODO Is there a need to define a lookup rule?
    if (TypeName.empty())
      TypeName = F->getTypeName();
  }
  const char *Fmt = F->getTypeIsList()
                        ? (Const ? ConstListType.data() : ListType.data())
                        : "{0}";
  return llvm::formatv(Fmt, TypeName).str();
}

namespace asttool {
void EmitClass(ASTDefinition &ASTDef, llvm::raw_ostream &OS) {
  ClassEmitter(ASTDef).run(OS);
}
} // namespace asttool
