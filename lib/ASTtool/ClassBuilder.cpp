//===--- ClassBuilder.h - ASTtool builder class -----------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the ClassBuilder helper class.
///
//===----------------------------------------------------------------------===//

#include "asttool/ClassBuilder.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Casting.h"

using namespace asttool;

void ClassBuilder::error(llvm::SMLoc Loc, llvm::Twine Msg) {
  Diag.error(Loc, Msg);
}

void ClassBuilder::warning(llvm::SMLoc Loc, llvm::Twine Msg) {
  Diag.warning(Loc, Msg);
}

void ClassBuilder::note(llvm::SMLoc Loc, llvm::Twine Msg) {
  Diag.note(Loc, Msg);
}

void ClassBuilder::actOnLanguage(Identifier Name) {
  if (!languageName.empty()) {
    warning(Name.getLoc(),
            "Language is already defined. Ignoring new definition.");
    note(languageLoc, "Previous definition");
  } else {
    llvm::StringRef NameStr = Name.getString();
    std::string lang = NameStr.substr(1, NameStr.size() - 2).lower();
    if (lang != "c++") {
      warning(Name.getLoc(), llvm::Twine("Unknonw language ")
                                 .concat(NameStr)
                                 .concat(". Ignoring definition."));
      note(Name.getLoc(), "Valid values are: c++");
    } else {
      languageName = "c++";
      languageLoc = Name.getLoc();
    }
  }
}

void ClassBuilder::actOnTypedef(Identifier Name, llvm::StringRef Code) {
  if (Typedefs.find(Name.getString()) != Typedefs.end()) {
    error(Name.getLoc(),
          llvm::Twine("Typedef ")
              .concat(Name.getString())
              .concat(" already defined. Ignoring new definition."));
  }
  Typedefs[Name.getString()] = Code.substr(1, Code.size() - 2);
}

void ClassBuilder::finalizeTypedefs() {
  static const llvm::ArrayRef<llvm::StringLiteral> CPPTypes = {
      "bool", "char", "int", "short", "long", "unsigned", "float", "double"};
  for (const llvm::StringRef &Type : CPPTypes) {
    if (Typedefs.find(Type) != Typedefs.end())
      warning(llvm::SMLoc(),
              llvm::Twine("Overriding definition for C++ type ").concat(Type));
    else
      Typedefs[Type] = Type;
  }
}

static std::pair<Class *, Member *> lookupMember(Class *C,
                                                 llvm::StringRef Name) {
  while (C) {
    auto It =
        std::find_if(C->getMembers().begin(), C->getMembers().end(),
                     [&](Member *M) { return M->getName() == Name; });
    if (It != C->getMembers().end())
      return std::pair<Class *, Member *>(C, *It);
    C = C->getSuperClass();
  }
  return std::pair<Class *, Member *>(nullptr, nullptr);
}

void ClassBuilder::actOnTypedecl(Class::ClassType CType, Identifier Name,
                                 Class *SuperClass,
                                 MemberList &Body, LetList &LetDefinitions) {
  if (CType == Class::Plain && SuperClass) {
    error(Name.getLoc(),
          llvm::Twine("Plain classes do not support inheritance."));
    SuperClass = nullptr;
  }
  Class *C = Classes.lookup(Name.getString());
  if (!C) {
    C = new Class(CType, Name.getLoc(), Name.getString(), SuperClass, Body, LetDefinitions);
    auto Result = Classes.insert(
        std::pair<llvm::StringRef, Class *>(Name.getString(), C));
    assert(Result.second && "Insertion failed unexpected");
    if (SuperClass)
      SuperClass->getSubClasses().push_back(C);
  } else
    error(Name.getLoc(),
          llvm::Twine("Node ")
              .concat(Name.getString())
              .concat(" already defined. Ignoring new definition."));
}

void ClassBuilder::actOnField(llvm::SmallVectorImpl<Member *> &MemberList,
                              unsigned Properties, Identifier Name,
                              Identifier TypeName, bool TypeIsList,
                              bool IsDefault, llvm::StringRef Code) {
  unsigned Initializer = Field::None;
  if (IsDefault)
    Initializer = Field::Default;
  else if (!Code.empty())
    Initializer = Field::Code;
  MemberList.emplace_back(new Field(Name.getLoc(), Name.getString(), Properties,
                                    Initializer, TypeName.getString(),
                                    TypeIsList, Code.substr(1, Code.size() - 2)));
}

void ClassBuilder::actOnEnum(llvm::SmallVectorImpl<Member *> &MemberList,
                             Identifier Name, llvm::StringRef Code) {
  MemberList.emplace_back(new Enum(Name.getLoc(), Name.getString(),
                                   Code.substr(1, Code.size() - 2)));
}

void ClassBuilder::actOnSuperClass(Class *&SuperClass, Identifier Name) {
  SuperClass = Classes.lookup(Name.getString());
  if (!SuperClass)
    error(Name.getLoc(), llvm::Twine("Superclass ")
                             .concat(Name.getString())
                             .concat(" does not exist."));
}

void ClassBuilder::actOnLet(llvm::SmallVectorImpl<Let *> &LetList,
                            Identifier Name, Class *SuperClass, bool IsDefault,
                            llvm::StringRef Code) {
  std::pair<Class *, Member *> Result =
      lookupMember(SuperClass, Name.getString());
  if (Result.first) {
    if (auto *F = llvm::dyn_cast<Field>(Result.second))
      LetList.emplace_back(new Let(Name.getLoc(), Result.first, F,
                                   Code.substr(1, Code.size() - 2), IsDefault));
    else
      error(Name.getLoc(),
            llvm::Twine(Name.getString()).concat(" is not a field."));
  } else
    error(Name.getLoc(), llvm::Twine("No field ")
                             .concat(Name.getString())
                             .concat(" exists in superclasses."));
}

void ClassBuilder::actOnPropertyIn(unsigned &Properties, llvm::SMLoc Loc) {
  if (Properties & Field::In)
    warning(Loc, "Property %in already set");
  Properties |= Field::In;
}

void ClassBuilder::actOnPropertyOut(unsigned &Properties, llvm::SMLoc Loc) {
  if (Properties & Field::Out)
    warning(Loc, "Property %out already set");
  Properties |= Field::Out;
}

ASTDefinition ClassBuilder::build() {
  for (auto &C : Classes) {
    llvm::DenseMap<llvm::StringRef, Member *> Members;
    for (auto *M : C.second->getMembers()) {
      llvm::DenseMap<llvm::StringRef, Member *>::iterator I;
      bool Successful;
      std::tie(I, Successful) =
          Members.insert(std::pair<llvm::StringRef, Member *>(M->getName(), M));
      if (!Successful) {
        error(M->getLoc(), llvm::Twine("Name ")
                               .concat(M->getName())
                               .concat(" already declared."));
        if (I != Members.end())
          note(I->second->getLoc(), "See first declaration here.");
      }
    }
    for (auto *M : C.second->getMembers()) {
      if (auto *F = llvm::dyn_cast<Field>(M)) {
        // Lookup order for type:
        // Enums (local), Classes, Typedefs
        llvm::StringRef TypeName = F->getTypeName();
        Member *LocalMember = Members.lookup(TypeName);
        if (!llvm::isa_and_nonnull<Enum>(LocalMember) && !Classes.lookup(TypeName) &&
            Typedefs.lookup(TypeName).empty()) {
          error(F->getLoc(), llvm::Twine("Undefined type ").concat(TypeName));
        }
      }
    }
  }
  return ASTDefinition(Typedefs, Classes);
}
