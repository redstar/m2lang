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

void ClassBuilder::actOnTypedecl(Class::ClassType CType, Identifier Name,
                                 llvm::StringRef Super,
                                 llvm::SmallVectorImpl<Member *> &Body) {
  if (!Super.empty() && Classes.find(Super) == Classes.end())
    error(Name.getLoc(),
          llvm::Twine("Superclass ").concat(Super).concat(" does not exist."));
  if (CType == Class::Plain && !Super.empty())
    error(Name.getLoc(),
          llvm::Twine("Plain classes do not support inheritance."));
  Class *C = new Class(CType, Name.getLoc(), Name.getString(), Super, Body);
  auto Result =
      Classes.insert(std::pair<llvm::StringRef, Class *>(Name.getString(), C));
  if (Result.second) {
    if (!Super.empty())
      Classes[Super]->getSubClasses().push_back(C);
  }
  else {
    delete C;
    error(Name.getLoc(),
          llvm::Twine("Node ")
              .concat(Name.getString())
              .concat(" already defined. Ignoring new definition."));
  }
}

void ClassBuilder::actOnField(llvm::SmallVectorImpl<Member *> &MemberList,
                              unsigned Properties, Identifier Name,
                              llvm::StringRef TypeName, bool TypeIsList) {
  MemberList.emplace_back(new Field(Name.getLoc(), Properties, Name.getString(),
                                    TypeName, TypeIsList));
}

void ClassBuilder::actOnEnum(llvm::SmallVectorImpl<Member *> &MemberList,
                             Identifier Name, llvm::StringRef Code) {
  MemberList.emplace_back(new Enum(Name.getLoc(), Name.getString(),
                                   Code.substr(1, Code.size() - 2)));
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
