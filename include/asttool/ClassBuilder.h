//===--- ClassBuilder.h - ASTtool builder class -----------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the ClassBuilder helper class.
///
//===----------------------------------------------------------------------===//

#ifndef ASTTOOL_CLASSBUILDER_H
#define ASTTOOL_CLASSBUILDER_H

#include "asttool/ASTDefinition.h"
#include "asttool/Diagnostic.h"
#include "asttool/VarStore.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace asttool {

class ClassBuilder {
private:
  Diagnostic &Diag;

  llvm::StringRef languageName;
  llvm::SMLoc languageLoc;

  llvm::DenseMap<llvm::StringRef, llvm::StringRef> Typedefs;

  llvm::SmallMapVector<llvm::StringRef, Class *, 64> Classes;

  VarStore variables;

  void error(llvm::SMLoc loc, llvm::Twine msg);
  void warning(llvm::SMLoc loc, llvm::Twine msg);
  void note(llvm::SMLoc loc, llvm::Twine msg);

public:
  ClassBuilder(Diagnostic &Diag) : Diag(Diag) {}
  ASTDefinition build();
  const VarStore &varStore() { return variables; }

  void actOnLanguage(Identifier Name);
  void actOnTypedef(Identifier Name, llvm::StringRef Code);
  void finalizeTypedefs();
  void actOnTypedecl(Class::ClassType CType, Identifier Name, Class *SuperClass,
                     MemberList &Body, LetList &LetDefintions);
  void actOnField(llvm::SmallVectorImpl<Member *> &MemberList,
                  unsigned Properties, Identifier Name, Identifier TypeName,
                  bool TypeIsList, bool IsDefault, llvm::StringRef Code);
  void actOnEnum(llvm::SmallVectorImpl<Member *> &MemberList, Identifier Name,
                 llvm::StringRef Code);
  void actOnLet(llvm::SmallVectorImpl<Let *> &LetList, Identifier Name,
                Class *SuperClass, bool IsDefault, llvm::StringRef Code);
  void actOnSuperClass(Class *&SuperClass, Identifier Name);
  void actOnPropertyIn(unsigned &Properties, llvm::SMLoc Loc);
  void actOnPropertyOut(unsigned &Properties, llvm::SMLoc Loc);
  void actOnDefine(const llvm::SMLoc loc, llvm::StringRef name,
              llvm::StringRef value, var::VarType type);
};
} // namespace asttool
#endif
