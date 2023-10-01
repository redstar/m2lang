//===--- Scope.cpp - M2 Language Family Scoped Symbol Table -----*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements a scoped symbol table.
///
//===----------------------------------------------------------------------===//

#include "m2lang/AST/Scope.h"
#include "m2lang/AST/AST.h"

using namespace m2lang;
using llvm::StringMap;

bool Scope::insert(Declaration *Decl) {
  return Symbols
      .insert(std::pair<StringRef, Declaration *>(Decl->getName(), Decl))
      .second;
}

Declaration *Scope::lookup(StringRef Name) {
  Scope *S = this;
  while (S) {
    StringMap<Declaration *>::const_iterator I = S->Symbols.find(Name);
    if (I != S->Symbols.end())
      return I->second;
    S = S->getParent();
  }
  return nullptr;
}
