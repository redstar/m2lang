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

#ifndef M2LANG_AST_SCOPE_H
#define M2LANG_AST_SCOPE_H

#include "m2lang/Basic/LLVM.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"

namespace m2lang {

class Declaration;

class Scope {
  Scope *Parent;
  llvm::StringMap<Declaration *> Symbols;

public:
  Scope(Scope *Parent = nullptr) : Parent(Parent) {}

  bool insert(Declaration *Declaration);
  Declaration *lookup(StringRef Name);

  Scope *getParent() { return Parent; }
};
} // namespace tinylang
#endif