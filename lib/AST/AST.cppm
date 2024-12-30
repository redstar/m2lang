//===--- AST.cppm - Modula-2 Abstract Syntax Tree -------------------------===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the AST implementation.
///
//===----------------------------------------------------------------------===//

module;

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SMLoc.h"

export module m2lang.ast:AST;

import :PervasiveKinds;
import m2lang.basic;

export namespace m2lang {

class Constant;
class Declaration;
class Expression;
class FixedRecordField;
class FormalParameter;
class FormalParameterType;
class GuardedStatement;
class ImportItem;
class Selector;
class Statement;
class Type;
class TypeDenoter;
using ActualParameter = llvm::PointerUnion<Expression *, Type *>;

class Scope {
  Scope *Parent;
  llvm::StringMap<Declaration *> Symbols;

public:
  Scope(Scope *Parent = nullptr) : Parent(Parent) {}

  bool insert(Declaration *Declaration);
  Declaration *lookup(llvm::StringRef Name, bool SearchParent = true);

  Scope *getParent() { return Parent; }

  void dump() const;
};

// TODO Evaluate average size of these lists.
using ActualParameterList = llvm::SmallVector<ActualParameter, 4>;
using ConstantList = llvm::SmallVector<Constant *, 4>;
using DeclarationList = llvm::SmallVector<Declaration *, 4>;
using FormalParameterList = llvm::SmallVector<FormalParameter *, 4>;
using ExpressionList = llvm::SmallVector<Expression *, 4>;
using ImportItemList = llvm::SmallVector<ImportItem, 4>;
using SelectorList = llvm::SmallVector<Selector *, 4>;
using StatementList = llvm::SmallVector<Statement *, 4>;
using GuardedStatementList = llvm::SmallVector<GuardedStatement, 4>;
using TypeDenoterList = llvm::SmallVector<TypeDenoter *, 4>;

using FormalParameterTypeList = llvm::SmallVector<FormalParameterType, 4>;
using RecordFieldList = llvm::SmallVector<FixedRecordField, 4>;

using StringIndexMap = llvm::StringMap<uint64_t>;

class Identifier {
  llvm::SMLoc Loc;
  llvm::StringRef Name;

public:
  Identifier() = default;
  Identifier(llvm::SMLoc Loc, llvm::StringRef Name) : Loc(Loc), Name(Name) {}

  llvm::SMLoc getLoc() const { return Loc; }
  llvm::StringRef getName() const { return Name; }
};

using IdentifierList = llvm::SmallVector<Identifier, 4>;

using VariableIdentifierList =
    llvm::SmallVector<std::pair<Identifier, Expression *>, 4>;

class OperatorInfo {
  llvm::SMLoc Loc;
  uint32_t Kind : 16;
  uint32_t IsUnspecified : 1;

public:
  OperatorInfo() : Loc(), Kind(tok::unknown), IsUnspecified(true) {}
  OperatorInfo(llvm::SMLoc Loc, tok::TokenKind Kind, bool IsUnspecified = false)
      : Loc(Loc), Kind(Kind), IsUnspecified(IsUnspecified) {}

  llvm::SMLoc getLocation() const { return Loc; }
  tok::TokenKind getKind() const { return static_cast<tok::TokenKind>(Kind); }
  bool isUnspecified() const { return IsUnspecified; }
};

#define AST_DECLARATION
#define AST_DISPATCHER
#include "ast.inc"

} // namespace m2lang

namespace m2lang {
#define AST_DEFINITION
#include "ast.inc"
} // namespace m2lang

using namespace m2lang;

bool Scope::insert(Declaration *Decl) {
  return Symbols
      .insert(std::pair<llvm::StringRef, Declaration *>(Decl->getName(), Decl))
      .second;
}

Declaration *Scope::lookup(llvm::StringRef Name, bool SearchParent) {
  Scope *S = this;
  while (S) {
    llvm::StringMap<Declaration *>::const_iterator I = S->Symbols.find(Name);
    if (I != S->Symbols.end())
      return I->second;
    if (SearchParent)
      S = S->getParent();
    else
      break;
  }
  return nullptr;
}

void Scope::dump() const {
  llvm::dbgs() << "Scope<" << this << "> {\n";
  for (auto Key : Symbols.keys())
    llvm::dbgs() << "  " << Key << "\n";
  llvm::dbgs() << "}\n";
}
