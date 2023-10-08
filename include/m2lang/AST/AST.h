//===--- AST.h - M2 Language Family Abstract Syntax Tree --------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the abstract syntax tree classes.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_AST_AST_H
#define M2LANG_AST_AST_H

#include "m2lang/AST/ASTContext.h"
#include "m2lang/AST/PervasiveTypeKinds.h"
#include "m2lang/Basic/LLVM.h"
#include "m2lang/Basic/TokenKinds.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include <string>
#include <vector>

namespace m2lang {

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

class Scope;

// TODO Evaluate average size of these lists.
using ActualParameterList = SmallVector<ActualParameter, 4>;
using ConstantList = llvm::SmallVector<Constant *, 4>;
using DeclarationList = SmallVector<Declaration *, 4>;
using FormalParameterList = SmallVector<FormalParameter *, 4>;
using ExpressionList = SmallVector<Expression *, 4>;
using ImportItemList = SmallVector<ImportItem, 4>;
using SelectorList = llvm::SmallVector<Selector *, 4>;
using StatementList = SmallVector<Statement *, 4>;
using GuardedStatementList = SmallVector<GuardedStatement, 4>;
using TypeDenoterList = SmallVector<TypeDenoter *, 4>;

using FormalParameterTypeList = llvm::SmallVector<FormalParameterType, 4>;
using RecordFieldList = SmallVector<FixedRecordField, 4>;

class Identifier {
  SMLoc Loc;
  StringRef Name;

public:
  Identifier() = default;
  Identifier(SMLoc Loc, StringRef Name) : Loc(Loc), Name(Name) {}

  SMLoc getLoc() const { return Loc; }
  StringRef getName() const { return Name; }
};

using IdentifierList = llvm::SmallVector<Identifier, 4>;

using VariableIdentifierList =
    llvm::SmallVector<std::pair<Identifier, Expression *>, 4>;

class OperatorInfo {
  SMLoc Loc;
  uint32_t Kind : 16;
  uint32_t IsUnspecified : 1;

public:
  OperatorInfo() : Loc(), Kind(tok::unknown), IsUnspecified(true) {}
  OperatorInfo(SMLoc Loc, tok::TokenKind Kind, bool IsUnspecified = false)
      : Loc(Loc), Kind(Kind), IsUnspecified(IsUnspecified) {}

  SMLoc getLocation() const { return Loc; }
  tok::TokenKind getKind() const { return static_cast<tok::TokenKind>(Kind); }
  bool isUnspecified() const { return IsUnspecified; }
};

#define AST_DECLARATION
#include "m2lang/AST/ast.inc"

} // namespace m2lang

#endif