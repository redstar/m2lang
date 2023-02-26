//===--- VarStore.cpp - LLtool variable container ---------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the variable container. Variables can be defined in zhe grammar file
/// with the %define directive. They are mainly used to customize the code
/// generation.
///
//===----------------------------------------------------------------------===//

#include "lltool/VarStore.h"

using namespace lltool;

namespace {
static const char *ExternalNames[] = {
#define VAR(NAME, VAR, TYPE, DEFAULT) NAME,
#include "lltool/Variables.def"
};
static var::VarType Types[] = {
#define VAR(NAME, VAR, TYPE, DEFAULT) var::TYPE,
#include "lltool/Variables.def"
};
static llvm::StringLiteral Defaults[] = {
#define VAR(NAME, VAR, TYPE, DEFAULT) DEFAULT,
#include "lltool/Variables.def"
};
} // namespace

var::VarType VarStore::getType(var::VarName Name) const {
  assert(Name != var::NUM_VARIABLES);
  return Types[Name];
}

llvm::StringRef VarStore::getDefault(var::VarName Name) const {
  assert(Name != var::NUM_VARIABLES);
  return Defaults[Name];
}

VarStore::VarStore() {}

llvm::Error VarStore::add(llvm::StringRef Name, llvm::StringRef Value,
                          var::VarType Type) {
  unsigned Idx = 0;
  for (; Idx < var::NUM_VARIABLES; ++Idx)
    if (ExternalNames[Idx] == Name)
      break;
  if (Idx >= var::NUM_VARIABLES) {
    return llvm::make_error<llvm::StringError>(
        llvm::Twine("unknown variable name ").concat(Name),
        llvm::inconvertibleErrorCode());
  }
  if (Types[Idx] != Type) {
    return llvm::make_error<llvm::StringError>("wrong variable type",
                                               llvm::inconvertibleErrorCode());
  }
  if (Type == var::Flag && (Value != "" && Value != "true")) {
    return llvm::make_error<llvm::StringError>(
        llvm::Twine("wrong value for flag variable ").concat(Name),
        llvm::inconvertibleErrorCode());
  }
  Vars[Idx] = Value;
  return llvm::Error::success();
}

void VarStore::set(var::VarName Name, llvm::StringRef Value) {
  assert(Name != var::NUM_VARIABLES);
  var::VarType Ty = Types[Name];
  assert(Ty != var::Flag || (Value == "" || Value == "true"));
  Vars[Name] = Value;
}
