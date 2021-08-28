//===--- VarStore.cpp - ASTtool variable container --------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the variable container. Variables can be defined in the AST file
/// with the %define directive. They are mainly used to customize the code
/// generation.
///
//===----------------------------------------------------------------------===//

#include "asttool/VarStore.h"

using namespace asttool;

namespace {
static const char *ExternalNames[] = {
#define VAR(NAME, VAR, TYPE) NAME,
#include "asttool/Variables.def"
};
static var::VarType Types[] = {
#define VAR(NAME, VAR, TYPE) var::TYPE,
#include "asttool/Variables.def"
};
} // namespace

var::VarType VarStore::getType(var::VarName name) const {
  assert(name != var::NUM_VARIABLES);
  return Types[name];
}

VarStore::VarStore() {}

llvm::Error VarStore::add(llvm::StringRef name, llvm::StringRef value,
                          var::VarType type) {
  unsigned idx = 0;
  for (; idx < var::NUM_VARIABLES; ++idx)
    if (ExternalNames[idx] == name)
      break;
  if (idx >= var::NUM_VARIABLES) {
    return llvm::make_error<llvm::StringError>(
        llvm::Twine("unknown variable name ").concat(name),
        llvm::inconvertibleErrorCode());
  }
  if (Types[idx] != type) {
    return llvm::make_error<llvm::StringError>("wrong variable type",
                                               llvm::inconvertibleErrorCode());
  }
  if (type == var::Flag && (value != "" && value != "true")) {
    return llvm::make_error<llvm::StringError>(
        llvm::Twine("wrong value for flag variable ").concat(name),
        llvm::inconvertibleErrorCode());
  }
  vars[idx] = value;
  return llvm::Error::success();
}

void VarStore::set(var::VarName name, llvm::StringRef value) {
  assert(name != var::NUM_VARIABLES);
  var::VarType Ty = Types[name];
  assert(Ty != var::Flag || (value == "" || value == "true"));
  vars[name] = value;
}
