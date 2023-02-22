//===--- VarStore.h - ASTtool variable container ----------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the variable container. Variables can be defined in the ASR file
/// with the %define directive. They are mainly used to customize the code
/// generation.
///
//===----------------------------------------------------------------------===//

#ifndef ASTTOOL_VARSTORE_H
#define ASTTOOL_VARSTORE_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Error.h"

namespace asttool {

namespace var {

enum VarName {
#define VAR(NAME, VAR, TYPE) VAR,
#include "asttool/Variables.def"
  NUM_VARIABLES
};

enum VarType { Identifier, Code, String, Flag };
} // namespace var

class VarStore {
  llvm::StringRef vars[var::NUM_VARIABLES];
  var::VarType getType(var::VarName) const;

public:
  VarStore();

  llvm::Error add(llvm::StringRef name, llvm::StringRef value,
                  var::VarType type);

  void set(var::VarName name, llvm::StringRef value);

  llvm::StringRef getVar(var::VarName name,
                         llvm::StringRef Default = "") const {
    return vars[name].empty() ? Default : vars[name];
  }

  bool getFlag(var::VarName name) const {
    assert(getType(name) == var::Flag && "getFlag() requires flag variable");
    return vars[name] == "true";
  }
};
} // namespace asttool
#endif
