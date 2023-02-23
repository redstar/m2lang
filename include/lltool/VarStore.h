//===--- VarStore.h - LLtool variable container -----------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the variable container. Variables can be defined in the grammar file
/// with the %define directive. They are mainly used to customize the code
/// generation.
///
//===----------------------------------------------------------------------===//

#ifndef LLTOOL_VARSTORE_H
#define LLTOOL_VARSTORE_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Error.h"

namespace lltool {

namespace var {

enum VarName {
#define VAR(NAME, VAR, TYPE) VAR,
#include "lltool/Variables.def"
  NUM_VARIABLES
};

enum VarType { Identifier, Code, String, Flag };
} // namespace var

class VarStore {
  llvm::StringRef Vars[var::NUM_VARIABLES];
  var::VarType getType(var::VarName) const;

public:
  VarStore();

  llvm::Error add(llvm::StringRef Name, llvm::StringRef Value,
                  var::VarType Type);

  void set(var::VarName Name, llvm::StringRef Value);

  llvm::StringRef getVar(var::VarName Name) const { return Vars[Name]; }

  bool getFlag(var::VarName Name) const {
    assert(getType(Name) == var::Flag && "getFlag() requires flag variable");
    return Vars[Name] == "true";
  }
};
} // namespace lltool
#endif
