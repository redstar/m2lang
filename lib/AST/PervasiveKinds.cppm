//===--- PervasiveKinds.cppm - Pervasive type enumerations ----------------===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Utility functions for pervasive types.
///
//===----------------------------------------------------------------------===//

export module m2lang.ast:PervasiveKinds;

export namespace m2lang {

namespace pervasive {
enum PervasiveTypeKind {
#define BUILTIN_TYPE(Id) Id,
#include "m2lang/AST/PervasiveTypes.def"
};

const char *getPervasiveTypeName(PervasiveTypeKind Kind);

enum PervasiveFunctionKind {
#define PROCEDURE(Name) Proc_##Name,
#define FUNCTION(Name) Func_##Name,
#include "m2lang/AST/PervasiveFunctions.def"
};

const char *getPervasiveFunctionName(PervasiveFunctionKind Kind);

} // namespace pervasive

} // namespace m2lang

using namespace m2lang;

const char *pervasive::getPervasiveTypeName(PervasiveTypeKind Kind) {
  switch (Kind) {
#define PERVASIVE_TYPE(Id, Name)                                               \
  case pervasive::Id:                                                          \
    return #Name;
#include "m2lang/AST/PervasiveTypes.def"
  default:
    return "";
  }
}

const char *pervasive::getPervasiveFunctionName(PervasiveFunctionKind Kind) {
  switch (Kind) {
#define PROCEDURE(Name)                                                        \
  case pervasive::Proc_##Name:                                                 \
    return #Name;
#define FUNCTION(Name)                                                         \
  case pervasive::Func_##Name:                                                 \
    return #Name;
#include "m2lang/AST/PervasiveFunctions.def"
  }
}
