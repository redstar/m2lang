//===--- PervasiveTypeKinds.cpp - Pervasive type enumeartion ----*- C++ -*-===//
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

#include "m2lang/AST/PervasiveKinds.h"

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
