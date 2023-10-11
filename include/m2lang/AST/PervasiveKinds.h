//===--- PervasiveKinds.h - Pervasive type enumeartion ----------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the enumeration for pervasive types and functions.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_AST_PERVASIVEKINDS_H
#define M2LANG_AST_PERVASIVEKINDS_H

namespace m2lang {

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

#endif