//===--- ASTContext.cpp - M2 Language Family AST Context --------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the context class for the AST.
///
//===----------------------------------------------------------------------===//

#include "m2lang/AST/ASTContext.h"
#include "m2lang/AST/AST.h"

using namespace m2lang;

void ASTContext::initialize() {
#define BUILTIN_TYPE(Id) Id##TyDe = new (*this) PervasiveType(pervasive::Id);
#include "m2lang/AST/PervasiveTypes.def"
}
