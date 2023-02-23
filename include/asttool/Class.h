//===--- Class.h - ASTtool data type for classes ----------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the Class data structure.
///
//===----------------------------------------------------------------------===//

#ifndef ASTTOOL_CLASS_H
#define ASTTOOL_CLASS_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace asttool {

#define AST_DECLARATION
#include "asttool/ast.inc"

using MemberList = llvm::SmallVector<Member *, 4>;
using LetList = llvm::SmallVector<Let *, 4>;
} // namespace asttool
#endif
