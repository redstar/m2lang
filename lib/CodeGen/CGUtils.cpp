//===--- CGUtils.h - Code Generator Utility Functions -----------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements some utility functions.
///
//===----------------------------------------------------------------------===//

#include "m2lang/CodeGen/CGUtils.h"
#include "m2lang/AST/AST.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"

using namespace m2lang;

std::string utils::mangleName(Declaration *Decl) {
  std::string Mangled;
  llvm::SmallString<16> Tmp;
  while (Decl) {
    llvm::StringRef Name = Decl->getName();
    Tmp.clear();
    Tmp.append(llvm::itostr(Name.size()));
    Tmp.append(Name);
    Mangled.insert(0, Tmp.c_str());
    Decl = Decl->getEnclosingDecl();
  }
  Mangled.insert(0, "_m");
  return Mangled;
}