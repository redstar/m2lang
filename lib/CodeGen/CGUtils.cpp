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
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Twine.h"

using namespace m2lang;

std::string utils::mangleName(const Declaration *Decl, const StringRef Suffix) {
  std::string Mangled("_m");
  llvm::SmallVector<llvm::StringRef, 4> Parts;
  for (; Decl; Decl = Decl->getEnclosingDecl())
    Parts.push_back(Decl->getName());
  while (!Parts.empty()) {
    llvm::StringRef Name = Parts.pop_back_val();
    Mangled.append(llvm::Twine(Name.size()).concat(Name).str());
  }
  if (!Suffix.empty())
    Mangled.append("_").append(Suffix);
  return Mangled;
}