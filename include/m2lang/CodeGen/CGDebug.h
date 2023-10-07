//===--- CGDebug.h - Debug information --------------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Adds debug information.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_CODEGEN_DEBUG_H
#define M2LANG_CODEGEN_DEBUG_H

#include "m2lang/AST/AST.h"
#include "llvm/IR/DIBuilder.h"

namespace m2lang {

class CGDebug {
  llvm::DIBuilder DBuilder;
public:
  CGDebug(llvm::Module &Mod)
      : DBuilder(Mod) {}

  void demo();
};

} // namespace m2lang
#endif