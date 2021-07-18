//===--- ASTDefinition.h - Definition of AST --------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the class holding the complete definition of an AST.
///
//===----------------------------------------------------------------------===//

#ifndef ASTTOOL_ASTDEFINITION_H
#define ASTTOOL_ASTDEFINITION_H

#include "asttool/Class.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/MapVector.h"

namespace asttool {

class ASTDefinition {
  llvm::DenseMap<llvm::StringRef, llvm::StringRef> Typedefs;
  llvm::SmallMapVector<llvm::StringRef, Class *, 64> Classes;

public:
  ASTDefinition(llvm::DenseMap<llvm::StringRef, llvm::StringRef> Typedefs,
                llvm::SmallMapVector<llvm::StringRef, Class *, 64> Classes)
      : Typedefs(Typedefs), Classes(Classes) {}

  llvm::DenseMap<llvm::StringRef, llvm::StringRef> &getTypedefs() {
    return Typedefs;
  };
  llvm::SmallMapVector<llvm::StringRef, Class *, 64> &getClasses() {
    return Classes;
  };
};
} // namespace asttool
#endif
