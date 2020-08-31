//===--- CGTBAA.h - Type Based Alias Analysis Metadat -----------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Define the interface for adding TBAA metadata.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_CODEGEN_CGTBAA_H
#define M2LANG_CODEGEN_CGTBAA_H

#include "m2lang/AST/AST.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Metadata.h"

namespace m2lang {

class CGTBAA {
  // MDHelper - Helper for creating metadata.
  llvm::MDBuilder MDHelper;

  // The root node of the TBAA hierarchy
  llvm::MDNode *Root;

  llvm::DenseMap<TypeDenoter *, llvm::MDNode *> MetadataCache;

  llvm::MDNode *createScalarTypeNode(TypeDenoter *TyDe, StringRef Name,
                                     llvm::MDNode *Parent);

public:
  CGTBAA(llvm::LLVMContext &Ctx)
      : MDHelper(llvm::MDBuilder(Ctx)), Root(nullptr) {}

  llvm::MDNode *getRoot();
  llvm::MDNode *getTypeInfo(TypeDenoter *TyDe);
  llvm::MDNode *getAccessTagInfo(TypeDenoter *TyDe);
};

} // namespace m2lang
#endif