//===--- CGTBAA.cppm - Type Based Alias Analysis Metadata -----------------===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Adds the definition of metadata for TBAA.
///
//===----------------------------------------------------------------------===//

module;

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Metadata.h"

export module m2lang.codegen:CGTBAA;

import m2lang.ast;

export namespace m2lang {

class CGTBAA {
  // MDHelper - Helper for creating metadata.
  llvm::MDBuilder MDHelper;

  // The root node of the TBAA hierarchy
  llvm::MDNode *Root;

  llvm::DenseMap<TypeDenoter *, llvm::MDNode *> MetadataCache;

  llvm::MDNode *createScalarTypeNode(TypeDenoter *TyDe, llvm::StringRef Name,
                                     llvm::MDNode *Parent);
  llvm::MDNode *createStructTypeNode(TypeDenoter *TyDe, llvm::StringRef Name,
                                     llvm::MDNode *Parent);

public:
  CGTBAA(llvm::LLVMContext &Ctx)
      : MDHelper(llvm::MDBuilder(Ctx)), Root(nullptr) {}

  llvm::MDNode *getRoot();
  llvm::MDNode *getTypeInfo(TypeDenoter *TyDe);
  llvm::MDNode *getAccessTagInfo(TypeDenoter *TyDe);
};

} // namespace m2lang

using namespace m2lang;

llvm::MDNode *CGTBAA::getRoot() {
  if (!Root)
    Root = MDHelper.createTBAARoot("Simple Modula-2 TBAA");

  return Root;
}

llvm::MDNode *CGTBAA::createScalarTypeNode(TypeDenoter *TyDe,
                                           llvm::StringRef Name,
                                           llvm::MDNode *Parent) {
  llvm::MDNode *N = MDHelper.createTBAAScalarTypeNode(Name, Parent);
  return MetadataCache[TyDe] = N;
}

llvm::MDNode *CGTBAA::createStructTypeNode(TypeDenoter *TyDe,
                                           llvm::StringRef Name,
                                           llvm::MDNode *Parent) {
  llvm::MDNode *N = MDHelper.createTBAAScalarTypeNode(Name, Parent);
  return MetadataCache[TyDe] = N;
}

llvm::MDNode *CGTBAA::getTypeInfo(TypeDenoter *TyDe) {
  if (llvm::MDNode *N = MetadataCache[TyDe])
    return N;

  if (auto *Pervasive = llvm::dyn_cast<PervasiveType>(TyDe)) {
    llvm::StringRef Name =
        pervasive::getPervasiveTypeName(Pervasive->getTypeKind());
    return createScalarTypeNode(Pervasive, Name, getRoot());
  }
  if (auto *Enum = llvm::dyn_cast<EnumerationType>(TyDe)) {
    // TODO Implement
    llvm::StringRef Name = "enum";
    return createScalarTypeNode(Enum, Name, getRoot());
  }
  if (auto *Pointer = llvm::dyn_cast<PointerType>(TyDe)) {
    // TODO Implement
    llvm::StringRef Name = "any pointer";
    return createScalarTypeNode(Pointer, Name, getRoot());
  }
  if (auto *Array = llvm::dyn_cast<ArrayType>(TyDe)) {
    // TODO Implement
    llvm::StringRef Name = "array";
    return createScalarTypeNode(Array, Name, getRoot());
  }
  if (auto *Record = llvm::dyn_cast<RecordType>(TyDe)) {
    // TODO Implement
    llvm::StringRef Name = "record";
    return createStructTypeNode(Record, Name, getRoot());
  }
  return nullptr;
}

llvm::MDNode *CGTBAA::getAccessTagInfo(TypeDenoter *TyDe) {
  if (auto *Pointer = llvm::dyn_cast<PointerType>(TyDe)) {
    return getTypeInfo(Pointer->getTyDen());
  }
  return nullptr;
}