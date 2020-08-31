//===--- CGTBAA.cpp - Type Based Alias Analysis Metadat ---------*- C++ -*-===//
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

#include "m2lang/CodeGen/CGTBAA.h"

using namespace m2lang;

llvm::MDNode *CGTBAA::getRoot() {
  if (!Root)
    Root = MDHelper.createTBAARoot("Simple Modula-2 TBAA");

  return Root;
}

llvm::MDNode *CGTBAA::createScalarTypeNode(TypeDenoter *TyDe, StringRef Name,
                                           llvm::MDNode *Parent) {
  llvm::MDNode *N = MDHelper.createTBAAScalarTypeNode(Name, Parent);
  return MetadataCache[TyDe] = N;
}

llvm::MDNode *CGTBAA::getTypeInfo(TypeDenoter *TyDe) {
  if (llvm::MDNode *N = MetadataCache[TyDe])
    return N;

  if (auto *Pervasive = llvm::dyn_cast<PervasiveType>(TyDe)) {
    StringRef Name = pervasive::getPervasiveTypeName(Pervasive->getTypeKind());
    return createScalarTypeNode(Pervasive, Name, getRoot());
  }
  if (auto *Enum = llvm::dyn_cast<EnumerationType>(TyDe)) {
    // TODO Implement
    StringRef Name = "enum";
    return createScalarTypeNode(Enum, Name, getRoot());
  }
  if (auto *Pointer = llvm::dyn_cast<PointerType>(TyDe)) {
    // TODO Implement
    StringRef Name = "any pointer";
    return createScalarTypeNode(Pointer, Name, getRoot());
  }
  if (auto *Array = llvm::dyn_cast<ArrayType>(TyDe)) {
    // TODO Implement
    StringRef Name = "array";
    return createScalarTypeNode(Array, Name, getRoot());
  }
  if (auto *Record = llvm::dyn_cast<RecordType>(TyDe)) {
    // TODO Implement
    StringRef Name = "array";
    return createScalarTypeNode(Record, Name, getRoot());
  }
  return nullptr;
}

llvm::MDNode *CGTBAA::getAccessTagInfo(TypeDenoter *TyDe) {
  if (auto *Pointer = llvm::dyn_cast<PointerType>(TyDe)) {
      assert(Pointer->isResolved() && "Pointer type is not resolved");
      return getTypeInfo(Pointer->getTyDen());
  }
  return nullptr;
}