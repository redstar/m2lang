//===--- CGUnit.cppm - Code Generator for CUs -----------------------------===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the code generator implementation for complilation unita.
///
//===----------------------------------------------------------------------===//

module;

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include <cstdint>

export module m2lang.codegen:CGUnit;

import :CGTBAA;
import m2lang.ast;

namespace m2lang {
namespace utils {

export std::string mangleName(const Declaration *Decl,
                              const llvm::StringRef Suffix = "");

}

export class CGUnit {
protected:
  ASTContext &ASTCtx;
  llvm::Module *M;
  CGTBAA TBAA;

  // Repository of global objects.
  llvm::DenseMap<Declaration *, llvm::GlobalObject *> Globals;

  // Cache converted types.
  llvm::DenseMap<TypeDenoter *, llvm::Type *> TypeCache;

public:
  llvm::Type *VoidTy;
  llvm::Type *Int1Ty;
  llvm::Type *Int8Ty;
  llvm::Type *Int32Ty;
  llvm::Type *Int64Ty;
  llvm::Type *FloatTy;
  llvm::Type *DoubleTy;
  llvm::PointerType *PtrTy;
  llvm::Constant *Int32Zero;

public:
  CGUnit(ASTContext &ASTCtx, llvm::Module *M)
      : ASTCtx(ASTCtx), M(M), TBAA(M->getContext()) {
    initialize();
  }

  void initialize();

  llvm::LLVMContext &getLLVMCtx() { return M->getContext(); }
  llvm::Module *getModule() { return M; }

  ASTContext &getASTCtx() { return ASTCtx; }

  llvm::Type *convertType(TypeDenoter *TyDe);
  llvm::Type *convertType(Type *Ty);

  void decorateInst(llvm::Instruction *Inst, TypeDenoter *TyDe);

  llvm::GlobalObject *getGlobal(Declaration *Decl) {
    return Globals.lookup(Decl);
  }
};
} // namespace m2lang

using namespace m2lang;

std::string utils::mangleName(const Declaration *Decl,
                              const llvm::StringRef Suffix) {
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

void CGUnit::initialize() {
  VoidTy = llvm::Type::getVoidTy(getLLVMCtx());
  Int1Ty = llvm::Type::getInt1Ty(getLLVMCtx());
  Int8Ty = llvm::Type::getInt8Ty(getLLVMCtx());
  Int32Ty = llvm::Type::getInt32Ty(getLLVMCtx());
  Int64Ty = llvm::Type::getInt64Ty(getLLVMCtx());
  FloatTy = llvm::Type::getFloatTy(getLLVMCtx());
  DoubleTy = llvm::Type::getDoubleTy(getLLVMCtx());
  PtrTy = llvm::PointerType::get(getLLVMCtx(), /*AddressSpace=*/0);
  Int32Zero = llvm::ConstantInt::get(Int32Ty, 0, /*isSigned*/ true);
}

llvm::Type *CGUnit::convertType(TypeDenoter *TyDe) {
  auto *Cached = TypeCache.lookup(TyDe);
  if (Cached != nullptr)
    return Cached;
  if (auto *P = llvm::dyn_cast<PervasiveType>(TyDe)) {
    switch (P->getTypeKind()) {
    case pervasive::Void:
      return VoidTy;
    case pervasive::Boolean:
      return Int1Ty;
    case pervasive::Char:
      return Int8Ty;
    case pervasive::Cardinal:
    case pervasive::Integer:
    case pervasive::WholeNumber:
      return Int64Ty;
    case pervasive::Real:
      return FloatTy;
    case pervasive::LongReal:
    case pervasive::RealNumber:
      return DoubleTy;
    default:
      return Int32Ty;
    }
  }
  if (auto *Rec = llvm::dyn_cast<EnumerationType>(TyDe)) {
    // An enumeration is currently always mapped to an i64 type.
    // This must be in sync with Sema::actOnEnumerationType().
    TypeCache[TyDe] = Int64Ty;
    return Int64Ty;
  }
  if (auto *A = llvm::dyn_cast<ArrayType>(TyDe)) {
    llvm::Type *Component = convertType(A->getComponentType());
    // IndexType is an ordinal type.
    TypeDenoter *IndexType = A->getIndexType();
    uint64_t NumElements;
    if (auto *EnumTy = llvm::dyn_cast<EnumerationType>(IndexType)) {
      NumElements = EnumTy->getMembers().size();
    } else if (llvm::dyn_cast<SubrangeType>(IndexType)) {
      // For LLVM, we need to compute MAX(IndexType) - MIN(IndexType) + 1,
      // e.g. [1..5] has 5-1+1 = 5 elements.
      // TODO Implement. The challenge here is that getTo() and getFrom() are
      //      constant expressions, but the value is not available.
      NumElements = 5;
    } else {
      // A whole number type.
      // TODO Implement.
      NumElements = 6;
    }
    llvm::Type *Ty = llvm::ArrayType::get(Component, NumElements);
    TypeCache[TyDe] = Ty;
    return Ty;
  }
  if (auto *Rec = llvm::dyn_cast<RecordType>(TyDe)) {
    llvm::Type *Ty = llvm::StructType::get(getLLVMCtx(), false);
    // TODO Fill in members.
    TypeCache[TyDe] = Ty;
    return Ty;
  }
  if (auto *Ptr = llvm::dyn_cast<PointerType>(TyDe)) {
    TypeCache[TyDe] = PtrTy;
    return PtrTy;
  }
  // TODO Implement.
  return Int32Ty;
}

llvm::Type *CGUnit::convertType(Type *Ty) {
  return convertType(Ty->getTypeDenoter());
}

void CGUnit::decorateInst(llvm::Instruction *Inst, TypeDenoter *TyDe) {
  if (auto *N = TBAA.getAccessTagInfo(TyDe))
    Inst->setMetadata(llvm::LLVMContext::MD_tbaa, N);
}
