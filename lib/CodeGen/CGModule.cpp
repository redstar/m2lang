//===--- CGModule.cpp - Code Generator for Modules --------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the code generator implementation for modules.
///
//===----------------------------------------------------------------------===//

#include "m2lang/CodeGen/CGModule.h"
#include "m2lang/CodeGen/CGProcedure.h"
#include "m2lang/CodeGen/CGUtils.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

using namespace m2lang;

void CGModule::initialize() {
  VoidTy = llvm::Type::getVoidTy(getLLVMCtx());
  Int1Ty = llvm::Type::getInt1Ty(getLLVMCtx());
  Int8Ty = llvm::Type::getInt8Ty(getLLVMCtx());
  Int32Ty = llvm::Type::getInt32Ty(getLLVMCtx());
  Int64Ty = llvm::Type::getInt64Ty(getLLVMCtx());
  FloatTy = llvm::Type::getFloatTy(getLLVMCtx());
  DoubleTy = llvm::Type::getDoubleTy(getLLVMCtx());
  Int32Zero = llvm::ConstantInt::get(Int32Ty, 0, /*isSigned*/ true);
}

llvm::Type *CGModule::convertType(TypeDenoter *TyDe) {
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
  if (auto *A = llvm::dyn_cast<ArrayType>(TyDe)) {
    llvm::Type *Component = convertType(A->getComponentType());
    // IndexType is an ordinal type.
    // For LLVM, we need to compate MAX(IndexType) - MIN(IndexType) + 1,
    // e.g. [1..5] has 5-1+1 = 5 elements.
    uint64_t NumElements = 5; /* MAX(Idx) -> Min(Idx) + 1*/
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
    llvm::Type *Pointee = convertType(Ptr->getTyDen());
    llvm::Type *Ty = Pointee->getPointerTo();
    TypeCache[TyDe] = Ty;
    return Ty;
  }
  // TODO Implement.
  return Int32Ty;
}

llvm::Type *CGModule::convertType(Type *Ty) {
  return convertType(Ty->getTypeDenoter());
}

void CGModule::decorateInst(llvm::Instruction *Inst, TypeDenoter *TyDe) {
  if (auto *N = TBAA.getAccessTagInfo(TyDe))
    Inst->setMetadata(llvm::LLVMContext::MD_tbaa, N);
}

void CGModule::run(CompilationModule *CM) {
  this->CM = CM;
  ImplementationModule *PM = llvm::cast<ImplementationModule>(CM);
  for (auto *Decl : PM->getDecls()) {
    if (auto *Var = llvm::dyn_cast<Variable>(Decl)) {
      llvm::GlobalVariable *V = new llvm::GlobalVariable(
          *M, convertType(Var->getTypeDenoter()),
          /*isConstant=*/false, llvm::GlobalValue::PrivateLinkage, nullptr,
          utils::mangleName(Var));
      Globals[Var] = V;
    } else if (auto *Proc = llvm::dyn_cast<Procedure>(Decl)) {
      CGProcedure CGP(*this);
      CGP.run(Proc);
    }
  }
}
