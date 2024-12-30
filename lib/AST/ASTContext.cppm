//===--- ASTContext.cppm - M2 Language Family AST Context -----------------===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the context class for the AST.
///
//===----------------------------------------------------------------------===//

module;

#include "llvm/Support/Allocator.h"
#include "llvm/Support/SourceMgr.h"
#include <cstddef> // For size_t,

export module m2lang.ast:ASTContext;

import :AST;
import m2lang.basic;

export {

namespace m2lang {
class ASTContext;
}

void *operator new(size_t Bytes, const m2lang::ASTContext &C,
                   size_t Alignment = 8);
void operator delete(void *Ptr, const m2lang::ASTContext &C, size_t);

void *operator new[](size_t Bytes, const m2lang::ASTContext &C,
                     size_t Alignment = 8);
void operator delete[](void *Ptr, const m2lang::ASTContext &C, size_t);

namespace m2lang {

class ASTContext {
  /// The language options used to create the AST associated with
  ///  this ASTContext object.
  LangOptions &LangOpts;

  /// The associated SourceManager object.
  llvm::SourceMgr &SrcMgr;

  /// The allocator used to create AST objects.
  ///
  /// AST objects are never destructed; rather, all memory associated with the
  /// AST objects will be released when the ASTContext itself is destroyed.
  mutable llvm::BumpPtrAllocator BumpAlloc;

public:
#define BUILTIN_TYPE(Id) PervasiveType *Id##TyDe;
#include "m2lang/AST/PervasiveTypes.def"

public:
  ASTContext(LangOptions &LangOpts, llvm::SourceMgr &SrcMgr)
      : LangOpts(LangOpts), SrcMgr(SrcMgr) {
    initialize();
  }
  void initialize();

  const LangOptions &getLangOpts() const { return LangOpts; }

  llvm::SourceMgr &getSourceMgr() { return SrcMgr; }
  const llvm::SourceMgr &getSourceMgr() const { return SrcMgr; }

  llvm::BumpPtrAllocator &getAllocator() const {
    return BumpAlloc;
  }

  void *Allocate(size_t Size, unsigned Align = 8) const {
    return BumpAlloc.Allocate(Size, Align);
  }
  template <typename T> T *Allocate(size_t Num = 1) const {
    return static_cast<T *>(Allocate(Num * sizeof(T), alignof(T)));
  }
  void Deallocate(void *Ptr) const {}

  /// Return the total amount of physical memory allocated for representing
  /// AST nodes and type information.
  size_t getASTAllocatedMemory() const {
    return BumpAlloc.getTotalMemory();
  }
};

} // namespace m2lang

// operator new and delete aren't allowed inside namespaces.

/// Placement new for using the ASTContext's allocator.
///
/// This placement form of operator new uses the ASTContext's allocator for
/// obtaining memory.
///
/// IMPORTANT: These are also declared in m2lang/AST/ASTContextAllocate.h!
/// Any changes here need to also be made there.
///
/// We intentionally avoid using a nothrow specification here so that the calls
/// to this operator will not perform a null check on the result -- the
/// underlying allocator never returns null pointers.
///
/// Usage looks like this (assuming there's an ASTContext 'Context' in scope):
/// @code
/// // Default alignment (8)
/// IntegerLiteral *Ex = new (Context) IntegerLiteral(arguments);
/// // Specific alignment
/// IntegerLiteral *Ex2 = new (Context, 4) IntegerLiteral(arguments);
/// @endcode
/// Memory allocated through this placement new operator does not need to be
/// explicitly freed, as ASTContext will free all of this memory when it gets
/// destroyed. Please note that you cannot use delete on the pointer.
///
/// @param Bytes The number of bytes to allocate. Calculated by the compiler.
/// @param C The ASTContext that provides the allocator.
/// @param Alignment The alignment of the allocated memory (if the underlying
///                  allocator supports it).
/// @return The allocated memory. Could be nullptr.
inline void *operator new(size_t Bytes, const m2lang::ASTContext &C,
                          size_t Alignment /* = 8 */) {
  return C.Allocate(Bytes, Alignment);
}

/// Placement delete companion to the new above.
///
/// This operator is just a companion to the new above. There is no way of
/// invoking it directly; see the new operator for more details. This operator
/// is called implicitly by the compiler if a placement new expression using
/// the ASTContext throws in the object constructor.
inline void operator delete(void *Ptr, const m2lang::ASTContext &C, size_t) {
  C.Deallocate(Ptr);
}

/// This placement form of operator new[] uses the ASTContext's allocator for
/// obtaining memory.
///
/// We intentionally avoid using a nothrow specification here so that the calls
/// to this operator will not perform a null check on the result -- the
/// underlying allocator never returns null pointers.
///
/// Usage looks like this (assuming there's an ASTContext 'Context' in scope):
/// @code
/// // Default alignment (8)
/// char *data = new (Context) char[10];
/// // Specific alignment
/// char *data = new (Context, 4) char[10];
/// @endcode
/// Memory allocated through this placement new[] operator does not need to be
/// explicitly freed, as ASTContext will free all of this memory when it gets
/// destroyed. Please note that you cannot use delete on the pointer.
///
/// @param Bytes The number of bytes to allocate. Calculated by the compiler.
/// @param C The ASTContext that provides the allocator.
/// @param Alignment The alignment of the allocated memory (if the underlying
///                  allocator supports it).
/// @return The allocated memory. Could be nullptr.
inline void *operator new[](size_t Bytes, const m2lang::ASTContext& C,
                            size_t Alignment /* = 8 */) {
  return C.Allocate(Bytes, Alignment);
}

/// Placement delete[] companion to the new[] above.
///
/// This operator is just a companion to the new[] above. There is no way of
/// invoking it directly; see the new[] operator for more details. This operator
/// is called implicitly by the compiler if a placement new[] expression using
/// the ASTContext throws in the object constructor.
inline void operator delete[](void *Ptr, const m2lang::ASTContext &C, size_t) {
  C.Deallocate(Ptr);
}
} // of export

using namespace m2lang;

void ASTContext::initialize() {
#define BUILTIN_TYPE(Id) Id##TyDe = new (*this) PervasiveType(pervasive::Id);
#include "m2lang/AST/PervasiveTypes.def"
}
