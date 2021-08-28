//===--- ClassEmitter.h - ASTtool class source emitter ----------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the emitter for ASTtool.
///
//===----------------------------------------------------------------------===//

#ifndef ASTTOOL_CLASSEMITTER_H
#define ASTTOOL_CLASSEMITTER_H

namespace llvm {
class raw_ostream;
}
namespace asttool {
class ASTDefinition;

void EmitClass(ASTDefinition &ASTDef, llvm::raw_ostream &OS);

} // namespace asttool
#endif