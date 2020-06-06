//===--- RDPEmitter.h - LLtool recursive descent parser emitter -*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the emitter for LLtool.
///
//===----------------------------------------------------------------------===//

#ifndef LLTOOL_RDPEMITTER_H
#define LLTOOL_RDPEMITTER_H

namespace llvm {
class raw_ostream;
}
namespace lltool {
class Grammar;
class VarStore;

void EmitRDP(Grammar &grammar, VarStore &Vars, llvm::raw_ostream &OS);

} // namespace lltool
#endif