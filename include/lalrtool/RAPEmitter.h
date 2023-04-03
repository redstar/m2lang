//===-- RAPEmitter.h - LALRtool recursive ascent parser emitter -*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the recursive ascent parser emitter for LALRtool.
///
//===----------------------------------------------------------------------===//

#ifndef LALRTOOL_RDPEMITTER_H
#define LALRTOOL_RDPEMITTER_H

namespace llvm {
class raw_ostream;
} // namespace llvm
namespace lalrtool {
class Grammar;
class LR0Automaton;
class VarStore;

void emitRAP(const Grammar &G, const LR0Automaton &LR0, VarStore &Vars,
             llvm::raw_ostream &OS);

} // namespace lalrtool
#endif