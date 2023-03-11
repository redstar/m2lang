//===--- Parser.cpp - LALRtool parser ---------------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the LALRtool parser class.
///
//===----------------------------------------------------------------------===//

#include "lalrtool/Parser.h"
#include "lalrtool/Grammar.h"
#include "lalrtool/VarStore.h"

namespace lalrtool {
using llvm::cast;
using llvm::SMLoc;
using llvm::StringRef;
} // namespace lalrtool

using namespace lalrtool;

void Parser::parse(Grammar &G, VarStore &V) {
  _TokenBitSet FollowSet{tok::eoi};
  parseLalrtool(FollowSet);
  G = Builder.build();
  V = Builder.varStore();
}

#define PARSER_DEFINITION
#include "lalrtool/lalrtool.g.inc"
#undef PARSER_DEFINITION
