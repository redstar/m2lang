//===--- Parser.cpp - LLtool parser -----------------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the LLtool parser class.
///
//===----------------------------------------------------------------------===//

#include "lltool/Parser.h"
#include "lltool/Node.h"
#include "lltool/VarStore.h"

namespace lltool {
using llvm::cast;
using llvm::SMLoc;
using llvm::StringRef;
} // namespace lltool

using namespace lltool;

void Parser::parse(Grammar &G, VarStore &V) {
  __TokenBitSet FollowSet{tok::eoi};
  parseLltool(FollowSet);
  G = Builder.build();
  V = Builder.varStore();
}

#define PARSER_DEFINITION
#include "lltool/lltool.inc"
#undef PARSER_DEFINITION
