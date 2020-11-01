//===--- Parser.cpp - ASTtool parser ----------------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the ASTtool parser class.
///
//===----------------------------------------------------------------------===//

#include "asttool/Parser.h"

namespace asttool {
using llvm::cast;
using llvm::SMLoc;
using llvm::StringRef;
} // namespace asttool

using namespace asttool;

namespace {
template <typename T> T tokenAs(Token Tok) { return T(Tok); }

template <> Identifier tokenAs(Token Tok) {
  return Identifier(Tok.getLoc(), Tok.getData());
}
} // namespace

void Parser::parse() {
  __TokenBitSet FollowSet{tok::eoi};
  parseAsttool(FollowSet);
}

#define PARSER_DEFINITION
#include "asttool/asttool.inc"
#undef PARSER_DEFINITION
