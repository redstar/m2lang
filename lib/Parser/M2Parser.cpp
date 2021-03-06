//===--- parser.cpp - Modula-2 Language parser ------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the parser implementation.
///
//===----------------------------------------------------------------------===//

#include "M2Parser.h"
#include "m2lang/Basic/TokenKinds.h"

using namespace m2lang;

namespace {
template <typename T> T tokenAs(Token Tok) { return T(Tok); }

template <> Identifier tokenAs(Token Tok) {
  return Identifier(Tok.getLocation(), Tok.getIdentifier());
}

template <> OperatorInfo tokenAs(Token Tok) {
  return OperatorInfo(Tok.getLocation(), Tok.getKind());
}
} // namespace

M2Parser::M2Parser(Preprocessor &PP, Sema &Actions) : PP(PP), Actions(Actions) {
  nextToken();
}

void M2Parser::initialize() {}

#define M2PARSER_DEFINITION
#include "modula-2.inc"
#undef M2PARSER_DEFINITION
