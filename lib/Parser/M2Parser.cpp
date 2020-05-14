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

M2Parser::M2Parser(Lexer &Lex, Sema &Actions) : Lex(Lex), Actions(Actions) {
  llvm::outs() << "File:\n" << Lex.getBuffer() << "\n----\n";
  nextToken();
}

void M2Parser::initialize() {}

#define M2PARSER_DEFINITION
#include "modula-2.inc"
#undef M2PARSER_DEFINITION
