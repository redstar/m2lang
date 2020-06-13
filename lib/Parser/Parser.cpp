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
#include "m2lang/Parser/Parser.h"
#include "m2lang/Basic/TokenKinds.h"

using namespace m2lang;

Parser::Parser(Lexer &Lex, Sema &Actions) : Impl(std::make_unique<M2Parser>(Lex, Actions)) {
}

Parser::Parser(Parser &&) noexcept = default;
Parser &Parser::operator=(Parser &&) noexcept = default;
Parser::~Parser() = default;

CompilationModule *Parser::parse() {
    return get()->parse();
}