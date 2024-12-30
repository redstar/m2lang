//===--- Parser.cppm - Modula-2 Language parser ---------------------------===//
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

module;

#include <memory>

export module m2lang.parser;

import :M2Parser;
import m2lang.ast;
import m2lang.lexer;
import m2lang.sema;

namespace m2lang {
export class Parser {
  std::unique_ptr<M2Parser> Impl;

  const M2Parser *get() const { return Impl.get(); }

  M2Parser *get() { return Impl.get(); }

public:
  Parser(Preprocessor &PP, Sema &Actions);
  ~Parser();
  Parser(Parser &&) noexcept;
  Parser(Parser const &) = delete;
  Parser &operator=(Parser &&) noexcept;
  Parser &operator=(Parser const &) = delete;

  CompilationModule *parse();
};
} // end namespace m2lang

using namespace m2lang;

Parser::Parser(Preprocessor &PP, Sema &Actions) : Impl(std::make_unique<M2Parser>(PP, Actions)) {
}

Parser::Parser(Parser &&) noexcept = default;
Parser &Parser::operator=(Parser &&) noexcept = default;
Parser::~Parser() = default;

CompilationModule *Parser::parse() {
    return get()->parse();
}