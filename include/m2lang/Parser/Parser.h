//===--- Parser.h - Modula-2 Language Parser --------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the parser interface.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_PARSER_PARSER_H
#define M2LANG_PARSER_PARSER_H

#include "m2lang/Basic/Diagnostic.h"
#include "m2lang/Basic/LangOptions.h"
#include "m2lang/Lexer/Lexer.h"
#include "m2lang/Sema/Sema.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include <memory>

namespace m2lang {

class M2Parser;

class Parser {
  std::unique_ptr<M2Parser> Impl;

  const M2Parser *get() const { return Impl.get(); }

  M2Parser *get() { return Impl.get(); }

public:
  Parser(Lexer &Lex, Sema &Actions);
  ~Parser();
  Parser(Parser &&) noexcept = default;
  Parser(Parser const &) = delete;
  Parser &operator=(Parser &&) noexcept = default;
  Parser &operator=(Parser const &) = delete;

  CompilationModule *parse();
};
} // end namespace m2lang
#endif