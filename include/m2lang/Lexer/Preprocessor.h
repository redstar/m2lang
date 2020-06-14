//===--- Preprocessor.h - Modula-2 Language Preprocessor --------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the Preprocessor interface.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_LEXER_PREPROCESSOR_H
#define M2LANG_LEXER_PREPROCESSOR_H

#include "m2lang/Lexer/Lexer.h"

namespace m2lang {

class Preprocessor {
  Lexer &Lex;

public:
  Preprocessor(Lexer &Lex) : Lex(Lex) {
    llvm::outs() << "File:\n" << Lex.getBuffer() << "\n----\n";
  }

  /// Returns the next token from the input.
  void next(Token &Tok);

  DiagnosticsEngine &getDiagnostics() { return Lex.getDiagnostics(); }

  const LangOptions &getLangOpts() const { return Lex.getLangOpts(); }

private:
  /// Handles compiler directives
  bool directive(Token &Tok);
};

} // end namespace m2lang

#endif
