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

#include "m2lang/Basic/LLVM.h"
#include "m2lang/Lexer/Lexer.h"
#include "llvm/ADT/SmallVector.h"

namespace m2lang {
class TargetInfo;

class Preprocessor {
  Lexer &Lex;

public:
  // State of current IF/ELSEIF/ELSE/END parsing.
  struct State {
    unsigned NextState : 1;  // 0 = Expect ELSEIF / ELSE / END, 1 = Expect END.
    unsigned Satisfied : 1;  // Condition was true somewhere.
    unsigned SyntaxOnly : 1; // Check only syntax.
    unsigned Skipping : 1;   // Current branch is skipped.

    State(bool Satisfied, bool SyntaxOnly, bool Skipping)
        : NextState(0), Satisfied(Satisfied), SyntaxOnly(SyntaxOnly),
          Skipping(Skipping) {}
  };
  using StateStack = SmallVector<State, 8>;

private:
  StateStack States;
  llvm::StringMap<StringRef> VersionTags = {
      {"m2lang", "TRUE"},     // This is the m2lang compiler.
      {"p1", "FALSE"},        // This is not the p1 compiler.
      {"StonyBrook", "FALSE"} // This is not the StonyBrook compiler.
  };

public:
  Preprocessor(Lexer &Lex, TargetInfo &TI);

  /// Returns the next token from the input.
  void next(Token &Tok);

  DiagnosticsEngine &getDiagnostics() { return Lex.getDiagnostics(); }

  const LangOptions &getLangOpts() const { return Lex.getLangOpts(); }

private:
  /// Handles compiler directives.
  void directive(Token &Tok);
};

} // end namespace m2lang

#endif
