//===--- M2Parser.h - Modula-2 Language Parser ------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the internal parser interface.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_PARSER_M2PARSER_H
#define M2LANG_PARSER_M2PARSER_H

#include "m2lang/Basic/Diagnostic.h"
#include "m2lang/Basic/LangOptions.h"
#include "m2lang/Lexer/Lexer.h"
#include "m2lang/Sema/Sema.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

namespace m2lang {
class M2Parser {

  Lexer &Lex;

  /// Actions - These are the callbacks we invoke as we parse various constructs
  /// in the file.
  Sema &Actions;

  /// Tok - The current token we are peeking ahead.  All parsing methods assume
  /// that this is valid.
  Token Tok;

  DiagnosticsEngine &getDiagnostics() const { return Lex.getDiagnostics(); }

  /// NextToken - This peeks ahead one token and returns it without
  /// consuming it.
  const Token &nextToken() {
    Lex.next(Tok);
    StringRef str = StringRef(Tok.getLocation().getPointer(), Tok.getLength());
    llvm::outs() << "Token: " << Tok.getName() << ": '" << str << "'\n";
    return Tok;
  }

  SMLoc consumeToken() {
    SMLoc PrevLoc = Tok.getLocation();
    nextToken();
    return PrevLoc;
  }

  void consumeAnyToken() { nextToken(); }

  void consumeSemi() {}

  /// Expects and consume the token.
  /// Returns true in case of syntax error
  bool expectAndConsume(tok::TokenKind ExpectedTok, llvm::StringRef Msg = "") {
    if (Tok.is(ExpectedTok)) {
      consumeToken();
      return false;
    }
    // There must be a better way!
    const char *Expected = tok::getPunctuatorSpelling(ExpectedTok);
    if (!Expected)
      Expected = tok::getKeywordSpelling(ExpectedTok);
    StringRef Actual =
        StringRef(Tok.getLocation().getPointer(), Tok.getLength());
    getDiagnostics().report(Tok.getLocation(), diag::err_expected)
        << Expected << Actual;
    return true;
  }

  void error() {
    // TODO Output error message
  }

  void advance() { nextToken(); }
  bool consume(tok::TokenKind ExpectedTok) {
    return expectAndConsume(ExpectedTok);
  }
  bool expect(tok::TokenKind ExpectedTok) {
    if (Tok.is(ExpectedTok)) {
      return false;
    }
    return true;
  }

  //===----------------------------------------------------------------------===//
  // Error recovery.
  //===----------------------------------------------------------------------===//

  /// Control flags for SkipUntil functions.
  enum SkipUntilFlags {
    StopAtSemi = 1 << 0, ///< Stop skipping at semicolon
    /// Stop skipping at specified token, but don't skip the token itself
    StopBeforeMatch = 1 << 1
  };

  static bool hasFlagsSet(SkipUntilFlags L, SkipUntilFlags R) {
    return (static_cast<unsigned>(L) & static_cast<unsigned>(R)) != 0;
  }

  bool skipUntil(tok::TokenKind T,
                 SkipUntilFlags Flags = static_cast<SkipUntilFlags>(0)) {
    while (true) {
      if (Tok.is(T)) {
        if (hasFlagsSet(Flags, StopBeforeMatch)) {
          // Noop, don't consume the token.
        } else {
          consumeAnyToken();
        }
        return true;
      }

      // Important special case: The caller has given up and just wants us to
      // skip the rest of the file. Do this without recursing, since we can
      // get here precisely because the caller detected too much recursion.
      if (T == tok::eof && !hasFlagsSet(Flags, StopAtSemi)) {
        while (Tok.isNot(tok::eof))
          consumeAnyToken();
        return true;
      }
      switch (Tok.getKind()) {
      case tok::eof:
        // Ran out of tokens.
        return false;
      case tok::semi:
        if (hasFlagsSet(Flags, StopAtSemi))
          return false;
        LLVM_FALLTHROUGH;
      default:
        // Skip this token.
        consumeAnyToken();
        break;
      }
    }
  }

#define M2PARSER_DECLARATION
#include "modula-2.inc"
#undef M2PARSER_DECLARATION

public:
  M2Parser(Lexer &Lex, Sema &Actions);

  void initialize();

  const LangOptions &getLangOpts() const { return Lex.getLangOpts(); }

  void parse() {
    __TokenBitSet Eof{tok::eof};
    parseCompilationModule(Eof);
  }
};
} // end namespace m2lang
#endif