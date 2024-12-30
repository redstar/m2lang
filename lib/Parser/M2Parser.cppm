//===--- M2Parser.cppm - Modula-2 Language parser -------------------------===//
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

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SMLoc.h"

module m2lang.parser:M2Parser;

import m2lang.ast;
import m2lang.basic;
import m2lang.sema;
import m2lang.lexer;

namespace m2lang {

// TODO These are used in the grammar file.
using llvm::StringRef;
using llvm::SMLoc;

export class M2Parser {

  Preprocessor &PP;

  /// Actions - These are the callbacks we invoke as we parse various constructs
  /// in the file.
  Sema &Actions;

  /// Tok - The current token we are peeking ahead.  All parsing methods assume
  /// that this is valid.
  Token Tok;

  DiagnosticsEngine &getDiagnostics() const { return PP.getDiagnostics(); }

  /// nextToken - This peeks ahead one token and returns it without
  /// consuming it.
  const Token &nextToken() {
    PP.next(Tok);
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
    getDiagnostics().report(Tok.getLocation(), diag::err_unexpected_symbol);
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

#define M2PARSER_DECLARATION
#include "modula-2.inc"
#undef M2PARSER_DECLARATION

public:
  M2Parser(Preprocessor &PP, Sema &Actions);

  void initialize();

  const LangOptions &getLangOpts() const { return PP.getLangOpts(); }

  CompilationModule *parse() {
    __TokenBitSet Eof{tok::eof};
    CompilationModule *CM = nullptr;
    parseCompilationModule(Eof, CM);
    return CM;
  }
};
} // end namespace m2lang

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
