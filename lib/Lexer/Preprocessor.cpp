//===--- Preprocessor.cpp - Modula-2 Language Preprocessor ------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the Preprocessor interface.
///
/// Tasks of the preprocesser are:
/// - Remove comments
/// - Implement conditional compiling
/// - Preprocess directives for use in parser
///
/// Conditional compiling uses the construct:
/// <* IF <bool-expr> THEN *>
/// <* ELSIF <bool-expr> THEN *>
/// <* ELSE *>
/// <* END *>
///
/// The boolean expression is over version tags. Tags can be defined with
/// <*DEFINE(TagName, value)*> and changed with <*ASSIGN(TagName, value)*>
/// where value can be a boolean expression.
/// Several version tags are predefined and they can also defined at the
/// command line.
/// This is similar to how Stony Brook Modula-2 and the Macintosh p1 compiler
/// handle directives, using the syntax from the ISO standard.
/// The alternate conditional compiling syntax from Stony Brook ("%IF") is not
/// supported.
///
/// Some directives are passed on to the parser. They are inspired by Modula-3:
/// - <*ASSERT bool-expr *>: valid at all places, where statements are allowed
/// - <*UNUSED*>: may precede any declaration
/// - <*OBSOLETE*>:  may precede any declaration
///
//===----------------------------------------------------------------------===//

#include "m2lang/Lexer/Preprocessor.h"
#include "llvm/ADT/StringMap.h"

using namespace m2lang;

namespace {
class DirectiveParser {
  Lexer &Lex;
  Token &Tok;
  const llvm::StringMap<bool> &VersionTags;
  Preprocessor::StateStack &States;
  bool IgnoreAdvance;

public:
  DirectiveParser(Lexer &Lex, Token &Tok,
                  const llvm::StringMap<bool> &VersionTags, Preprocessor::StateStack &States)
      : Lex(Lex), Tok(Tok), VersionTags(VersionTags), States(States),
        IgnoreAdvance(false) {
    assert(Tok.is(tok::lessstar) && "Current token must be '<*'");
  }

  void reset() { IgnoreAdvance = false; }

  void parse() {
    __TokenBitSet Eof{tok::eof};
    parseDirective(Eof);
  }

private:
  /// Called if source code is to be skipped.
  void skipUntilNextDirective() {
    IgnoreAdvance = true;
    while (!Tok.isOneOf(tok::eof, tok::lessstar))
      Lex.next(Tok);
  }

  void handleIf(SMLoc Loc, bool Val) {
    States.emplace_back(Val);
    // If Condition is false, then skip source until next directive.
    if (!Val)
      skipUntilNextDirective();
  }

  void handleElsif(SMLoc Loc, bool Val) {
    if (States.empty()) {
      Lex.getDiagnostics().report(Loc,
                                  diag::err_unexpected_elseif_in_directive);
      return;
    }
    Preprocessor::State &St = States.back();
    if (St.NextState != 0) {
      Lex.getDiagnostics().report(Loc,
                                  diag::err_unexpected_elseif_in_directive);
      return;
    }
    if (Val && !St.Satisfied) {
      // Condition is true and was not previously true, so include source
      St.Satisfied = true;
    } else {
      // Condition is false, skip source until next directive.
      skipUntilNextDirective();
    }
  }

  void handleElse(SMLoc Loc) {
    if (States.empty()) {
      Lex.getDiagnostics().report(Loc, diag::err_unexpected_else_in_directive);
      return;
    }
    Preprocessor::State &St = States.back();
    if (St.NextState != 0) {
      Lex.getDiagnostics().report(Loc,
                                  diag::err_unexpected_elseif_in_directive);
      return;
    }
    St.NextState = 1;
    // Condition was true, skip source until next directive.
    if (St.Satisfied)
      skipUntilNextDirective();
  }

  void handleEnd(SMLoc Loc) {
    if (States.empty())
      Lex.getDiagnostics().report(Loc, diag::err_unexpected_end_in_directive);
    else
      States.pop_back();
  }

  void handleDefine(StringRef Identifier, StringRef Value) {
    // TODO Implement
  }

  void handleAssign(StringRef Identifier, StringRef Value) {
    // TODO Implement
  }

  bool lookup(StringRef Identifier) {
    llvm::StringMap<bool>::const_iterator I = VersionTags.find(Identifier);
    if (I != VersionTags.end())
      return I->second;
    // Pervasive identifiers.
    if (Identifier == "TRUE")
      return true;
    if (Identifier == "FALSE")
      return false;
    // Nothing found. Assume false.
    Lex.getDiagnostics().report(Tok.getLocation(),
                                diag::warn_version_tag_not_found)
        << Identifier;
    return false;
  }

  void advance() {
    if (!IgnoreAdvance)
      Lex.next(Tok);
  }

  bool consume(tok::TokenKind ExpectedTok) {
    if (Tok.is(ExpectedTok)) {
      advance();
      return false;
    }
    error();
    return true;
  }

  bool expect(tok::TokenKind ExpectedTok) {
    if (Tok.is(ExpectedTok)) {
      return false;
    }
    return true;
  }

  void error() {
    Lex.getDiagnostics().report(Tok.getLocation(), diag::err_unexpected_symbol);
  }

#define DIRECTIVEPARSER_DECLARATION
#include "DirectiveParser.inc"
#undef DIRECTIVEPARSER_DECLARATION
};

#define DIRECTIVEPARSER_DEFINITION
#include "DirectiveParser.inc"
#undef DIRECTIVEPARSER_DEFINITION

} // namespace

void Preprocessor::next(Token &Tok) {
  do {
    Lex.next(Tok);
    if (Tok.is(tok::lessstar))
      directive(Tok);
  } while (Tok.is(tok::comment));
  if (Tok.is(tok::eof) && !States.empty()) {
    // Emit error message.
  }
}

void Preprocessor::directive(Token &Tok) {
  DirectiveParser DParser(Lex, Tok, VersionTags, States);
  while (Tok.is(tok::lessstar)) {
    DParser.reset();
    DParser.parse();
  }
}
