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
/// The grammar is based on the draft technical report "Interfacing Modula-2 to
/// C", Annex B:
/// http://www.zi.biologie.uni-muenchen.de/~enger/SC22WG13/im2c-981130.html#TR-AXI-PRAGMAS
/// and is compatible to the Macintosh p1 compiler,
/// https://modula2.awiedemann.de/manual/comp4.html#L4_2
///
//===----------------------------------------------------------------------===//

#include "m2lang/Lexer/Preprocessor.h"
#include "llvm/ADT/StringMap.h"

using namespace m2lang;

namespace {
class DirectiveParser {
  Lexer &Lex;
  Token &Tok;
  llvm::StringMap<StringRef> &VersionTags;
  Preprocessor::StateStack &States;
  bool IgnoreAdvance;

  static const StringRef TRUE;
  static const StringRef FALSE;

public:
  DirectiveParser(Lexer &Lex, Token &Tok,
                  llvm::StringMap<StringRef> &VersionTags,
                  Preprocessor::StateStack &States)
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

  bool toBool(const StringRef &Val) {
    if (Val.data() == TRUE)
      return true;
    if (Val.data() == FALSE)
      return false;
    // TODO Emit ERROR
    return false;
  }

  bool isBool(const StringRef &Val) {
    return Val.data() == TRUE || Val.data() == FALSE;
  }

  void actOnAssignment(SMLoc Loc, const StringRef &Identifier, const StringRef &Value) {
    llvm::StringMap<StringRef>::iterator I = VersionTags.find(Identifier);
    if (I == VersionTags.end()) {
      // TODO Emit error.
      return ;
    }
    I->second = Value;
  }

  void actOnEnvironment(SMLoc Loc, const StringRef &Identifier, const StringRef &Value) {
    // TODO Add command line option
  }

  void actOnDefinition(SMLoc Loc, const StringRef &Identifier, const StringRef &Value) {
    if (!VersionTags.insert(std::pair<StringRef, StringRef>(Identifier, Value)).second) {
      // TODO Emit error.
    }
  }

  void actOnIf(SMLoc Loc, const StringRef &StrVal) {
    bool Val = toBool(StrVal);
    States.emplace_back(Val);
    // If Condition is false, then skip source until next directive.
    if (!Val)
      skipUntilNextDirective();
  }

  void actOnElsIf(SMLoc Loc, const StringRef &StrVal) {
    bool Val = toBool(StrVal);
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

  void actOnElse(SMLoc Loc) {
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

  void actOnEnd(SMLoc Loc) {
    if (States.empty())
      Lex.getDiagnostics().report(Loc, diag::err_unexpected_end_in_directive);
    else
      States.pop_back();
  }

  StringRef actOnRelation(tok::TokenKind Op, const StringRef &Left,
                          const StringRef &Right) {
    // Check for syntax error on relational operator.
    if (Op != tok::equal && Op != tok::hash)
      return FALSE;
    if (isBool(Left) && isBool(Right))
      return toBool(Left) == toBool(Right) ? TRUE : FALSE;
    if (!isBool(Left) && !isBool(Right))
      return Left.equals(Right) ? TRUE : FALSE;
    // TODO Emit ERROR
    return FALSE;
  }

  StringRef actOnOr(const StringRef &Left, const StringRef &Right) {
    return toBool(Left) || toBool(Right) ? FALSE : TRUE;
  }

  StringRef actOnAnd(const StringRef &Left, const StringRef &Right) {
    return toBool(Left) && toBool(Right) ? FALSE : TRUE;
  }

  StringRef actOnNot(StringRef &Val) { return toBool(Val) ? FALSE : TRUE; }

  StringRef actOnIdentifierValue(StringRef Identifier) {
    if (Identifier.equals(TRUE))
      return TRUE;
    if (Identifier.equals(FALSE))
      return FALSE;
    // Lookup identifier in version tag container.
    llvm::StringMap<StringRef>::const_iterator I = VersionTags.find(Identifier);
    if (I != VersionTags.end())
      return I->second;
    // Nothing found. Assume false.
    Lex.getDiagnostics().report(Tok.getLocation(),
                                diag::warn_version_tag_not_found)
        << Identifier;
    return FALSE;
  }

  void advance() {
    if (!IgnoreAdvance) {
      Lex.next(Tok);
      if (Tok.is(tok::identifier)) {
        tok::TokenKind Kind =
            llvm::StringSwitch<tok::TokenKind>(Tok.getIdentifier())
#define DIRECTIVE(NAME) .Case(#NAME, tok::kw_##NAME)
#include "m2lang/Basic/TokenKinds.def"
                .Default(tok::identifier);
        Tok.setKind(Kind);
      }
    }
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

const StringRef DirectiveParser::TRUE = llvm::StringLiteral("TRUE");
const StringRef DirectiveParser::FALSE = llvm::StringLiteral("FALSE");

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
