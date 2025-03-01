//===--- Preprocessor.cppm - Modula-2 Language Preprocessor ---------------===//
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

module;

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/SMLoc.h"

export module m2lang.lexer:Preprocessor;

import :Lexer;
import :Token;
import m2lang.basic;

namespace m2lang {

// TODO These are used in the grammar file.
using llvm::StringRef;
using llvm::SMLoc;

export class Preprocessor {
  m2lang::Lexer &Lex;

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
  using StateStack = llvm::SmallVector<State, 8>;

private:
  StateStack States;
  llvm::StringMap<llvm::StringRef> VersionTags = {
      {"m2lang", "TRUE"},     // This is the m2lang compiler.
      {"p1", "FALSE"},        // This is not the p1 compiler.
      {"StonyBrook", "FALSE"} // This is not the StonyBrook compiler.
  };

public:
  Preprocessor(m2lang::Lexer &Lex, TargetInfo &TI);

  /// Returns the next token from the input.
  void next(m2lang::Token &Tok);

  DiagnosticsEngine &getDiagnostics() { return Lex.getDiagnostics(); }

  const LangOptions &getLangOpts() const { return Lex.getLangOpts(); }

private:
  /// Handles compiler directives.
  void directive(m2lang::Token &Tok);
};

} // end namespace m2lang

using namespace m2lang;

using VersionTag = std::pair<llvm::StringRef, llvm::StringRef>;
using VersionTagMap = llvm::StringMap<llvm::StringRef>;

namespace {
// The singleton values for TRUE and FALSE.
static const char *const TRUE = "TRUE";
static const char *const FALSE = "FALSE";
} // namespace

namespace cl {
/// VersionTagParser: a custom command line parser
///
/// Parses definition of version tags with the following syntax:
/// -D+foo sets foo to boolean value TRUE
/// -D-foo sets foo to boolean value FALSE
/// -Dfoo=bar or -Dfoo="bar" or -Dfoo='bar' sets foo to string value bar
struct VersionTagParser : public llvm::cl::parser<VersionTag> {
  explicit VersionTagParser(llvm::cl::Option &O)
      : llvm::cl::parser<VersionTag>(O) {}

  // parse - Return true on error.
  bool parse(llvm::cl::Option &O, llvm::StringRef ArgName,
             llvm::StringRef ArgValue, VersionTag &Val);
  const char *ident(const char *Ptr);
  const char *string(const char *Ptr);
};

const char *VersionTagParser::ident(const char *Ptr) {
  if ((*Ptr >= 'a' && *Ptr <= 'z') || (*Ptr >= 'A' && *Ptr <= 'Z') ||
      *Ptr == '_') {
    while ((*Ptr >= 'a' && *Ptr <= 'z') || (*Ptr >= 'A' && *Ptr <= 'Z') ||
           (*Ptr >= '0' && *Ptr <= '9') || *Ptr == '_')
      ++Ptr;
    return Ptr;
  }
  return nullptr;
}

const char *VersionTagParser::string(const char *Ptr) {
  if (*Ptr != '"' && *Ptr != '\'')
    return ident(Ptr);
  const char Delim = *Ptr++;
  while (*Ptr && *Ptr != Delim)
    ++Ptr;
  return *Ptr ? Ptr + 1 : nullptr;
}

bool VersionTagParser::parse(llvm::cl::Option &O, llvm::StringRef ArgName,
                             llvm::StringRef ArgValue, VersionTag &Val) {
  const char *ArgStart = ArgValue.data();
  while (true) {
    if (*ArgStart == '+' || *ArgStart == '-') {
      llvm::StringRef Bool((*ArgStart++ == '+') ? TRUE : FALSE);
      if (const char *IdentEnd = ident(ArgStart)) {
        llvm::StringRef Ident(ArgStart, IdentEnd - ArgStart);
        Val = std::pair<llvm::StringRef, llvm::StringRef>(Ident, Bool);
        ArgStart = IdentEnd;
      } else
        return O.error(llvm::Twine("Expect identifier after ") +
                       llvm::Twine(ArgStart[-1]));
    } else {
      if (const char *IdentEnd = ident(ArgStart)) {
        llvm::StringRef Ident(ArgStart, IdentEnd - ArgStart);
        ArgStart = IdentEnd;
        if (*ArgStart != '=')
          return O.error("Expected = after " + Ident);
        if (const char *StringEnd = string(++ArgStart)) {
          llvm::StringRef String(ArgStart, StringEnd - ArgStart);
          if (String.starts_with("'") || String.starts_with("\""))
            String = String.substr(1, String.size() - 2);
          Val = std::pair<llvm::StringRef, llvm::StringRef>(Ident, String);
          ArgStart = StringEnd;
        } else
          return O.error("Expected string after " + Ident + "=");
      } else
        return O.error("Expect identifier");
    }
    if (!*ArgStart)
      return false;
    if (*ArgStart == ',')
      ++ArgStart;
    else
      break;
  }
  return O.error("Could not parse argument " + ArgValue);
}
} // namespace cl

// Specialication of the llvm::cl::list_storage<> template, for VersionTag
// values stored in a VersionTagMap. This implementation assumes the user will
// specify a variable to store the data into with the cl::location(x) modifier.
template <> class llvm::cl::list_storage<VersionTag, VersionTagMap> {
  VersionTagMap *Location = nullptr; // Where to store the object...
  std::vector<OptionValue<VersionTag>> Default =
      std::vector<OptionValue<VersionTag>>();
  bool DefaultAssigned = false;

public:
  list_storage() = default;

  void clear() {}

  bool setLocation(Option &O, VersionTagMap &L) {
    if (Location)
      return O.error("cl::location(x) specified more than once!");
    Location = &L;
    return false;
  }

  template <class T> void addValue(const T &V, bool initial = false) {
    assert(Location != 0 && "cl::location(...) not specified for a command "
                            "line option with external storage!");
    Location->insert(V);
    if (initial)
      Default.push_back(V);
  }

  const std::vector<OptionValue<VersionTag>> &getDefault() const {
    return Default;
  }

  void assignDefault() { DefaultAssigned = true; }
  void overwriteDefault() { DefaultAssigned = false; }
  bool isDefaultAssigned() { return DefaultAssigned; }
};

// TODO Make global
static llvm::cl::OptionCategory M2langCat("m2lang Options");

// Storage varibale for the command line option.
static VersionTagMap DefineVersionTags;

static llvm::cl::list<VersionTag, VersionTagMap, cl::VersionTagParser>
    DefineVersionTagsOpt(
        "D", llvm::cl::location(DefineVersionTags), llvm::cl::cat(M2langCat),
        llvm::cl::Prefix, llvm::cl::ZeroOrMore,
        llvm::cl::desc("Define version tag: -D+foo -D-foo -Dfoo=bar"));

namespace {
class DirectiveParser {
  Lexer &Lex;
  Token &Tok;
  VersionTagMap &VersionTags;
  Preprocessor::StateStack &States;

  // SkipMode is true, if conditional compiing leads to skipping of tokens.
  // In SkipMode, comments and normal tokens are skipped. Directives are
  // syntactically checked, but not interpreted. SkipMode is left if an
  // matching ELSIF, ELSE or END directive is found. Conditional compilation
  // directives can be nested.
  bool SkipMode;

public:
  DirectiveParser(Lexer &Lex, Token &Tok, VersionTagMap &VersionTags,
                  Preprocessor::StateStack &States)
      : Lex(Lex), Tok(Tok), VersionTags(VersionTags), States(States),
        SkipMode(false) {
    assert(Tok.is(tok::lessstar) && "Current token must be '<*'");
  }

  void parse() {
    const _TokenBitSet Eof{tok::eof};
    SkipMode = false;
    while (Tok.is(tok::lessstar)) {
      parseDirective(Eof);
      if (SkipMode)
        skipUntilNextDirective();
    }
  }

private:
  DiagnosticsEngine &getDiagnostics() const { return Lex.getDiagnostics(); }

  /// Called if source code is to be skipped.
  void skipUntilNextDirective() {
    while (!Tok.isOneOf(tok::eof, tok::lessstar))
      Lex.next(Tok);
  }

  bool toBool(const llvm::StringRef &Val) {
    if (Val.data() == TRUE)
      return true;
    if (Val.data() == FALSE)
      return false;
    // TODO Emit ERROR
    return false;
  }

  bool isBool(const llvm::StringRef &Val) {
    return Val.data() == TRUE || Val.data() == FALSE;
  }

  void actOnAssignment(llvm::SMLoc Loc, const llvm::StringRef &Identifier,
                       const llvm::StringRef &Value) {
    if (SkipMode)
      return;
    llvm::StringMap<llvm::StringRef>::iterator I = VersionTags.find(Identifier);
    if (I == VersionTags.end()) {
      // TODO Emit error.
      return;
    }
    I->second = Value;
  }

  void actOnEnvironment(llvm::SMLoc Loc, const llvm::StringRef &Identifier,
                        const llvm::StringRef &Value) {
    if (SkipMode)
      return;
    VersionTagMap::const_iterator I = DefineVersionTags.find(Identifier);
    if (I != DefineVersionTags.end()) {
      VersionTags[Identifier] = I->second;
    } else {
      VersionTags[Identifier] = Value;
    }
  }

  void actOnDefinition(llvm::SMLoc Loc, const llvm::StringRef &Identifier,
                       const llvm::StringRef &Value) {
    if (SkipMode)
      return;
    if (!VersionTags
             .insert(
                 std::pair<llvm::StringRef, llvm::StringRef>(Identifier, Value))
             .second) {
      // TODO Emit error.
    }
  }

  void actOnIf(llvm::SMLoc Loc, const llvm::StringRef &StrVal) {
    if (SkipMode) {
      // Stack the IF directive to enable syntax check
      States.emplace_back(false, true, true);
      return;
    }
    bool Val = toBool(StrVal);
    // If Condition is false, then skip source until next directive.
    bool NewSkipMode = !Val || SkipMode;
    States.emplace_back(Val, SkipMode, NewSkipMode);
    SkipMode = NewSkipMode;
  }

  // Need to turn off SkipMode if this ELSIF needs more than syntax check.
  void actOnElsIf(llvm::SMLoc Loc) {
    if (SkipMode) {
      if (States.empty()) {
        getDiagnostics().report(Loc, diag::err_unexpected_elseif_in_directive);
        return;
      }
      Preprocessor::State &St = States.back();
      if (!St.SyntaxOnly)
        SkipMode = false;
    }
  }

  void actOnElsIf(llvm::SMLoc Loc, const llvm::StringRef &StrVal) {
    if (States.empty()) {
      getDiagnostics().report(Loc, diag::err_unexpected_elseif_in_directive);
      return;
    }
    Preprocessor::State &St = States.back();
    if (St.NextState != 0) {
      getDiagnostics().report(Loc, diag::err_unexpected_elseif_in_directive);
      return;
    }
    if (SkipMode) {
      assert(St.SyntaxOnly && "SyntaxOnly not set with SkipMode");
      return;
    }
    bool Val = toBool(StrVal);
    if (Val && !St.Satisfied) {
      // Condition is true and was not previously true, so include source
      St.Satisfied = true;
    } else {
      // Condition is false, skip source until next directive.
      SkipMode = true;
    }
    St.Skipping = SkipMode;
  }

  void actOnElse(llvm::SMLoc Loc) {
    if (States.empty()) {
      getDiagnostics().report(Loc, diag::err_unexpected_else_in_directive);
      return;
    }
    Preprocessor::State &St = States.back();
    if (St.NextState != 0) {
      getDiagnostics().report(Loc, diag::err_unexpected_elseif_in_directive);
      return;
    }
    St.NextState = 1;
    // Condition was true, skip source until next directive - if not already
    // skipping!
    SkipMode = St.SyntaxOnly || St.Satisfied;
    St.Skipping = SkipMode;
  }

  void actOnEnd(llvm::SMLoc Loc) {
    if (States.empty()) {
      getDiagnostics().report(Loc, diag::err_unexpected_end_in_directive);
      return;
    }
    States.pop_back();
    if (SkipMode) {
      if (States.empty()) {
        // TODO Error mes
        SkipMode = false;
        return;
      }
      Preprocessor::State &St = States.back();
      SkipMode = St.Skipping;
    }
  }

  llvm::StringRef actOnRelation(tok::TokenKind Op, const llvm::StringRef &Left,
                                const llvm::StringRef &Right) {
    if (SkipMode)
      return FALSE;
    // Check for syntax error on relational operator.
    if (Op != tok::equal && Op != tok::hash)
      return FALSE;
    if (isBool(Left) && isBool(Right))
      return toBool(Left) == toBool(Right) ? TRUE : FALSE;
    if (!isBool(Left) && !isBool(Right))
      return Left == Right ? TRUE : FALSE;
    // TODO Emit ERROR
    return FALSE;
  }

  llvm::StringRef actOnOr(const llvm::StringRef &Left,
                          const llvm::StringRef &Right) {
    if (SkipMode)
      return FALSE;
    return toBool(Left) || toBool(Right) ? FALSE : TRUE;
  }

  llvm::StringRef actOnAnd(const llvm::StringRef &Left,
                           const llvm::StringRef &Right) {
    if (SkipMode)
      return FALSE;
    return toBool(Left) && toBool(Right) ? FALSE : TRUE;
  }

  llvm::StringRef actOnNot(llvm::StringRef &Val) {
    if (SkipMode)
      return FALSE;
    return toBool(Val) ? FALSE : TRUE;
  }

  llvm::StringRef actOnIdentifierValue(llvm::StringRef Identifier) {
    if (SkipMode)
      return FALSE;
    if (Identifier == TRUE)
      return TRUE;
    if (Identifier == FALSE)
      return FALSE;
    // Lookup identifier in version tag container.
    llvm::StringMap<llvm::StringRef>::const_iterator I =
        VersionTags.find(Identifier);
    if (I != VersionTags.end())
      return I->second;
    // Nothing found. Assume false.
    getDiagnostics().report(Tok.getLocation(), diag::warn_version_tag_not_found)
        << Identifier;
    return FALSE;
  }

  void advance() {
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
#include "DirectiveParser.g.inc"
#undef DIRECTIVEPARSER_DECLARATION
};

#define DIRECTIVEPARSER_DEFINITION
#include "DirectiveParser.g.inc"
#undef DIRECTIVEPARSER_DEFINITION
} // namespace

Preprocessor::Preprocessor(Lexer &Lex, TargetInfo &TI) : Lex(Lex) {
  // Add target-dependent values.
  VersionTags.insert(std::pair("Endian", TI.isBigEndian() ? "Big" : "Little"));
  for (auto &Val : TI.getTargetDefines())
    VersionTags.insert(Val);
}

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
  DParser.parse();
}
