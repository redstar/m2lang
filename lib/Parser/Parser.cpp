//===--- Parser.h - Modula-2 Language Parser --------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the BSD 3-Clause License.
// See the LICENSE file for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the parser implementation.
///
//===----------------------------------------------------------------------===//

#include "m2lang/Lexer/Lexer.h"
#include "llvm/ADT/StringRef.h"

using namespace m2lang;
using namespace llvm;

class Parser {
  Token Tok;

  /// NextToken - This peeks ahead one token and returns it without
  /// consuming it.
  const Token &NextToken() {
    return Token();
  }

  void ConsumeToken() {
  }

  void ConsumeAnyToken() {
  }

  void ConsumeSemi() {
  }

  /// Expects and consume the token.
  /// Returns true in case of syntax error
  bool ExpectAndConsume(tok::TokenKind ExpectedTok, StringRef Msg) {
    return false;
  }

public:
  void Initialize() {
  }

  /// Parse a compilation unit.
  void ParseCompilationUnit() {
  }

  /// Parse import lists.
  void ParseImport() {
  }

  /// Parse definitions.
  void ParseDefinition() {
  }

  void ParseModule() {
  }

};