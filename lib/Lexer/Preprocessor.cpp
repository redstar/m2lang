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

using namespace m2lang;

void Preprocessor::next(Token &Tok) {
  while (true) {
    Lex.next(Tok);
    // Begin of directive.
    if (Tok.is(tok::lessstar) && directive(Tok))
      continue;
    // Skip comments.
    if (Tok.is(tok::comment))
      continue;
    break;
  }
}

/*
Syntax:
directive
  : "IF" expr "THEN"
  | "ELSEIF" expr "THEN"
  | "ELSE"
  | "END"
  | "DEFINE" "(" string "," expr ")"
  | "ASSIGN" "(" string "," expr ")"
  | "ASSERT"
  | "UNUSED"
  | "OBSOLETE"
  ;
expr
  : term ("OR" term)*
  ;
term
  : factor ("AND" factor)*
  ;
factor
  : ident
  | "(" expr ")"
  | "NOT" factor
  ;
*/
bool Preprocessor::directive(Token &Tok) {
  // Just skip over the text until end of directive is reached.
  // TODO Implement.
  do {
    Lex.next(Tok);
  } while (!Tok.isOneOf(tok::eof, tok::stargreater));
  return true;
}
