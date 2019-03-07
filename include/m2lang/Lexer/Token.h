//===--- TokenKinds.h - Enum values for Modula-2 Token Kinds ----*- C++ -*-===//
//
// Part of the M2Lang Project, under the BSD 3-Clause License.
// See the LICENSE file for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the m2lang::TokenKind enum and support functions.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_LEXER_TOKEN_H
#define M2LANG_LEXER_TOKEN_H

#include "m2lang/Basic/TokenKinds.h"

namespace m2lang {

  class Token {
    size_t Loc;

    /// Kind - The actual flavor of token this is.
    tok::TokenKind Kind;

  public:
    tok::TokenKind getKind() const { return Kind; }
    void setKind(tok::TokenKind K) { Kind = K; }

    /// is/isNot - Predicates to check if this token is a specific kind, as in
    /// "if (Tok.is(tok::l_brace)) {...}".
    bool is(tok::TokenKind K) const { return Kind == K; }
    bool isNot(tok::TokenKind K) const { return Kind != K; }
    bool isOneOf(tok::TokenKind K1, tok::TokenKind K2) const {
      return is(K1) || is(K2);
    }
    template <typename... Ts>
    bool isOneOf(tok::TokenKind K1, tok::TokenKind K2, Ts... Ks) const {
      return is(K1) || isOneOf(K2, Ks...);
    }

    const char *getName() const { return tok::getTokenName(Kind); }
  };

} // end namespace m2lang
#endif