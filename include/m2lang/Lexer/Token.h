//===--- TokenKinds.h - Enum values for Modula-2 Token Kinds ----*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
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
#include "llvm/ADT/StringRef.h"

namespace m2lang {

class Lexer;

  class Token {
    friend class Lexer;

    /// The begin of the token.
    const char *Ptr;

    /// The length of the token.
    size_t Length;

    /// Kind - The actual flavor of token this is.
    tok::TokenKind Kind;

  public:
    tok::TokenKind getKind() const { return Kind; }
    void setKind(tok::TokenKind K) { Kind =  K; }

    /// is/isNot - Predicates to check if this token is a specific kind, as in
    /// "if (Tok.is(tok::l_brace)) {...}".
    bool is(tok::TokenKind K) const { return Kind == K; }
    bool isNot(tok::TokenKind K) const { return Kind != K; }
    template <typename... TokenKind> bool isOneOf(TokenKind &&...Tks) const {
      return (... || is(Tks));
    }
    const char *getName() const { return tok::getTokenName(Kind); }

    SMLoc getLocation() const {
      return SMLoc::getFromPointer(Ptr);
    }

    size_t getLength() const {
      return Length;
    }

    llvm::StringRef getIdentifier() {
      assert(is(tok::identifier) && "Cannot get identfier of non-identifier");
      return llvm::StringRef(Ptr, Length);
    }

    llvm::StringRef getLiteralData() {
      assert(isOneOf(tok::integer_literal, tok::real_literal, tok::char_literal,
                     tok::string_literal) &&
             "Cannot get literal data of non-literal");
      return llvm::StringRef(Ptr, Length);
    }
  };

} // end namespace m2lang
#endif