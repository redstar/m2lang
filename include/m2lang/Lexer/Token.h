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

  class Token {
    /// The location of the token.
    size_t Loc;

    /// The length of the token.
    size_t Length;

    /// Kind - The actual flavor of token this is.
    tok::TokenKind Kind;

    /// Pointer to token-specific data.
    void *PtrData;
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

    size_t getLocation() const { // TODO
      return Loc;
    }
    size_t getLength() const {
      return Length;
    }

    void setLocation(size_t L) { Loc = L; } // TODO
    void setLength(size_t Len) {
      Length = Len;
    }

    llvm::StringRef getIdentifier() {
      assert(is(tok::identifier) && "Cannot get identfier of non-identifier");
      return llvm::StringRef(static_cast<const char *>(PtrData), Length);
    }

    void setIdentifier(const char *Ident) {
      assert(is(tok::identifier) && "Cannot get identfier of non-identifier");
      PtrData = const_cast<char*>(Ident);
    }

    llvm::StringRef getLiteralData() {
      assert(isOneOf(tok::integer_literal, tok::real_literal, tok::char_literal,
                     tok::string_literal) &&
             "Cannot get literal data of non-literal");
      return llvm::StringRef(static_cast<const char *>(PtrData), Length);
    }

    void setLiteralData(const char *Literal) {
      assert(isOneOf(tok::integer_literal, tok::real_literal, tok::char_literal,
                     tok::string_literal) &&
             "Cannot get literal data of non-literal");
      PtrData = const_cast<char*>(Literal);
    }
  };

} // end namespace m2lang
#endif