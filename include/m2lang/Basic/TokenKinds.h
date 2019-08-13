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

#ifndef M2LANG_BASIC_TOKENKINDS_H
#define M2LANG_BASIC_TOKENKINDS_H

#include "llvm/Support/Compiler.h"

namespace m2lang {

  namespace tok {
    enum TokenKind : unsigned short {
#define TOK(X) X,
#include "TokenKinds.def"
      NUM_TOKENS
    };

    const char *getTokenName(TokenKind Kind) LLVM_READNONE;

    const char *getPunctuatorSpelling(TokenKind Kind) LLVM_READNONE;

    const char *getKeywordSpelling(TokenKind Kind) LLVM_READNONE;
  }  // end namespace tok
}  // end namespace m2lang

#endif