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