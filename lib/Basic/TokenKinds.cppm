//===--- TokenKinds.cppm - Token Kinds Support ----------------------------===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the TokenKind enum and support functions.
///
//===----------------------------------------------------------------------===//

module;

#include "llvm/Support/Compiler.h"
#include "llvm/Support/ErrorHandling.h"

export module m2lang.basic:TokenKinds;

export namespace m2lang {

namespace tok {
enum TokenKind : unsigned short {
#define TOK(X) X,
#include "m2lang/Basic/TokenKinds.def"
  NUM_TOKENS
};

const char *getTokenName(TokenKind Kind) LLVM_READNONE;

const char *getPunctuatorSpelling(TokenKind Kind) LLVM_READNONE;

const char *getKeywordSpelling(TokenKind Kind) LLVM_READNONE;
} // end namespace tok
} // end namespace m2lang

using namespace m2lang;

static const char *const TokNames[] = {
#define TOK(X) #X,
#define KEYWORD(X, Y) #X,
#include "m2lang/Basic/TokenKinds.def"
    nullptr};

const char *tok::getTokenName(TokenKind Kind) {
  if (Kind < tok::NUM_TOKENS)
    return TokNames[Kind];
  llvm_unreachable("unknown TokenKind");
  return nullptr;
}

const char *tok::getPunctuatorSpelling(TokenKind Kind) {
  switch (Kind) {
#define PUNCTUATOR(X, Y)                                                       \
  case X:                                                                      \
    return Y;
#include "m2lang/Basic/TokenKinds.def"
  default:
    break;
  }
  return nullptr;
}

const char *tok::getKeywordSpelling(TokenKind Kind) {
  switch (Kind) {
#define KEYWORD(X, Y)                                                          \
  case kw_##X:                                                                 \
    return #X;
#include "m2lang/Basic/TokenKinds.def"
  default:
    break;
  }
  return nullptr;
}