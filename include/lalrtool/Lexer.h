//===--- Lexer.h - LALRtool lexer -------------------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the grammar.
///
//===----------------------------------------------------------------------===//

#ifndef LALRTOOL_LEXER_H
#define LALRTOOL_LEXER_H

#include "lalrtool/Diagnostic.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"

namespace lalrtool {

class Lexer;

namespace tok {
enum TokenKind : unsigned short {
  unknown,
  eoi,
  identifier,
  qualidentifier,
  string,
  argument,
  code,
  equal,
  colon,
  comma,
  semi,
  pipe,
  l_paren,
  r_paren,
  r_parenquestion,
  r_parenstar,
  r_parenplus,
  percentpercent,
  kw_eoi,
  kw_define,
  kw_if,
  kw_language,
  kw_start,
  kw_token,
  NUM_TOKENS
};
} // end namespace tok

class Token {
  friend class Lexer;

  /// The location of the token.
  const char *Ptr;

  /// The length of the token.
  size_t Length;

  /// Kind - The actual flavor of token this is.
  tok::TokenKind Kind;

public:
  tok::TokenKind getKind() const { return Kind; }

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

  llvm::SMLoc getLoc() const { return llvm::SMLoc::getFromPointer(Ptr); }
  size_t getLength() const { return Length; }

  llvm::StringRef getData() { return llvm::StringRef(Ptr, Length); }
};

class Lexer {
  llvm::SourceMgr &SrcMgr;
  Diagnostic Diag;

  const char *CurPtr = nullptr;
  llvm::StringRef CurBuf;

  /// CurBuffer - This is the current buffer index we're lexing from as managed
  /// by the SourceMgr object.
  unsigned CurBuffer = 0;

public:
  Lexer(llvm::SourceMgr &SrcMgr) : SrcMgr(SrcMgr), Diag(Diagnostic(SrcMgr)) {
    CurBuffer = SrcMgr.getMainFileID();
    CurBuf = SrcMgr.getMemoryBuffer(CurBuffer)->getBuffer();
    CurPtr = CurBuf.begin();
  }

  Diagnostic &getDiagnostic() { return Diag; }

  void next(Token &Tok);

private:
  void identifier(Token &Tok);
  void code(Token &Tok, char Open, const char Close, tok::TokenKind Kind);
  void keyword(Token &Tok);
  void string(Token &Tok);
  void multilinecomment();
  void singlelinecomment();

  void printError(const char *Loc, const llvm::Twine &Msg);
  void formToken(Token &Result, const char *TokEnd, tok::TokenKind Kind);
};
} // namespace lalrtool
#endif