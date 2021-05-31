//===--- Lexer.h - Modula-2 Language Lexer ----------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the lexer implementation.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_LEXER_LEXER_H
#define M2LANG_LEXER_LEXER_H

#include "m2lang/Basic/Diagnostic.h"
#include "m2lang/Basic/LangOptions.h"
#include "m2lang/Lexer/Token.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/MemoryBuffer.h"

namespace m2lang {

class KeywordFilter {
  using HashTableTy = llvm::StringMap<tok::TokenKind, llvm::BumpPtrAllocator>;
  HashTableTy HashTable;

  void addKeyword(llvm::StringRef Keyword, tok::TokenKind TokenCode,
                  unsigned Flags, const LangOptions &LangOpts);

public:
  void addKeywords(const LangOptions &LangOpts);

  tok::TokenKind getKeyword(llvm::StringRef Name,
                            tok::TokenKind DefaultTokenCode = tok::unknown) {
    auto result = HashTable.find(Name);
    if (result != HashTable.end())
      return result->second;
    return DefaultTokenCode;
  }
};

class Lexer {
  SourceMgr &SrcMgr;
  DiagnosticsEngine *Diags;

  const char *CurPtr;
  StringRef CurBuf;

  /// CurBuffer - This is the current buffer index we're
  /// lexing from as managed by the SourceMgr object.
  unsigned CurBuffer = 0;

  // LangOpts enabled by this language (cache).
  LangOptions LangOpts;

  KeywordFilter Keywords;

public:
  Lexer(SourceMgr &SrcMgr, DiagnosticsEngine &diags,
        const LangOptions &LangOpts)
      : SrcMgr(SrcMgr), Diags(&diags), LangOpts(LangOpts) {
    CurBuffer = SrcMgr.getMainFileID();
    CurBuf = SrcMgr.getMemoryBuffer(CurBuffer)->getBuffer();
    CurPtr = CurBuf.begin();
    Keywords.addKeywords(LangOpts);
  }

  DiagnosticsEngine &getDiagnostics() const { return *Diags; }
  void setDiagnostics(DiagnosticsEngine &D) { Diags = &D; }

  /// getLangOpts - Return the language features currently enabled.
  const LangOptions &getLangOpts() const { return LangOpts; }

  /// Returns the next token from the input.
  void next(Token &token);

  /// Gets source code buffer.
  llvm::StringRef getBuffer() const { return CurBuf; }

private:
  void identifier(Token &token);
  void number(Token &token);
  void string(Token &token);
  void comment(Token &token);

  SMLoc getLoc() { return SMLoc::getFromPointer(CurPtr); }
  void formTokenWithChars(Token &Result, const char *TokEnd,
                          tok::TokenKind Kind);
};
} // end namespace m2lang
#endif