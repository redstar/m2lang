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
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"

namespace m2lang {
  class Lexer {
    // Start of the buffer.
    const char *BufferStart;

    // End of the buffer.
    const char *BufferEnd;

    // BufferPtr - Current pointer into the buffer.  This is the next character
    // to be lexed.
    const char *BufferPtr;

    // LangOpts enabled by this language (cache).
    LangOptions LangOpts;

    DiagnosticsEngine *Diags;

  public:
    Lexer(DiagnosticsEngine &diags, const llvm::MemoryBuffer *InputFile, const LangOptions &LangOpts)
      : Diags(&diags), LangOpts(LangOpts)
    {
      BufferStart = InputFile->getBufferStart();
      BufferEnd = InputFile->getBufferEnd();
      BufferPtr = BufferStart;
    }

    DiagnosticsEngine &getDiagnostics() const { return *Diags; }
    void setDiagnostics(DiagnosticsEngine &D) { Diags = &D; }

    /// getLangOpts - Return the language features currently enabled.
    /// NOTE: this lexer modifies features as a file is parsed!
    const LangOptions &getLangOpts() const { return LangOpts; }

    void next(Token &token);

    /// Gets source code buffer.
    llvm::StringRef getBuffer() const {
      return llvm::StringRef(BufferStart, BufferEnd - BufferStart);
    }

  private:
    void identifier(Token &token);
    void number(Token &token);
    void string(Token &token);
    void comment(Token &token);
    void directive(Token &token);

    void FormTokenWithChars(Token &Result, const char *TokEnd, tok::TokenKind Kind);
  };
} // end namespace m2lang
#endif