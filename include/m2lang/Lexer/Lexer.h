//===--- Lexer.h - Modula-2 Language Lexer ----------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the BSD 3-Clause License.
// See the LICENSE file for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the lexer implementation.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_LEXER_LEXER_H
#define M2LANG_LEXER_LEXER_H

#include "m2lang/Basic/LangOptions.h"
#include "m2lang/Lexer/Token.h"
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

  public:
    Lexer(const llvm::MemoryBuffer *InputFile, const LangOptions &LangOpts)
      : LangOpts(LangOpts)
    {
      BufferStart = InputFile->getBufferStart();
      BufferEnd = InputFile->getBufferEnd();
      BufferPtr = BufferStart;
    }

  /// getLangOpts - Return the language features currently enabled.
  /// NOTE: this lexer modifies features as a file is parsed!
  const LangOptions &getLangOpts() const { return LangOpts; }

  void next(Token &token);

  private:
    void identifier(Token &token);
    void number(Token &token);
    void string(Token &token);
    void comment(Token &token);
    void directive(Token &token);

    void FormTokenWithChars(Token &token, const char *tokEnd, tok::TokenKind kind);
  };
} // end namespace m2lang
#endif