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

#ifndef M2LANG_LEXER_H
#define M2LANG_LEXER_H

#include "llvm/Support/MemoryBuffer.h"
#include "m2lang/Token.h"

namespace m2lang {
  class Lexer {
    // Start of the buffer.
    const char *BufferStart;

    // End of the buffer.
    const char *BufferEnd;

    // BufferPtr - Current pointer into the buffer.  This is the next character
    // to be lexed.
    const char *BufferPtr;

  public:
    Lexer(const llvm::MemoryBuffer *InputFile) {
      BufferStart = InputFile->getBufferStart();
      BufferEnd = InputFile->getBufferEnd();
      BufferPtr = BufferStart;
    }

    void next(Token &token);

  private:
    void identifier(Token &token);
    void number(Token &token);
    void string(Token &token);
    void comment(Token &token);

    void FormTokenWithChars(Token &token, const char *tokEnd, tok::TokenKind kind);
  };
} // end namespace m2lang
#endif