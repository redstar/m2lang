//===--- Lexer.cpp - ASTtool lexer ------------------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the ASTtool lexer implementation.
///
//===----------------------------------------------------------------------===//

#include "asttool/Lexer.h"

using namespace asttool;

namespace charinfo {
LLVM_READNONE inline bool isDigit(char c) { return c >= '0' && c <= '9'; }

LLVM_READNONE inline bool isLetter(char c) {
  return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}
} // namespace charinfo

void Lexer::next(Token &token) {
repeat:
  while (*CurPtr == '\r' || *CurPtr == '\n' || *CurPtr == ' ' ||
         *CurPtr == '\t' || *CurPtr == '\f' || *CurPtr == '\v') {
    ++CurPtr;
  }
  if (!*CurPtr) {
    token.Kind = tok::eoi;
    return;
  }
  if (charinfo::isLetter(*CurPtr)) {
    identifier(token);
    return;
  }
  switch (*CurPtr) {
#define CASE(ch, tok)                                                          \
  case ch:                                                                     \
    formToken(token, CurPtr + 1, tok);                                         \
    return
    CASE('=', tok::equal);
    CASE(':', tok::colon);
    CASE(',', tok::comma);
    CASE(';', tok::semi);
#undef CASE
  case '<':
    if (*(CurPtr + 1) == '=') {
      formToken(token, CurPtr + 2, tok::lesscolon);
      return;
    }
    break;
  case '/':
    if (char ch = *(CurPtr + 1)) {
      if (ch == '*') {
        multilinecomment();
        goto repeat;
      }
      if (ch == '/') {
        singlelinecomment();
        goto repeat;
      }
    }
    break;
  case '"':
  case '\'':
    string(token);
    return;
  case '{':
    code(token, '{', '}', tok::code);
    return;
  case '%':
    if (*(CurPtr + 1) == '%')
      formToken(token, CurPtr + 2, tok::percentpercent);
    else
      keyword(token);
    return;
  default:
    break;
  }
  formToken(token, CurPtr + 1, tok::unknown);
}

void Lexer::identifier(Token &token) {
  const char *start = CurPtr;
  const char *end = CurPtr + 1;
  while (charinfo::isLetter(*end) || charinfo::isDigit(*end) || *end == '_') {
    ++end;
  }
  formToken(token, end, tok::identifier);
  token.Ptr = start;
}

void Lexer::keyword(Token &token) {
  const char *start = CurPtr;
  const char *end = CurPtr + 1;
  while (charinfo::isLetter(*end) || charinfo::isDigit(*end))
    ++end;
  // Exclude '%' from compare.
  llvm::StringRef Keyword = llvm::StringRef(start + 1, end - start - 1);
  tok::TokenKind Kind = llvm::StringSwitch<tok::TokenKind>(Keyword)
                            .Case("base", tok::kw_base)
                            .Case("in", tok::kw_in)
                            .Case("language", tok::kw_language)
                            .Case("list", tok::kw_list)
                            .Case("node", tok::kw_node)
                            .Case("out", tok::kw_out)
                            .Case("plain", tok::kw_plain)
                            .Case("typedef", tok::kw_typedef)
                            .Default(tok::unknown);
  if (Kind == tok::unknown)
    Diag.error(start, "unrecognized keyword");
  formToken(token, end, Kind);
}

void Lexer::code(Token &token, char open, const char close,
                 tok::TokenKind kind) {
  const char *start = CurPtr;
  const char *end = CurPtr + 1;
  const bool dot = *end == '.';
  if (dot) {
    do {
      ++end;
      while (*end && *end != close)
        ++end;
    } while (dot && *end && start + 1 < end && end[-1] != '.');
  } else {
    unsigned level = 1;
    while (*end && (*end != close || --level)) {
      if (*end == open)
        ++level;
      ++end;
    }
  }
  if (!*end)
    Diag.error(start, "unterminated code");
  formToken(token, end + 1, kind);
}

void Lexer::string(Token &token) {
  const char *start = CurPtr;
  const char *end = CurPtr + 1;
  while (*end && *end != *start && *CurPtr != '\n' && *CurPtr != '\r')
    ++end;
  if (*CurPtr == '\n' || *CurPtr == '\r') {
    Diag.error(start, "unterminated string");
  }
  formToken(token, end + 1, tok::string);
  token.Ptr = start;
}

void Lexer::multilinecomment() {
  const char *start = CurPtr;
  CurPtr += 2;
  do {
    while (*CurPtr && *CurPtr != '*')
      ++CurPtr;
    ++CurPtr;
  } while (*CurPtr && *CurPtr != '/');
  if (!*CurPtr)
    Diag.error(start, "unterminated comment");
  ++CurPtr;
}

void Lexer::singlelinecomment() {
  // Line endings: Unix \n, Mac \r, Dos/Windows \r\n
  while (*CurPtr && *CurPtr != '\n' && *CurPtr != '\r')
    ++CurPtr;
  if (*(CurPtr + 1) && *CurPtr == '\r' && *(CurPtr + 1) == '\n')
    ++CurPtr;
  ++CurPtr;
}

void Lexer::formToken(Token &Tok, const char *TokEnd, tok::TokenKind Kind) {
  size_t TokLen = TokEnd - CurPtr;
  Tok.Ptr = CurPtr;
  Tok.Length = TokLen;
  Tok.Kind = Kind;
  CurPtr = TokEnd;
}