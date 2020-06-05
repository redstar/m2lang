//===--- Lexer.cpp - LLtool  lexer ------------------------------*- C++ -*-===//
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

#include "Lexer.h"

using namespace lltool;

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
    CASE('|', tok::pipe);
    CASE('(', tok::l_paren);
#undef CASE
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
  case '<':
    code(token, '<', '>', tok::argument);
    return;
  case '{':
    code(token, '{', '}', tok::code);
    return;
  case ')':
    if (char ch = *(CurPtr + 1)) {
      switch (ch) {
      case '?':
        formToken(token, CurPtr + 2, tok::r_parenquestion);
        return;
      case '*':
        formToken(token, CurPtr + 2, tok::r_parenstar);
        return;
      case '+':
        formToken(token, CurPtr + 2, tok::r_parenplus);
        return;
      default:
        formToken(token, CurPtr + 1, tok::r_paren);
        return;
      }
    } else
      formToken(token, CurPtr + 1, tok::r_paren);
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
  bool qualified = false;
  const char *start = CurPtr;
  const char *end = CurPtr + 1;
  while (charinfo::isLetter(*end) || charinfo::isDigit(*end) || *end == '_' ||
         *end == '.') {
    if (*end == '.')
      qualified = true;
    ++end;
  }
  formToken(token, end, qualified ? tok::qualidentifier : tok::identifier);
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
                            .Case("eoi", tok::kw_eoi)
                            .Case("define", tok::kw_define)
                            .Case("if", tok::kw_if)
                            .Case("language", tok::kw_language)
                            .Case("start", tok::kw_start)
                            .Case("token", tok::kw_token)
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
    while (*end && *end != close || --level) {
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