//===--- Lexer.cpp - LALRtool lexer -----------------------------*- C++ -*-===//
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

#include "lalrtool/Lexer.h"
#include "llvm/ADT/StringSwitch.h"

using namespace lalrtool;

namespace charinfo {
LLVM_READNONE inline bool isDigit(char C) { return C >= '0' && C <= '9'; }

LLVM_READNONE inline bool isLetter(char C) {
  return (C >= 'A' && C <= 'Z') || (C >= 'a' && C <= 'z');
}
} // namespace charinfo

void Lexer::next(Token &Tok) {
repeat:
  while (*CurPtr == '\r' || *CurPtr == '\n' || *CurPtr == ' ' ||
         *CurPtr == '\t' || *CurPtr == '\f' || *CurPtr == '\v') {
    ++CurPtr;
  }
  if (!*CurPtr) {
    Tok.Kind = tok::eoi;
    return;
  }
  if (charinfo::isLetter(*CurPtr)) {
    identifier(Tok);
    return;
  }
  switch (*CurPtr) {
#define CASE(Ch, tok)                                                          \
  case Ch:                                                                     \
    formToken(Tok, CurPtr + 1, tok);                                           \
    return
    CASE('=', tok::equal);
    CASE(':', tok::colon);
    CASE(',', tok::comma);
    CASE(';', tok::semi);
    CASE('|', tok::pipe);
    CASE('(', tok::l_paren);
#undef CASE
  case '/':
    if (char Ch = *(CurPtr + 1)) {
      if (Ch == '*') {
        multilinecomment();
        goto repeat;
      }
      if (Ch == '/') {
        singlelinecomment();
        goto repeat;
      }
    }
    break;
  case '"':
  case '\'':
    string(Tok);
    return;
  case '<':
    code(Tok, '<', '>', tok::argument);
    return;
  case '{':
    code(Tok, '{', '}', tok::code);
    return;
  case ')':
    if (char Ch = *(CurPtr + 1)) {
      switch (Ch) {
      case '?':
        formToken(Tok, CurPtr + 2, tok::r_parenquestion);
        return;
      case '*':
        formToken(Tok, CurPtr + 2, tok::r_parenstar);
        return;
      case '+':
        formToken(Tok, CurPtr + 2, tok::r_parenplus);
        return;
      default:
        formToken(Tok, CurPtr + 1, tok::r_paren);
        return;
      }
    } else
      formToken(Tok, CurPtr + 1, tok::r_paren);
    return;
  case '%':
    if (*(CurPtr + 1) == '%')
      formToken(Tok, CurPtr + 2, tok::percentpercent);
    else
      keyword(Tok);
    return;
  default:
    break;
  }
  formToken(Tok, CurPtr + 1, tok::unknown);
}

void Lexer::identifier(Token &Tok) {
  bool Qualified = false;
  const char *Start = CurPtr;
  const char *End = CurPtr + 1;
  while (charinfo::isLetter(*End) || charinfo::isDigit(*End) || *End == '_' ||
         *End == '.') {
    if (*End == '.')
      Qualified = true;
    ++End;
  }
  formToken(Tok, End, Qualified ? tok::qualidentifier : tok::identifier);
  Tok.Ptr = Start;
}

void Lexer::keyword(Token &Tok) {
  const char *Start = CurPtr;
  const char *End = CurPtr + 1;
  while (charinfo::isLetter(*End) || charinfo::isDigit(*End))
    ++End;
  // Exclude '%' from compare.
  llvm::StringRef Keyword = llvm::StringRef(Start + 1, End - Start - 1);
  tok::TokenKind Kind = llvm::StringSwitch<tok::TokenKind>(Keyword)
                            .Case("eoi", tok::kw_eoi)
                            .Case("define", tok::kw_define)
                            .Case("if", tok::kw_if)
                            .Case("language", tok::kw_language)
                            .Case("start", tok::kw_start)
                            .Case("token", tok::kw_token)
                            .Default(tok::unknown);
  if (Kind == tok::unknown)
    Diag.error(Start, "unrecognized keyword");
  formToken(Tok, End, Kind);
}

void Lexer::code(Token &Tok, char Open, const char Close, tok::TokenKind Kind) {
  const char *Start = CurPtr;
  const char *End = CurPtr + 1;
  const bool Dot = *End == '.';
  if (Dot) {
    do {
      ++End;
      while (*End && *End != Close)
        ++End;
    } while (Dot && *End && Start + 1 < End && End[-1] != '.');
  } else {
    unsigned Level = 1;
    while (*End && (*End != Close || --Level)) {
      if (*End == Open)
        ++Level;
      ++End;
    }
  }
  if (!*End)
    Diag.error(Start, "unterminated code");
  formToken(Tok, End + 1, Kind);
}

void Lexer::string(Token &Tok) {
  const char *Start = CurPtr;
  const char *End = CurPtr + 1;
  while (*End && *End != *Start && *CurPtr != '\n' && *CurPtr != '\r')
    ++End;
  if (*CurPtr == '\n' || *CurPtr == '\r') {
    Diag.error(Start, "unterminated string");
  }
  formToken(Tok, End + 1, tok::string);
  Tok.Ptr = Start;
}

void Lexer::multilinecomment() {
  const char *Start = CurPtr;
  CurPtr += 2;
  do {
    while (*CurPtr && *CurPtr != '*')
      ++CurPtr;
    ++CurPtr;
  } while (*CurPtr && *CurPtr != '/');
  if (!*CurPtr)
    Diag.error(Start, "unterminated comment");
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