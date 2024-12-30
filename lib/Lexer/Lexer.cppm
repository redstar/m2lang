//===--- Lexer.cppm - Modula-2 Language Lexer -----------------------------===//
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

module;

#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"

export module m2lang.lexer:Lexer;

import :Token;
import m2lang.basic;

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

export class Lexer {
  [[maybe_unused]] llvm::SourceMgr &SrcMgr;
  DiagnosticsEngine *Diags;

  const char *CurPtr;
  llvm::StringRef CurBuf;

  /// CurBuffer - This is the current buffer index we're
  /// lexing from as managed by the SourceMgr object.
  unsigned CurBuffer = 0;

  // LangOpts enabled by this language (cache).
  const LangOptions &LangOpts;

  KeywordFilter Keywords;

public:
  Lexer(llvm::SourceMgr &SrcMgr, DiagnosticsEngine &diags,
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

  llvm::SMLoc getLoc() { return llvm::SMLoc::getFromPointer(CurPtr); }
  void formTokenWithChars(Token &Result, const char *TokEnd,
                          tok::TokenKind Kind);
};
} // end namespace m2lang

using namespace m2lang;

namespace {
// Constants for TokenKinds.def
enum {
  KEYPIM = 0x1,
  KEYISO = 0x2,
  KEYISOGS = 0x4,
  KEYISOOO = 0x8,
  KEYR10 = 0x10,
  KEYM2P = 0x20,
  KEYALL = KEYPIM | KEYISO | KEYISOGS | KEYISOOO | KEYR10 | KEYM2P
};

/// How a keyword is treated in the selected standard.
enum KeywordStatus {
  KS_Disabled, // Disabled
  KS_Enabled   // Enabled
};

/// Translates flags as specified in TokenKinds.def into keyword status
/// in the given language standard.
static KeywordStatus getKeywordStatus(const LangOptions &LangOpts,
                                      unsigned Flags) {
  if (Flags == KEYALL)
    return KS_Enabled;
  if (LangOpts.PIM && (Flags & KEYPIM))
    return KS_Enabled;
  if (LangOpts.ISO && (Flags & KEYISO))
    return KS_Enabled;
  if (LangOpts.ISOGenerics && (Flags & KEYISOGS))
    return KS_Enabled;
  if (LangOpts.ISOObjects && (Flags & KEYISOOO))
    return KS_Enabled;
  if (LangOpts.M2R10 && (Flags & KEYR10))
    return KS_Enabled;
  if (LangOpts.M2Plus && (Flags & KEYM2P))
    return KS_Enabled;
  return KS_Disabled;
}
} // namespace

void KeywordFilter::addKeyword(llvm::StringRef Keyword,
                               tok::TokenKind TokenCode, unsigned Flags,
                               const LangOptions &LangOpts) {
  if (getKeywordStatus(LangOpts, Flags) == KS_Enabled) {
    HashTable.insert(std::make_pair(Keyword, TokenCode));
  }
}

void KeywordFilter::addKeywords(const LangOptions &LangOpts) {
  // Add keywords and tokens for the current language.
#define KEYWORD(NAME, FLAGS)                                                   \
  addKeyword(llvm::StringRef(#NAME), tok::kw_##NAME, FLAGS, LangOpts);
#include "m2lang/Basic/TokenKinds.def"
}

// TODO Optimize and move to separate file
namespace charinfo {
LLVM_READNONE inline bool isASCII(char c) {
  return static_cast<unsigned char>(c) <= 127;
}

LLVM_READNONE inline bool isVerticalWhitespace(char c) {
  return isASCII(c) && (c == '\r' || c == '\n');
}

LLVM_READNONE inline bool isHorizontalWhitespace(char c) {
  return isASCII(c) && (c == ' ' || c == '\t' || c == '\f' || c == '\v');
}

LLVM_READNONE inline bool isWhitespace(char c) {
  return isHorizontalWhitespace(c) || isVerticalWhitespace(c);
}

LLVM_READNONE inline bool isDigit(char c) {
  return isASCII(c) && c >= '0' && c <= '9';
}

LLVM_READNONE inline bool isOctalDigit(char c) {
  return isASCII(c) && c >= '0' && c <= '7';
}

LLVM_READNONE inline bool isHexDigit(char c) {
  return isASCII(c) && (isDigit(c) || (c >= 'A' && c <= 'F'));
}

LLVM_READNONE inline bool isIdentifierHead(char c) {
  return isASCII(c) &&
         (c == '_' || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'));
}

LLVM_READNONE inline bool isIdentifierBody(char c) {
  return isIdentifierHead(c) || isDigit(c);
}
} // namespace charinfo

void Lexer::next(Token &token) {
  while (*CurPtr && charinfo::isWhitespace(*CurPtr)) {
    ++CurPtr;
  }
  if (!*CurPtr) {
    token.Kind= tok::eof;
    return;
  }
  if (charinfo::isIdentifierHead(*CurPtr)) {
    identifier(token);
    return;
  } else if (charinfo::isDigit(*CurPtr)) {
    number(token);
    return;
  } else if (*CurPtr == '"' || *CurPtr == '\'') {
    string(token);
    return;
  } else {
    switch (*CurPtr) {
    case '=':
      formTokenWithChars(token, CurPtr + 1, tok::equal);
      break;
    case '#':
      formTokenWithChars(token, CurPtr + 1, tok::hash);
      break;
    case '+':
      formTokenWithChars(token, CurPtr + 1, tok::plus);
      break;
    case '-':
      formTokenWithChars(token, CurPtr + 1, tok::minus);
      break;
    case '*':
      if (*(CurPtr + 1) == '>' && (LangOpts.ISO || LangOpts.M2R10))
        formTokenWithChars(token, CurPtr + 2, tok::stargreater);
      else
        formTokenWithChars(token, CurPtr + 1, tok::star);
      break;
    case '/':
      formTokenWithChars(token, CurPtr + 1, tok::slash);
      break;
    case '(':
      if (*(CurPtr + 1) == '*') {
        comment(token);
      }
      else if (*(CurPtr + 1) == '!')
        formTokenWithChars(token, CurPtr + 2, tok::l_square);
      else if (*(CurPtr + 1) == ':')
        formTokenWithChars(token, CurPtr + 2, tok::l_brace);
      else
        formTokenWithChars(token, CurPtr + 1, tok::l_paren);
      break;
    case '[':
      formTokenWithChars(token, CurPtr + 1, tok::l_square);
      break;
    case '{':
      formTokenWithChars(token, CurPtr + 1, tok::l_brace);
      break;
    case ')':
      formTokenWithChars(token, CurPtr + 1, tok::r_paren);
      break;
    case ']':
      formTokenWithChars(token, CurPtr + 1, tok::r_square);
      break;
    case '}':
      formTokenWithChars(token, CurPtr + 1, tok::r_brace);
      break;
    case '^':
      formTokenWithChars(token, CurPtr + 1, tok::caret);
      break;
    case '|':
      formTokenWithChars(token, CurPtr + 1, tok::pipe);
      break;
    case ',':
      formTokenWithChars(token, CurPtr + 1, tok::comma);
      break;
    case ';':
      formTokenWithChars(token, CurPtr + 1, tok::semi);
      break;
    case '.':
      if (*(CurPtr + 1) == '.')
        formTokenWithChars(token, CurPtr + 2, tok::ellipsis);
      else
        formTokenWithChars(token, CurPtr + 1, tok::period);
      break;
    case ':':
      if (*(CurPtr + 1) == '=')
        formTokenWithChars(token, CurPtr + 2, tok::colonequal);
      else if (*(CurPtr + 1) == ')')
        formTokenWithChars(token, CurPtr + 2, tok::r_brace);
      else
        formTokenWithChars(token, CurPtr + 1, tok::colon);
      break;
    case '<':
      if (*(CurPtr + 1) == '=')
        formTokenWithChars(token, CurPtr + 2, tok::lessequal);
      else if (*(CurPtr + 1) == '>' && !LangOpts.M2R10)
        formTokenWithChars(token, CurPtr + 2, tok::hash);
      else if (*(CurPtr + 1) == '*' && (LangOpts.ISO || LangOpts.M2R10))
        formTokenWithChars(token, CurPtr + 2, tok::lessstar);
      else
        formTokenWithChars(token, CurPtr + 1, tok::less);
      break;
    case '>':
      if (*(CurPtr + 1) == '=')
        formTokenWithChars(token, CurPtr + 2, tok::greaterequal);
      else
        formTokenWithChars(token, CurPtr + 1, tok::greater);
      break;
    case '&':
      if (LangOpts.M2R10)
        Diags->report(getLoc(), diag::err_not_allowed_in_r10);
      formTokenWithChars(token, CurPtr + 1, tok::kw_AND);
      break;
    case '~':
      if (LangOpts.M2R10)
        Diags->report(getLoc(), diag::err_not_allowed_in_r10);
      formTokenWithChars(token, CurPtr + 1, tok::kw_NOT);
      break;
    case '!':
      if (!LangOpts.ISO)
        Diags->report(getLoc(), diag::err_requires_iso);
      if (*(CurPtr + 1) == ')')
        formTokenWithChars(token, CurPtr + 2, tok::r_square);
      else
        formTokenWithChars(token, CurPtr + 1, tok::pipe);
      break;
    case '@':
      if (!LangOpts.ISO)
        Diags->report(getLoc(), diag::err_requires_iso);
      formTokenWithChars(token, CurPtr + 1, tok::caret);
      break;
    default:
      token.Kind = tok::unknown;
    }
    return;
  }
}

void Lexer::identifier(Token &token) {
  const char *start = CurPtr;
  const char *end = CurPtr + 1;
  while (charinfo::isIdentifierBody(*end))
    ++end;
  llvm::StringRef Name(start, end - start);
  formTokenWithChars(token, end, Keywords.getKeyword(Name, tok::identifier));
}

void Lexer::number(Token &token) {
  const char *start = CurPtr;
  const char *end = CurPtr + 1;
  tok::TokenKind kind = tok::unknown;
  // TODO Check language variant
  bool maybeOctal = charinfo::isOctalDigit(*start);
  bool isHex = false;
  while (*end) {
    // End of number reached if digit is not a hexadecimal digit
    // Hexadecimal digits B and C require a check if they are really digits
    // or format specifiers.
    if (!charinfo::isHexDigit(*end) ||
        ((*end == 'B' || *end == 'C') && *(end + 1) &&
         !charinfo::isHexDigit(*(end + 1))))
      break;
    maybeOctal &= charinfo::isOctalDigit(*end);
    if (!charinfo::isDigit(*end))
      isHex = true;
    ++end;
  }
  switch (*end) {
  case 'B': /* octal number */
    if (!maybeOctal)
      Diags->report(getLoc(), diag::err_non_octal_digit_in_number);
    LLVM_FALLTHROUGH;
  case 'H': /* hex number */
    kind = tok::integer_literal;
    ++end;
    break;
  default: /* decimal number */
    if (isHex)
      Diags->report(getLoc(), diag::err_hex_digit_in_decimal);
    kind = tok::integer_literal;
    break;
  case 'C': /* octal char const */
    if (!maybeOctal)
      Diags->report(getLoc(), diag::err_non_octal_digit_in_char);
    kind = tok::char_literal;
    ++end;
    break;
  case '.': /* real number or .. */
    if (*(end + 1) == '.') {
      kind = tok::integer_literal;
      break;
    }
    kind = tok::real_literal;
    ++end;
    while (charinfo::isDigit(*end))
      ++end;
    if (*end == 'E') { // scale factor
      ++end;
      if (*end == '+' || *end == '-')
        ++end;
      if (!charinfo::isDigit(*end))
        Diags->report(getLoc(), diag::err_exponent_has_no_digits);
      while (charinfo::isDigit(*end))
        ++end;
    }
    break;
  }
  formTokenWithChars(token, end, kind);
}

void Lexer::string(Token &token) {
  const char *start = CurPtr;
  const char *end = CurPtr + 1;
  while (*end && *end != *start && !charinfo::isVerticalWhitespace(*end))
    ++end;
  if (charinfo::isVerticalWhitespace(*end)) {
    Diags->report(getLoc(), diag::err_unterminated_char_or_string);
  }
  formTokenWithChars(token, end + 1, tok::string_literal);
}

void Lexer::comment(Token &token) {
  const char *end = CurPtr + 2;
  unsigned level = 1;
  while (*end && level) {
    // Check for nested comment.
    if (*end == '(' && *(end + 1) == '*') {
      end += 2;
      level++;
    }
    // Check for end of comment
    else if (*end == '*' && *(end + 1) == ')') {
      end += 2;
      level--;
    } else
      ++end;
  }
  if (!*end) {
    Diags->report(getLoc(), diag::err_unterminated_block_comment);
  }
  formTokenWithChars(token, end, tok::comment);
}

void Lexer::formTokenWithChars(Token &Result, const char *TokEnd,
                               tok::TokenKind Kind) {
  size_t TokLen = TokEnd - CurPtr;
  Result.Ptr = CurPtr;
  Result.Length = TokLen;
  Result.Kind = Kind;
  CurPtr = TokEnd;
}