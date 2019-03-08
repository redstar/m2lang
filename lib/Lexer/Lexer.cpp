//===--- Lexer.cpp - Modula-2 Language Lexer --------------------*- C++ -*-===//
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

#include "m2lang/Lexer/Lexer.h"

using namespace m2lang;

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

  LLVM_READNONE inline bool isIdentifierHead(char c) {
    return isASCII(c) && (c == '_'
    || (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z'));
  }

  LLVM_READNONE inline bool isIdentifierBody(char c) {
    return isIdentifierHead(c) || isDigit(c);
  }
}

void Lexer::next(Token &token) {
  while (*BufferPtr && charinfo::isWhitespace(*BufferPtr)) {
    ++BufferPtr;
  }
  if (!*BufferPtr) {
    token.setKind(tok::eof);
    return;
  }
  if (charinfo::isIdentifierHead(*BufferPtr)) {
    identifier(token);
    return;
  }
  else if (charinfo::isDigit(*BufferPtr)) {
    number(token);
    return;
  }
  else if (*BufferPtr == '"' || *BufferPtr == '\'') {
    string(token);
    return;
  }
  else {
    switch (*BufferPtr++) {
      case '=': FormTokenWithChars(token, BufferPtr, tok::equal); break;
      case '+': FormTokenWithChars(token, BufferPtr, tok::plus); break;
      case '-': FormTokenWithChars(token, BufferPtr, tok::minus); break;
      case '*': FormTokenWithChars(token, BufferPtr, tok::star); break;
      case '/': FormTokenWithChars(token, BufferPtr, tok::slash); break;
      case '(':
        if (*BufferPtr == '*')
          comment(token);
        else if (*BufferPtr == '!' && LangOpts.Trigraphs)
          FormTokenWithChars(token, BufferPtr + 1, tok::l_square);
        else if (*BufferPtr == ':' && LangOpts.Trigraphs)
          FormTokenWithChars(token, BufferPtr + 1, tok::l_brace);
        else
          FormTokenWithChars(token, BufferPtr, tok::l_paren);
        break;
      case '[': FormTokenWithChars(token, BufferPtr, tok::l_square); break;
      case '{': FormTokenWithChars(token, BufferPtr, tok::l_brace); break;
      case ')': FormTokenWithChars(token, BufferPtr, tok::r_paren); break;
      case ']': FormTokenWithChars(token, BufferPtr, tok::r_square); break;
      case '}': FormTokenWithChars(token, BufferPtr, tok::r_brace); break;
      case '^': FormTokenWithChars(token, BufferPtr, tok::caret); break;
      case '|': FormTokenWithChars(token, BufferPtr, tok::pipe); break;
      case ',': FormTokenWithChars(token, BufferPtr, tok::comma); break;
      case ';': FormTokenWithChars(token, BufferPtr, tok::semi); break;
      case '.':
        if (*BufferPtr == '.')
          FormTokenWithChars(token, BufferPtr + 1, tok::ellipsis);
        else
          FormTokenWithChars(token, BufferPtr, tok::period);
        break;
      case ':':
        if (*BufferPtr == '=')
          FormTokenWithChars(token, BufferPtr+1, tok::colonequal);
        else if (*BufferPtr == ')' && LangOpts.Trigraphs)
          FormTokenWithChars(token, BufferPtr + 1, tok::r_brace);
        else
          FormTokenWithChars(token, BufferPtr, tok::colon);
        break;
      case '<':
        if (*BufferPtr == '=')
          FormTokenWithChars(token, BufferPtr + 1, tok::lessequal);
        else if (*BufferPtr == '>' && !LangOpts.M2R10)
          FormTokenWithChars(token, BufferPtr + 1, tok::hash);
        else if (*BufferPtr == '*' && (LangOpts.ISO || LangOpts.M2R10))
          directive(token);
        else
          FormTokenWithChars(token, BufferPtr, tok::less);
        break;
      case '>':
        if (*BufferPtr == '=')
          FormTokenWithChars(token, BufferPtr + 1, tok::greaterequal);
        else
          FormTokenWithChars(token, BufferPtr, tok::greater);
        break;
      if (!LangOpts.M2R10) {
        case '&': FormTokenWithChars(token, BufferPtr, tok::kw_AND); break;
        case '~': FormTokenWithChars(token, BufferPtr, tok::kw_NOT); break;
      }
      if (LangOpts.ISO) {
        case '!':
          if (*BufferPtr == ')' && LangOpts.Trigraphs)
            FormTokenWithChars(token, BufferPtr + 1, tok::r_square);
          else
            FormTokenWithChars(token, BufferPtr, tok::pipe);
          break;
        case '@': FormTokenWithChars(token, BufferPtr, tok::caret); break;
      }
      default:
        token.setKind(tok::unknown);
    }
    return;
  }
}

void Lexer::identifier(Token &token) {
  const char *start = BufferPtr;
  const char *end = BufferPtr+1;
  while (charinfo::isIdentifierBody(*end))
    ++end;
  llvm::StringRef Name(start, end-start);

  // TODO Check language variant
  tok::TokenKind kind = llvm::StringSwitch<tok::TokenKind>(Name)
#define KEYWORD(Keyword,Conditions) .Case(#Keyword, tok::kw_ ## Keyword)
#include "m2lang/Basic/TokenKinds.def"
    .Default(tok::identifier);

  FormTokenWithChars(token, end, kind);
}

void Lexer::number(Token &token) {
  const char *start = BufferPtr;
  const char *end = BufferPtr + 1;
  // TODO Implement
  FormTokenWithChars(token, end, tok::numeric_constant);
}

void Lexer::string(Token &token) {
  const char *start = BufferPtr;
  const char *end = BufferPtr + 1;
  while (*end && *end != *start && !charinfo::isVerticalWhitespace(*end))
    ++end;
  if (charinfo::isVerticalWhitespace(*end)) {
    // TODO Run-away string; emit error message
  }
  FormTokenWithChars(token, end, tok::string_literal);
}

void Lexer::comment(Token &token) {
  const char *start = BufferPtr - 1;
  const char *end = BufferPtr + 1;
  unsigned level = 1;
  while (*end && level)
  {
    // Check for nested comment.
    if (*end == '(' && *(end + 1) == '*') {
      end += 2;
      level++;
    }
    // Check for end of comment
    else if (*end == '*' && *(end +1) == ')') {
      end += 2;
      level--;
    }
    else
      ++end;
  }
  if (!*end) {
    // TODO Run-away comment; emit error message
  }
  FormTokenWithChars(token, end, tok::comment);
}

void Lexer::directive(Token &token) {
  const char *start = BufferPtr;
  const char *end = BufferPtr + 1;
  // TODO Implement
  FormTokenWithChars(token, end, tok::directive);
}

void Lexer::FormTokenWithChars(Token &token, const char *tokEnd,
                               tok::TokenKind kind)
{
  token.setKind(kind);
  BufferPtr = tokEnd;
}