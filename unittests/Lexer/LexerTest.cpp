//===- unittests/Lexer/LexerTest.cpp ------ Lexer tests -------------------===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "m2lang/Lexer/Lexer.h"
#include "gtest/gtest.h"

using namespace m2lang;

namespace {

TEST(LexerTest, operatorTest) {
    LangOptions langOpts;
    langOpts.PIM = 1;
    llvm::StringRef Input("+ - * / := . , ; ( [ { ^ = # < > <= >= .. : ) ] } |");
    std::unique_ptr<llvm::MemoryBuffer> InputBuffer = llvm::MemoryBuffer::getMemBuffer(Input);
    SourceMgr SrcMgr;
    DiagnosticsEngine Diags(SrcMgr);
    SrcMgr.AddNewSourceBuffer(std::move(InputBuffer), llvm::SMLoc());
    auto lexer = Lexer(SrcMgr, Diags, langOpts);
    Token token;
    lexer.next(token); EXPECT_EQ(tok::plus, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::minus, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::star, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::slash, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::colonequal, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::period, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::comma, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::semi, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::l_paren, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::l_square, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::l_brace, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::caret, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::equal, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::hash, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::less, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::greater, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::lessequal, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::greaterequal, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::ellipsis, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::colon, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::r_paren, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::r_square, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::r_brace, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::pipe, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::eof, token.getKind());
}

TEST(LexerTest, trigraphs1Test) {
    LangOptions langOpts;
    langOpts.ISO = 1;
    llvm::StringRef Input("(! !) (: :)");
    std::unique_ptr<llvm::MemoryBuffer> InputBuffer = llvm::MemoryBuffer::getMemBuffer(Input);
    SourceMgr SrcMgr;
    DiagnosticsEngine Diags(SrcMgr);
    SrcMgr.AddNewSourceBuffer(std::move(InputBuffer), llvm::SMLoc());
    auto lexer = Lexer(SrcMgr, Diags, langOpts);
    Token token;
    lexer.next(token); EXPECT_EQ(tok::l_square, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::r_square, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::l_brace, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::r_brace, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::eof, token.getKind());
}

TEST(LexerTest, trigraphs2Test) {
    LangOptions langOpts;
    langOpts.ISO = 1;
    llvm::StringRef Input("(!!)(::)");
    std::unique_ptr<llvm::MemoryBuffer> InputBuffer = llvm::MemoryBuffer::getMemBuffer(Input);
    SourceMgr SrcMgr;
    DiagnosticsEngine Diags(SrcMgr);
    SrcMgr.AddNewSourceBuffer(std::move(InputBuffer), llvm::SMLoc());
    auto lexer = Lexer(SrcMgr, Diags, langOpts);
    Token token;
    lexer.next(token); EXPECT_EQ(tok::l_square, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::r_square, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::l_brace, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::r_brace, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::eof, token.getKind());
}

TEST(LexerTest, aliasTest) {
    LangOptions langOpts;
    langOpts.ISO = 1;
    llvm::StringRef Input("&~!@");
    std::unique_ptr<llvm::MemoryBuffer> InputBuffer = llvm::MemoryBuffer::getMemBuffer(Input);
    SourceMgr SrcMgr;
    DiagnosticsEngine Diags(SrcMgr);
    SrcMgr.AddNewSourceBuffer(std::move(InputBuffer), llvm::SMLoc());
    auto lexer = Lexer(SrcMgr, Diags, langOpts);
    Token token;
    lexer.next(token); EXPECT_EQ(tok::kw_AND, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::kw_NOT, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::pipe, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::caret, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::eof, token.getKind());
}

TEST(LexerTest, numberTest) {
    LangOptions langOpts;
    langOpts.ISO = 1;
    llvm::StringRef Input("42 42H 42B 42C 42.42E+3");
    std::unique_ptr<llvm::MemoryBuffer> InputBuffer = llvm::MemoryBuffer::getMemBuffer(Input);
    SourceMgr SrcMgr;
    DiagnosticsEngine Diags(SrcMgr);
    SrcMgr.AddNewSourceBuffer(std::move(InputBuffer), llvm::SMLoc());
    auto lexer = Lexer(SrcMgr, Diags, langOpts);
    Token token;
    lexer.next(token); EXPECT_EQ(tok::integer_literal, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::integer_literal, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::integer_literal, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::char_literal, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::real_literal, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::eof, token.getKind());
}

TEST(LexerTest, ellipsisTest) {
    LangOptions langOpts;
    langOpts.ISO = 1;
    llvm::StringRef Input("0..100");
    std::unique_ptr<llvm::MemoryBuffer> InputBuffer = llvm::MemoryBuffer::getMemBuffer(Input);
    SourceMgr SrcMgr;
    DiagnosticsEngine Diags(SrcMgr);
    SrcMgr.AddNewSourceBuffer(std::move(InputBuffer), llvm::SMLoc());
    auto lexer = Lexer(SrcMgr, Diags, langOpts);
    Token token;
    lexer.next(token); EXPECT_EQ(tok::integer_literal, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::ellipsis, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::integer_literal, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::eof, token.getKind());
}

TEST(LexerTest, keywordsSorted) {
  const char *keywords[] = {
#define KEYWORD(NAME, FLAGS)                                                   \
  #NAME,
#include "m2lang/Basic/TokenKinds.def"
  nullptr,
  };
  for (size_t i = 1; keywords[i]; ++i)
  {
      int cmp = strcmp(keywords[i-1], keywords[i]);
      ASSERT_LT(cmp, 0);
  }
}

} // anonymous namespace
