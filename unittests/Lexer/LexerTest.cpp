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
    llvm::StringRef input("+ - * / := . , ; ( [ { ^ = # < > <= >= .. : ) ] } |");
    std::unique_ptr<llvm::MemoryBuffer> inputBuffer = llvm::MemoryBuffer::getMemBuffer(input);
    auto lexer = Lexer(inputBuffer.get(), langOpts);
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
    langOpts.Trigraphs = 1;
    llvm::StringRef input("(! !) (: :)");
    std::unique_ptr<llvm::MemoryBuffer> inputBuffer = llvm::MemoryBuffer::getMemBuffer(input);
    auto lexer = Lexer(inputBuffer.get(), langOpts);
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
    langOpts.Trigraphs = 1;
    llvm::StringRef input("(!!)(::)");
    std::unique_ptr<llvm::MemoryBuffer> inputBuffer = llvm::MemoryBuffer::getMemBuffer(input);
    auto lexer = Lexer(inputBuffer.get(), langOpts);
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
    llvm::StringRef input("&~!@");
    std::unique_ptr<llvm::MemoryBuffer> inputBuffer = llvm::MemoryBuffer::getMemBuffer(input);
    auto lexer = Lexer(inputBuffer.get(), langOpts);
    Token token;
    lexer.next(token); EXPECT_EQ(tok::kw_AND, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::kw_NOT, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::pipe, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::caret, token.getKind());
    lexer.next(token); EXPECT_EQ(tok::eof, token.getKind());
}

} // anonymous namespace
