//===--- Parser.h - LALRtool parser -----------------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the LALRtool parser class.
///
//===----------------------------------------------------------------------===//

#ifndef LALRTOOL_PARSER_H
#define LALRTOOL_PARSER_H

#include "lalrtool/GrammarBuilder.h"
#include "lalrtool/Lexer.h"

namespace lalrtool {
class Node;

class Parser {
  llvm::SourceMgr &SrcMgr;
  Lexer Lex;
  Token Tok;

  GrammarBuilder Builder;

  void advance() { Lex.next(Tok); }

  bool consume(tok::TokenKind ExpectedTok) {
    if (Tok.is(ExpectedTok)) {
      advance();
      return false;
    }
    // There must be a better way!
    error();
    return true;
  }

  bool expect(tok::TokenKind ExpectedTok) {
    if (Tok.is(ExpectedTok)) {
      return false;
    }
    return true;
  }

  void error() { getDiag().error(Tok.getLoc(), "unexpected token"); }

public:
  Parser(llvm::SourceMgr &SrcMgr)
      : SrcMgr(SrcMgr), Lex(Lexer(SrcMgr)),
        Builder(GrammarBuilder(Lex.getDiagnostic())) {
    advance();
  }

  Diagnostic &getDiag() { return Lex.getDiagnostic(); }

  void parse(Grammar &G, VarStore &V);

private:
#define PARSER_DECLARATION
#include "lalrtool/lalrtool.g.inc"
#undef PARSER_DECLARATION
};
} // namespace lalrtool
#endif