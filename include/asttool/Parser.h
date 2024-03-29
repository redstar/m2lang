//===--- Parser.h - ASTtool parser ------------------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the ASTtool parser class.
///
//===----------------------------------------------------------------------===//

#ifndef ASTTOOL_PARSER_H
#define ASTTOOL_PARSER_H

#include "asttool/ClassBuilder.h"
#include "asttool/Lexer.h"

namespace asttool {
class Node;
class VarStore;

class Parser {
  Lexer Lex;
  Token Tok;

  ClassBuilder Builder;

  void advance() { Lex.next(Tok); }

  bool consume(tok::TokenKind ExpectedTok) {
    if (expect(ExpectedTok))
      return true;
    advance();
    return false;
  }

  bool expect(tok::TokenKind ExpectedTok) {
    if (Tok.is(ExpectedTok)) {
      return false;
    }
    // There must be a better way!
    error();
    return true;
  }

  void error() { getDiag().error(Tok.getLoc(), "unexpected token"); }

public:
  Parser(llvm::SourceMgr &SrcMgr)
      : Lex(Lexer(SrcMgr)), Builder(ClassBuilder(Lex.getDiagnostic())) {
    advance();
  }

  Diagnostic &getDiag() { return Lex.getDiagnostic(); }

  ASTDefinition parse(VarStore &Vars);

private:
#define PARSER_DECLARATION
#include "asttool/asttool.inc"
#undef PARSER_DECLARATION
};
} // namespace asttool
#endif