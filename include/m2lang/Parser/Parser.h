//===--- Parser.h - Modula-2 Language Parser --------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the parser interface.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_PARSER_PARSER_H
#define M2LANG_PARSER_PARSER_H

#include "m2lang/Basic/Diagnostic.h"
#include "m2lang/Basic/LangOptions.h"
#include "m2lang/Lexer/Lexer.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

namespace m2lang {
  class Parser {

    Lexer& Lex;

    /// Tok - The current token we are peeking ahead.  All parsing methods assume
    /// that this is valid.
    Token Tok;

    /// NextToken - This peeks ahead one token and returns it without
    /// consuming it.
    const Token &nextToken() {
      Lex.next(Tok);
      llvm::StringRef buf = Lex.getBuffer();
      llvm::StringRef str = buf.substr(Tok.getLocation(), Tok.getLength());
      llvm::outs() << "Token: " << Tok.getName() << ": '" << str << "'\n";
      return Tok;
    }

    void consumeToken() {
      nextToken();
    }

    void consumeAnyToken() {}

    void consumeSemi() {}

    /// Expects and consume the token.
    /// Returns true in case of syntax error
    bool expectAndConsume(tok::TokenKind ExpectedTok, llvm::StringRef Msg = "") {
      if (Tok.is(ExpectedTok)) {
        consumeToken();
        return false;
      }
      llvm::outs() << "Error: Unexpected token " << Tok.getName() << "\n";
      llvm::outs() << "         Expected token " << tok::getTokenName(ExpectedTok) << "\n";
      return true;
    }

  public:
    Parser(Lexer& Lex);

    void initialize();

    const LangOptions &getLangOpts() const { return Lex.getLangOpts(); }

    /* Parser implementation */
    void parseNumber();
    void parseString();
    void parseQualident();
    void parseConstantDeclaration();
    void parseConstExpression();
    void parseTypeDeclaration();
    void parseType();
    void parseSimpleType();
    void parseEnumeration();
    void parseIdentList();
    void parseSubrangeType();
    void parseArrayType();
    void parseRecordType();
    void parseFieldListSequence();
    void parseFieldList();
    void parseVariant();
    void parseCaseLabelList();
    void parseCaseLabels();
    void parseSetType();
    void parsePointerType();
    void parseProcedureType();
    void parseFormalTypeList();
    void parseVariableDeclaration();
    void parseDesignator();
    void parseSelector();
    void parseExpList();
    void parseExpression();
    void parseRelation();
    void parseSimpleExpression();
    void parseAddOperator();
    void parseTerm();
    void parseMulOperator();
    void parseFactor();
    void parseSetValues();
    void parseElement();
    void parseActualParameters();
    void parseStatement();
    void parseStatementSequence();
    void parseIfStatement();
    void parseCaseStatement();
    void parseCase();
    void parseWhileStatement();
    void parseRepeatStatement();
    void parseForStatement();
    void parseLoopStatement();
    void parseWithStatement();
    void parseProcedureDeclaration();
    void parseProcedureHeading();
    void parseBlock();
    void parseDeclaration();
    void parseFormalParameters();
    void parseFPSection();
    void parseFormalType();
    void parseModuleDeclaration();
    void parsePriority();
    void parseExport();
    void parseImport();

    void parseActualParameter(); // ISO generics only
    void parseActualModuleParameters(); // ISO generics only

    /// ISO generics: chapter 6.3.4
    void parseFormalModuleParameter(); // ISO generics only

    /// ISO generics: chapter 6.3.4
    void parseFormalModuleParameters(); // ISO generics only
    void parseDefinitionModule(bool IsGenericModule);
    void parseDefinition();
    void parseProgramModule(bool IsImplModule, bool IsGenericModule);

    /// PIM4: chapter 14
    /// ISO: clause 6.1
    /// ISO generics: chapter 6.2.2
    void parseCompilationUnit();
  };
} // end namespace m2lang
#endif