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
    const Token &NextToken() {
      Lex.next(Tok);
      llvm::StringRef buf = Lex.getBuffer();
      llvm::StringRef str = buf.substr(Tok.getLocation(), Tok.getLength());
      llvm::outs() << "Token: " << Tok.getName() << ": '" << str << "'\n";
      return Tok;
    }

    void ConsumeToken() {
      NextToken();
    }

    void ConsumeAnyToken() {}

    void ConsumeSemi() {}

    /// Expects and consume the token.
    /// Returns true in case of syntax error
    bool ExpectAndConsume(tok::TokenKind ExpectedTok, llvm::StringRef Msg = "") {
      if (Tok.is(ExpectedTok)) {
        ConsumeToken();
        return false;
      }
      llvm::outs() << "Error: Unexpected token " << Tok.getName() << "\n";
      llvm::outs() << "         Expected token " << tok::getTokenName(ExpectedTok) << "\n";
      return true;
    }

  public:
    Parser(Lexer& Lex);

    void Initialize();

    const LangOptions &getLangOpts() const { return Lex.getLangOpts(); }

    /* Parser implementation */
    void ParseNumber();
    void ParseString();
    void ParseQualident();
    void ParseConstantDeclaration();
    void ParseConstExpression();
    void ParseTypeDeclaration();
    void ParseType();
    void ParseSimpleType();
    void ParseEnumeration();
    void ParseIdentList();
    void ParseSubrangeType();
    void ParseArrayType();
    void ParseRecordType();
    void ParseFieldListSequence();
    void ParseFieldList();
    void ParseVariant();
    void ParseCaseLabelList();
    void ParseCaseLabels();
    void ParseSetType();
    void ParsePointerType();
    void ParseProcedureType();
    void ParseFormalTypeList();
    void ParseVariableDeclaration();
    void ParseDesignator();
    void ParseExpList();
    void ParseExpression();
    void ParseRelation();
    void ParseSimpleExpression();
    void ParseAddOperator();
    void ParseTerm();
    void ParseMulOperator();
    void ParseFactor();
    void ParseSet();
    void ParseElement();
    void ParseActualParameters();
    void ParseStatement();
    void ParseStatementSequence();
    void ParseIfStatement();
    void ParseCaseStatement();
    void ParseCase();
    void ParseWhileStatement();
    void ParseRepeatStatement();
    void ParseForStatement();
    void ParseLoopStatement();
    void ParseWithStatement();
    void ParseProcedureDeclaration();
    void ParseProcedureHeading();
    void ParseBlock();
    void ParseDeclaration();
    void ParseFormalParameters();
    void ParseFPSection();
    void ParseFormalType();
    void ParseModuleDeclaration();
    void ParsePriority();
    void ParseExport();
    void ParseImport();

    void ParseActualParameter(); // ISO generics only
    void ParseActualModuleParameters(); // ISO generics only

    /// ISO generics: chapter 6.3.4
    void ParseFormalModuleParameter(); // ISO generics only

    /// ISO generics: chapter 6.3.4
    void ParseFormalModuleParameters(); // ISO generics only
    void ParseDefinitionModule(bool IsGenericModule);
    void ParseDefinition();
    void ParseProgramModule(bool IsImplModule, bool IsGenericModule);

    /// PIM4: chapter 14
    /// ISO: clause 6.1
    /// ISO generics: chapter 6.2.2
    void ParseCompilationUnit();
  };
} // end namespace m2lang
#endif