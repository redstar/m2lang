//===--- Parser.cpp - Modula-2 Language Parser ------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the parser implementation.
///
//===----------------------------------------------------------------------===//

#include "m2lang/Parser/Parser.h"
#include "m2lang/Basic/TokenKinds.h"

using namespace m2lang;
using namespace llvm;

Parser::Parser(Lexer& Lex) : Lex(Lex) { }

void Parser::Initialize() {}

void Parser::ParseNumber() {
    if (Tok.getKind() == tok::integer_literal) {
        ConsumeToken();
    }
    else if (Tok.getKind() == tok::real_literal) {
        ConsumeToken();
    }
}

void Parser::ParseString() {
    if (Tok.getKind() == tok::string_literal) {
        ConsumeToken();
    }
    else if (Tok.getKind() == tok::char_literal) {
        ConsumeToken();
    }
}

void Parser::ParseQualident() {
    ExpectAndConsume(tok::identifier, "error msg");
    while (Tok.getKind() == tok::period /* Unresolved LL(1) conflict */) {
        ConsumeToken();
        ExpectAndConsume(tok::identifier, "error msg");
    }
}

void Parser::ParseConstantDeclaration() {
    ExpectAndConsume(tok::identifier, "error msg");
    ExpectAndConsume(tok::equal, "error msg");
    ParseConstExpression();
}

void Parser::ParseConstExpression() {
    ParseExpression();
}

void Parser::ParseTypeDeclaration() {
    ExpectAndConsume(tok::identifier, "error msg");
    ExpectAndConsume(tok::equal, "error msg");
    ParseType();
}

void Parser::ParseType() {
    if (Tok.isOneOf(tok::l_paren, tok::l_square, tok::identifier)) {
        ParseSimpleType();
    }
    else if (Tok.getKind() == tok::kw_ARRAY) {
        ParseArrayType();
    }
    else if (Tok.getKind() == tok::kw_RECORD) {
        ParseRecordType();
    }
    else if (Tok.getKind() == tok::kw_SET) {
        ParseSetType();
    }
    else if (Tok.getKind() == tok::kw_POINTER) {
        ParsePointerType();
    }
    else if (Tok.getKind() == tok::kw_PROCEDURE) {
        ParseProcedureType();
    }
}

void Parser::ParseSimpleType() {
    if (Tok.getKind() == tok::identifier /* Unresolved LL(1) conflict */) {
        ParseQualident();
    }
    else if (Tok.getKind() == tok::l_paren) {
        ParseEnumeration();
    }
    else if (Tok.isOneOf(tok::l_square, tok::identifier)) {
        ParseSubrangeType();
    }
}

void Parser::ParseEnumeration() {
    ExpectAndConsume(tok::l_paren, "error msg");
    ParseIdentList();
    ExpectAndConsume(tok::r_paren, "error msg");
}

void Parser::ParseIdentList() {
    ExpectAndConsume(tok::identifier, "error msg");
    while (Tok.getKind() == tok::comma) {
        ConsumeToken();
        ExpectAndConsume(tok::identifier, "error msg");
    }
}

void Parser::ParseSubrangeType() {
    if (Tok.getKind() == tok::identifier) {
        ConsumeToken();
    }
    ExpectAndConsume(tok::l_square, "error msg");
    ParseConstExpression();
    ExpectAndConsume(tok::ellipsis, "error msg");
    ParseConstExpression();
    ExpectAndConsume(tok::r_square, "error msg");
}

void Parser::ParseArrayType() {
    ExpectAndConsume(tok::kw_ARRAY, "error msg");
    ParseSimpleType();
    while (Tok.getKind() == tok::comma) {
        ConsumeToken();
        ParseSimpleType();
    }
    ExpectAndConsume(tok::kw_OF, "error msg");
    ParseType();
}

void Parser::ParseRecordType() {
    ExpectAndConsume(tok::kw_RECORD, "error msg");
    ParseFieldListSequence();
    ExpectAndConsume(tok::kw_END, "error msg");
}

void Parser::ParseFieldListSequence() {
    ParseFieldList();
    while (Tok.getKind() == tok::semi) {
        ConsumeToken();
        ParseFieldList();
    }
}

void Parser::ParseFieldList() {
    if (Tok.isOneOf(tok::kw_CASE, tok::identifier)) {
        if (Tok.getKind() == tok::identifier) {
            ParseIdentList();
            ExpectAndConsume(tok::colon, "error msg");
            ParseType();
        }
        else if (Tok.getKind() == tok::kw_CASE) {
            ConsumeToken();
            if (Tok.getKind() == tok::identifier) {
                ConsumeToken();
            }
            ExpectAndConsume(tok::colon, "error msg");
            ParseQualident();
            ExpectAndConsume(tok::kw_OF, "error msg");
            ParseVariant();
            while (Tok.getKind() == tok::pipe) {
                ConsumeToken();
                ParseVariant();
            }
            if (Tok.getKind() == tok::kw_ELSE) {
                ConsumeToken();
                ParseFieldListSequence();
            }
            ExpectAndConsume(tok::kw_END, "error msg");
        }
    }
}

void Parser::ParseVariant() {
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::l_brace, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
        ParseCaseLabelList();
        ExpectAndConsume(tok::colon, "error msg");
        ParseFieldListSequence();
    }
}

void Parser::ParseCaseLabelList() {
    ParseCaseLabels();
    while (Tok.getKind() == tok::comma) {
        ConsumeToken();
        ParseCaseLabels();
    }
}

void Parser::ParseCaseLabels() {
    ParseConstExpression();
    if (Tok.getKind() == tok::ellipsis) {
        ConsumeToken();
        ParseConstExpression();
    }
}

void Parser::ParseSetType() {
    ExpectAndConsume(tok::kw_SET, "error msg");
    ExpectAndConsume(tok::kw_OF, "error msg");
    ParseSimpleType();
}

void Parser::ParsePointerType() {
    ExpectAndConsume(tok::kw_POINTER, "error msg");
    ExpectAndConsume(tok::kw_TO, "error msg");
    ParseType();
}

void Parser::ParseProcedureType() {
    ExpectAndConsume(tok::kw_PROCEDURE, "error msg");
    if (Tok.getKind() == tok::l_paren) {
        ParseFormalTypeList();
    }
}

void Parser::ParseFormalTypeList() {
    ExpectAndConsume(tok::l_paren, "error msg");
    if (Tok.isOneOf(tok::kw_ARRAY, tok::kw_VAR, tok::identifier)) {
        if (Tok.getKind() == tok::kw_VAR) {
            ConsumeToken();
        }
        ParseFormalType();
        while (Tok.getKind() == tok::comma) {
            ConsumeToken();
            if (Tok.getKind() == tok::kw_VAR) {
                ConsumeToken();
            }
            ParseFormalType();
        }
    }
    ExpectAndConsume(tok::r_paren, "error msg");
    if (Tok.getKind() == tok::colon) {
        ConsumeToken();
        ParseQualident();
    }
}

void Parser::ParseVariableDeclaration() {
    ParseIdentList();
    ExpectAndConsume(tok::colon, "error msg");
    ParseType();
}

void Parser::ParseDesignator() {
    ParseQualident();
    while (Tok.isOneOf(tok::period, tok::l_square, tok::caret)) {
        if (Tok.getKind() == tok::period) {
            ConsumeToken();
            ExpectAndConsume(tok::identifier, "error msg");
        }
        else if (Tok.getKind() == tok::l_square) {
            ConsumeToken();
            ParseExpList();
            ExpectAndConsume(tok::r_square, "error msg");
        }
        else if (Tok.getKind() == tok::caret) {
            ConsumeToken();
        }
    }
}

void Parser::ParseExpList() {
    ParseExpression();
    while (Tok.getKind() == tok::comma) {
        ConsumeToken();
        ParseExpression();
    }
}

void Parser::ParseExpression() {
    ParseSimpleExpression();
    if (Tok.isOneOf(tok::hash, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_IN)) {
        ParseRelation();
        ParseSimpleExpression();
    }
}

void Parser::ParseRelation() {
    if (Tok.getKind() == tok::equal) {
        ConsumeToken();
    }
    else if (Tok.getKind() == tok::hash) {
        ConsumeToken();
    }
    else if (Tok.getKind() == tok::less) {
        ConsumeToken();
    }
    else if (Tok.getKind() == tok::lessequal) {
        ConsumeToken();
    }
    else if (Tok.getKind() == tok::greater) {
        ConsumeToken();
    }
    else if (Tok.getKind() == tok::greaterequal) {
        ConsumeToken();
    }
    else if (Tok.getKind() == tok::kw_IN) {
        ConsumeToken();
    }
}

void Parser::ParseSimpleExpression() {
    if (Tok.isOneOf(tok::plus, tok::minus)) {
        if (Tok.getKind() == tok::plus) {
            ConsumeToken();
        }
        else if (Tok.getKind() == tok::minus) {
            ConsumeToken();
        }
    }
    ParseTerm();
    while (Tok.isOneOf(tok::plus, tok::minus, tok::kw_OR)) {
        ParseAddOperator();
        ParseTerm();
    }
}

void Parser::ParseAddOperator() {
    if (Tok.getKind() == tok::plus) {
        ConsumeToken();
    }
    else if (Tok.getKind() == tok::minus) {
        ConsumeToken();
    }
    else if (Tok.getKind() == tok::kw_OR) {
        ConsumeToken();
    }
}

void Parser::ParseTerm() {
    ParseFactor();
    while (Tok.isOneOf(tok::star, tok::slash, tok::kw_AND, tok::kw_DIV, tok::kw_MOD)) {
        ParseMulOperator();
        ParseFactor();
    }
}

void Parser::ParseMulOperator() {
    if (Tok.getKind() == tok::star) {
        ConsumeToken();
    }
    else if (Tok.getKind() == tok::slash) {
        ConsumeToken();
    }
    else if (Tok.getKind() == tok::kw_DIV) {
        ConsumeToken();
    }
    else if (Tok.getKind() == tok::kw_MOD) {
        ConsumeToken();
    }
    else if (Tok.getKind() == tok::kw_AND) {
        ConsumeToken();
    }
}

void Parser::ParseFactor() {
    if (Tok.isOneOf(tok::integer_literal, tok::real_literal)) {
        ParseNumber();
    }
    else if (Tok.isOneOf(tok::char_literal, tok::string_literal)) {
        ParseString();
    }
    else if (Tok.isOneOf(tok::l_brace, tok::identifier /* Unresolved LL(1) conflict */)) {
        ParseSet();
    }
    else if (Tok.getKind() == tok::identifier) {
        ParseDesignator();
        if (Tok.getKind() == tok::l_paren) {
            ParseActualParameters();
        }
    }
    else if (Tok.getKind() == tok::l_paren) {
        ConsumeToken();
        ParseExpression();
        ExpectAndConsume(tok::r_paren, "error msg");
    }
    else if (Tok.getKind() == tok::kw_NOT) {
        ConsumeToken();
        ParseFactor();
    }
}

void Parser::ParseSet() {
    if (Tok.getKind() == tok::identifier) {
        ParseQualident();
    }
    ExpectAndConsume(tok::l_brace, "error msg");
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::l_brace, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
        ParseElement();
        while (Tok.getKind() == tok::comma) {
            ConsumeToken();
            ParseElement();
        }
    }
    ExpectAndConsume(tok::r_brace, "error msg");
}

void Parser::ParseElement() {
    ParseExpression();
    if (Tok.getKind() == tok::ellipsis) {
        ConsumeToken();
        ParseExpression();
    }
}

void Parser::ParseActualParameters() {
    ExpectAndConsume(tok::l_paren, "error msg");
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::l_brace, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
        ParseExpList();
    }
    ExpectAndConsume(tok::r_paren, "error msg");
}

void Parser::ParseStatement() {
    if (Tok.isOneOf(tok::kw_CASE, tok::kw_EXIT, tok::kw_FOR, tok::kw_IF, tok::kw_LOOP, tok::kw_REPEAT, tok::kw_RETURN, tok::kw_WHILE, tok::kw_WITH, tok::identifier)) {
        if (Tok.getKind() == tok::identifier /* Unresolved LL(1) conflict */) {
            ParseAssignment();
        }
        else if (Tok.getKind() == tok::identifier) {
            ParseProcedureCall();
        }
        else if (Tok.getKind() == tok::kw_IF) {
            ParseIfStatement();
        }
        else if (Tok.getKind() == tok::kw_CASE) {
            ParseCaseStatement();
        }
        else if (Tok.getKind() == tok::kw_WHILE) {
            ParseWhileStatement();
        }
        else if (Tok.getKind() == tok::kw_REPEAT) {
            ParseRepeatStatement();
        }
        else if (Tok.getKind() == tok::kw_LOOP) {
            ParseLoopStatement();
        }
        else if (Tok.getKind() == tok::kw_FOR) {
            ParseForStatement();
        }
        else if (Tok.getKind() == tok::kw_WITH) {
            ParseWithStatement();
        }
        else if (Tok.getKind() == tok::kw_EXIT) {
            ConsumeToken();
        }
        else if (Tok.getKind() == tok::kw_RETURN) {
            ConsumeToken();
            if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::l_brace, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
                ParseExpression();
            }
        }
    }
}

void Parser::ParseAssignment() {
    ParseDesignator();
    ExpectAndConsume(tok::colonequal, "error msg");
    ParseExpression();
}

void Parser::ParseProcedureCall() {
    ParseDesignator();
    if (Tok.getKind() == tok::l_paren) {
        ParseActualParameters();
    }
}

void Parser::ParseStatementSequence() {
    ParseStatement();
    while (Tok.getKind() == tok::semi) {
        ConsumeToken();
        ParseStatement();
    }
}

void Parser::ParseIfStatement() {
    ExpectAndConsume(tok::kw_IF, "error msg");
    ParseExpression();
    ExpectAndConsume(tok::kw_THEN, "error msg");
    ParseStatementSequence();
    while (Tok.getKind() == tok::kw_ELSIF) {
        ConsumeToken();
        ParseExpression();
        ExpectAndConsume(tok::kw_THEN, "error msg");
        ParseStatementSequence();
    }
    if (Tok.getKind() == tok::kw_ELSE) {
        ConsumeToken();
        ParseStatementSequence();
    }
    ExpectAndConsume(tok::kw_END, "error msg");
}

void Parser::ParseCaseStatement() {
    ExpectAndConsume(tok::kw_CASE, "error msg");
    ParseExpression();
    ExpectAndConsume(tok::kw_OF, "error msg");
    ParseCase();
    while (Tok.getKind() == tok::pipe) {
        ConsumeToken();
        ParseCase();
    }
    if (Tok.getKind() == tok::kw_ELSE) {
        ConsumeToken();
        ParseStatementSequence();
    }
    ExpectAndConsume(tok::kw_END, "error msg");
}

void Parser::ParseCase() {
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::l_brace, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
        ParseCaseLabelList();
        ExpectAndConsume(tok::colon, "error msg");
        ParseStatementSequence();
    }
}

void Parser::ParseWhileStatement() {
    ExpectAndConsume(tok::kw_WHILE, "error msg");
    ParseExpression();
    ExpectAndConsume(tok::kw_DO, "error msg");
    ParseStatementSequence();
    ExpectAndConsume(tok::kw_END, "error msg");
}

void Parser::ParseRepeatStatement() {
    ExpectAndConsume(tok::kw_REPEAT, "error msg");
    ParseStatementSequence();
    ExpectAndConsume(tok::kw_UNTIL, "error msg");
    ParseExpression();
}

void Parser::ParseForStatement() {
    ExpectAndConsume(tok::kw_FOR, "error msg");
    ExpectAndConsume(tok::identifier, "error msg");
    ExpectAndConsume(tok::colonequal, "error msg");
    ParseExpression();
    ExpectAndConsume(tok::kw_TO, "error msg");
    ParseExpression();
    if (Tok.getKind() == tok::kw_BY) {
        ConsumeToken();
        ParseConstExpression();
    }
    ExpectAndConsume(tok::kw_DO, "error msg");
    ParseStatementSequence();
    ExpectAndConsume(tok::kw_END, "error msg");
}

void Parser::ParseLoopStatement() {
    ExpectAndConsume(tok::kw_LOOP, "error msg");
    ParseStatementSequence();
    ExpectAndConsume(tok::kw_END, "error msg");
}

void Parser::ParseWithStatement() {
    ExpectAndConsume(tok::kw_WITH, "error msg");
    ParseDesignator();
    ExpectAndConsume(tok::kw_DO, "error msg");
    ParseStatementSequence();
    ExpectAndConsume(tok::kw_END, "error msg");
}

void Parser::ParseProcedureDeclaration() {
    ParseProcedureHeading();
    ExpectAndConsume(tok::semi, "error msg");
    ParseBlock();
    ExpectAndConsume(tok::identifier, "error msg");
}

void Parser::ParseProcedureHeading() {
    ExpectAndConsume(tok::kw_PROCEDURE, "error msg");
    ExpectAndConsume(tok::identifier, "error msg");
    if (Tok.getKind() == tok::l_paren) {
        ParseFormalParameters();
    }
}

void Parser::ParseBlock() {
    while (Tok.isOneOf(tok::kw_CONST, tok::kw_MODULE, tok::kw_PROCEDURE, tok::kw_TYPE, tok::kw_VAR)) {
        ParseDeclaration();
    }
    if (Tok.getKind() == tok::kw_BEGIN) {
        ConsumeToken();
        ParseStatementSequence();
    }
    ExpectAndConsume(tok::kw_END, "error msg");
}

void Parser::ParseDeclaration() {
    if (Tok.getKind() == tok::kw_CONST) {
        ConsumeToken();
        while (Tok.getKind() == tok::identifier) {
            ParseConstantDeclaration();
            ExpectAndConsume(tok::semi, "error msg");
        }
    }
    else if (Tok.getKind() == tok::kw_TYPE) {
        ConsumeToken();
        while (Tok.getKind() == tok::identifier) {
            ParseTypeDeclaration();
            ExpectAndConsume(tok::semi, "error msg");
        }
    }
    else if (Tok.getKind() == tok::kw_VAR) {
        ConsumeToken();
        while (Tok.getKind() == tok::identifier) {
            ParseVariableDeclaration();
            ExpectAndConsume(tok::semi, "error msg");
        }
    }
    else if (Tok.getKind() == tok::kw_PROCEDURE) {
        ParseProcedureDeclaration();
        ExpectAndConsume(tok::semi, "error msg");
    }
    else if (Tok.getKind() == tok::kw_MODULE) {
        ParseModuleDeclaration();
        ExpectAndConsume(tok::semi, "error msg");
    }
}

void Parser::ParseFormalParameters() {
    ExpectAndConsume(tok::l_paren, "error msg");
    if (Tok.isOneOf(tok::kw_VAR, tok::identifier)) {
        ParseFPSection();
        while (Tok.getKind() == tok::semi) {
            ConsumeToken();
            ParseFPSection();
        }
    }
    ExpectAndConsume(tok::r_paren, "error msg");
    if (Tok.getKind() == tok::colon) {
        ConsumeToken();
        ParseQualident();
    }
}

void Parser::ParseFPSection() {
    if (Tok.getKind() == tok::kw_VAR) {
        ConsumeToken();
    }
    ParseIdentList();
    ExpectAndConsume(tok::colon, "error msg");
    ParseFormalType();
}

void Parser::ParseFormalType() {
    if (Tok.getKind() == tok::kw_ARRAY) {
        ConsumeToken();
        ExpectAndConsume(tok::kw_OF, "error msg");
    }
    ParseQualident();
}

void Parser::ParseModuleDeclaration() {
    ExpectAndConsume(tok::kw_MODULE, "error msg");
    ExpectAndConsume(tok::identifier, "error msg");
    if (Tok.getKind() == tok::l_square) {
        ParsePriority();
    }
    ExpectAndConsume(tok::semi, "error msg");
    while (Tok.isOneOf(tok::kw_FROM, tok::kw_IMPORT)) {
        ParseImport();
    }
    if (Tok.getKind() == tok::kw_EXPORT) {
        ParseExport();
    }
    ParseBlock();
    ExpectAndConsume(tok::identifier, "error msg");
}

void Parser::ParsePriority() {
    ExpectAndConsume(tok::l_square, "error msg");
    ParseConstExpression();
    ExpectAndConsume(tok::r_square, "error msg");
}

void Parser::ParseExport() {
    ExpectAndConsume(tok::kw_EXPORT, "error msg");
    if (Tok.getKind() == tok::kw_QUALIFIED) {
        ConsumeToken();
    }
    ParseIdentList();
    ExpectAndConsume(tok::semi, "error msg");
}

void Parser::ParseImport() {
    if (Tok.getKind() == tok::kw_FROM) {
        ConsumeToken();
        ExpectAndConsume(tok::identifier, "error msg");
    }
    ExpectAndConsume(tok::kw_IMPORT, "error msg");
    ParseIdentList();
    ExpectAndConsume(tok::semi, "error msg");
}

void Parser::ParseDefinitionModule() {
    ExpectAndConsume(tok::kw_DEFINITION, "error msg");
    ExpectAndConsume(tok::kw_MODULE, "error msg");
    ExpectAndConsume(tok::identifier, "error msg");
    ExpectAndConsume(tok::semi, "error msg");
    while (Tok.isOneOf(tok::kw_FROM, tok::kw_IMPORT)) {
        ParseImport();
    }
    while (Tok.isOneOf(tok::kw_CONST, tok::kw_PROCEDURE, tok::kw_TYPE, tok::kw_VAR)) {
        ParseDefinition();
    }
    ExpectAndConsume(tok::kw_END, "error msg");
    ExpectAndConsume(tok::identifier, "error msg");
    ExpectAndConsume(tok::period, "error msg");
}

void Parser::ParseDefinition() {
    if (Tok.getKind() == tok::kw_CONST) {
        ConsumeToken();
        while (Tok.getKind() == tok::identifier) {
            ParseConstantDeclaration();
            ExpectAndConsume(tok::semi, "error msg");
        }
    }
    else if (Tok.getKind() == tok::kw_TYPE) {
        ConsumeToken();
        while (Tok.getKind() == tok::identifier) {
            ConsumeToken();
            if (Tok.getKind() == tok::equal) {
                ConsumeToken();
                ParseType();
            }
            ExpectAndConsume(tok::semi, "error msg");
        }
    }
    else if (Tok.getKind() == tok::kw_VAR) {
        ConsumeToken();
        while (Tok.getKind() == tok::identifier) {
            ParseVariableDeclaration();
            ExpectAndConsume(tok::semi, "error msg");
        }
    }
    else if (Tok.getKind() == tok::kw_PROCEDURE) {
        ParseProcedureHeading();
        ExpectAndConsume(tok::semi, "error msg");
    }
}

void Parser::ParseProgramModule() {
    ExpectAndConsume(tok::kw_MODULE, "error msg");
    ExpectAndConsume(tok::identifier, "error msg");
    if (Tok.getKind() == tok::l_square) {
        ParsePriority();
    }
    ExpectAndConsume(tok::semi, "error msg");
    while (Tok.isOneOf(tok::kw_FROM, tok::kw_IMPORT)) {
        ParseImport();
    }
    ParseBlock();
    ExpectAndConsume(tok::identifier, "error msg");
    ExpectAndConsume(tok::period, "error msg");
}

void Parser::ParseCompilationUnit() {
    if (Tok.getKind() == tok::kw_DEFINITION) {
        ParseDefinitionModule();
    }
    else if (Tok.isOneOf(tok::kw_IMPLEMENTATION, tok::kw_MODULE)) {
        if (Tok.getKind() == tok::kw_IMPLEMENTATION) {
            ConsumeToken();
        }
        ParseProgramModule();
    }
}