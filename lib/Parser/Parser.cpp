//===--- parser.cpp - Modula-2 Language parser ------------------*- C++ -*-===//
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

Parser::Parser(Lexer &Lex, Sema &Actions) : Lex(Lex), Actions(Actions) {
  llvm::outs() << "File:\n" << Lex.getBuffer() << "\n----\n";
  nextToken();
}

void Parser::initialize() {}

void Parser::parseNumber() {
    if (Tok.getKind() == tok::integer_literal) {
        consumeToken();
    }
    else if (Tok.getKind() == tok::real_literal) {
        consumeToken();
    }
}

void Parser::parseString() {
    if (Tok.getKind() == tok::string_literal) {
        consumeToken();
    }
    else if (Tok.getKind() == tok::char_literal) {
        consumeToken();
    }
}

void Parser::parseQualident() {
    expectAndConsume(tok::identifier);
     /* Parsing the sequence of identifiers is an LL(1) conflict with parsing
      * the selectors as part of a  designator. This sequence can be longer as
      * intended. The semantic phase check and corrects this.
      */
    while (Tok.getKind() == tok::period) {
        consumeToken();
        expectAndConsume(tok::identifier);
    }
}

void Parser::parseConstantDeclaration() {
  SourceLocation Loc;
  StringRef Name;
  if (Tok.is(tok::identifier)) {
    Loc = Tok.getLocation();
    Name = getIdentifier(Tok);
    consumeToken();
  } else {
    // TODO Emit error message
    skipUntil(tok::eof, SkipUntilFlags::StopAtSemi);
    return;
  }
  expectAndConsume(tok::equal);
  parseConstExpression();
  Actions.actOnConstantDecl(Loc, Name, nullptr);
}

void Parser::parseConstExpression() {
    parseExpression();
}

void Parser::parseTypeDeclaration() {
  SourceLocation Loc;
  StringRef Name;
  if (Tok.is(tok::identifier)) {
    Loc = Tok.getLocation();
    Name = getIdentifier(Tok);
    consumeToken();
  } else {
    // TODO Emit error message
    skipUntil(tok::eof, SkipUntilFlags::StopAtSemi);
    return ;
  }
  expectAndConsume(tok::equal);
  parseType();
  Actions.actOnTypeDecl(Loc, Name, nullptr);
}

void Parser::parseType() {
    if (Tok.isOneOf(tok::l_paren, tok::l_square, tok::identifier)) {
        parseSimpleType();
    }
    else if (Tok.getKind() == tok::kw_ARRAY) {
        parseArrayType();
    }
    else if (Tok.getKind() == tok::kw_RECORD) {
        parseRecordType();
    }
    else if (Tok.getKind() == tok::kw_SET) {
        parseSetType();
    }
    else if (Tok.getKind() == tok::kw_POINTER) {
        parsePointerType();
    }
    else if (Tok.getKind() == tok::kw_PROCEDURE) {
        parseProcedureType();
    }
}

void Parser::parseSimpleType() {
    if (Tok.getKind() == tok::identifier /* Unresolved LL(1) conflict */) {
        parseQualident();
    }
    else if (Tok.getKind() == tok::l_paren) {
        parseEnumeration();
    }
    else if (Tok.isOneOf(tok::l_square, tok::identifier)) {
        parseSubrangeType();
    }
}

void Parser::parseEnumeration() {
    expectAndConsume(tok::l_paren);
    parseIdentList();
    expectAndConsume(tok::r_paren);
}

void Parser::parseIdentList() {
    expectAndConsume(tok::identifier);
    while (Tok.getKind() == tok::comma) {
        consumeToken();
        expectAndConsume(tok::identifier);
    }
}

void Parser::parseSubrangeType() {
    if (Tok.getKind() == tok::identifier) {
        consumeToken();
    }
    expectAndConsume(tok::l_square);
    parseConstExpression();
    expectAndConsume(tok::ellipsis);
    parseConstExpression();
    expectAndConsume(tok::r_square);
}

void Parser::parseArrayType() {
    expectAndConsume(tok::kw_ARRAY);
    parseSimpleType();
    while (Tok.getKind() == tok::comma) {
        consumeToken();
        parseSimpleType();
    }
    expectAndConsume(tok::kw_OF);
    parseType();
}

void Parser::parseRecordType() {
    expectAndConsume(tok::kw_RECORD);
    parseFieldListSequence();
    expectAndConsume(tok::kw_END);
}

void Parser::parseFieldListSequence() {
    parseFieldList();
    while (Tok.getKind() == tok::semi) {
        consumeToken();
        parseFieldList();
    }
}

void Parser::parseFieldList() {
    if (Tok.isOneOf(tok::kw_CASE, tok::identifier)) {
        if (Tok.getKind() == tok::identifier) {
            parseIdentList();
            expectAndConsume(tok::colon);
            parseType();
        }
        else if (Tok.getKind() == tok::kw_CASE) {
            consumeToken();
            if (Tok.getKind() == tok::identifier) {
                consumeToken();
            }
            expectAndConsume(tok::colon);
            parseQualident();
            expectAndConsume(tok::kw_OF);
            parseVariant();
            while (Tok.getKind() == tok::pipe) {
                consumeToken();
                parseVariant();
            }
            if (Tok.getKind() == tok::kw_ELSE) {
                consumeToken();
                parseFieldListSequence();
            }
            expectAndConsume(tok::kw_END);
        }
    }
}

void Parser::parseVariant() {
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::l_brace, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
        parseCaseLabelList();
        expectAndConsume(tok::colon);
        parseFieldListSequence();
    }
}

void Parser::parseCaseLabelList() {
    parseCaseLabels();
    while (Tok.getKind() == tok::comma) {
        consumeToken();
        parseCaseLabels();
    }
}

void Parser::parseCaseLabels() {
    parseConstExpression();
    if (Tok.getKind() == tok::ellipsis) {
        consumeToken();
        parseConstExpression();
    }
}

void Parser::parseSetType() {
    expectAndConsume(tok::kw_SET);
    expectAndConsume(tok::kw_OF);
    parseSimpleType();
}

void Parser::parsePointerType() {
    expectAndConsume(tok::kw_POINTER);
    expectAndConsume(tok::kw_TO);
    parseType();
}

void Parser::parseProcedureType() {
    expectAndConsume(tok::kw_PROCEDURE);
    if (Tok.getKind() == tok::l_paren) {
        parseFormalTypeList();
    }
}

void Parser::parseFormalTypeList() {
    expectAndConsume(tok::l_paren);
    if (Tok.isOneOf(tok::kw_ARRAY, tok::kw_VAR, tok::identifier)) {
        if (Tok.getKind() == tok::kw_VAR) {
            consumeToken();
        }
        parseFormalType();
        while (Tok.getKind() == tok::comma) {
            consumeToken();
            if (Tok.getKind() == tok::kw_VAR) {
                consumeToken();
            }
            parseFormalType();
        }
    }
    expectAndConsume(tok::r_paren);
    if (Tok.getKind() == tok::colon) {
        consumeToken();
        parseQualident();
    }
}

void Parser::parseVariableDeclaration() {
    parseIdentList();
    expectAndConsume(tok::colon);
    parseType();
    Actions.actOnVariableDecl(42, "DemoVar", nullptr);
}

void Parser::parseDesignator() {
    parseQualident();
    while (Tok.isOneOf(tok::period, tok::l_square, tok::caret)) {
        Parser::parseSelector();
    }
}

void Parser::parseSelector() {
    if (Tok.getKind() == tok::period) {
        consumeToken();
        expectAndConsume(tok::identifier);
    }
    else if (Tok.getKind() == tok::l_square) {
        consumeToken();
        parseExpList();
        expectAndConsume(tok::r_square);
    }
    else if (Tok.getKind() == tok::caret) {
        consumeToken();
    }
}

void Parser::parseExpList() {
    parseExpression();
    while (Tok.getKind() == tok::comma) {
        consumeToken();
        parseExpression();
    }
}

void Parser::parseExpression() {
    parseSimpleExpression();
    if (Tok.isOneOf(tok::hash, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_IN)) {
        parseRelation();
        parseSimpleExpression();
    }
}

void Parser::parseRelation() {
    if (Tok.getKind() == tok::equal) {
        consumeToken();
    }
    else if (Tok.getKind() == tok::hash) {
        consumeToken();
    }
    else if (Tok.getKind() == tok::less) {
        consumeToken();
    }
    else if (Tok.getKind() == tok::lessequal) {
        consumeToken();
    }
    else if (Tok.getKind() == tok::greater) {
        consumeToken();
    }
    else if (Tok.getKind() == tok::greaterequal) {
        consumeToken();
    }
    else if (Tok.getKind() == tok::kw_IN) {
        consumeToken();
    }
}

void Parser::parseSimpleExpression() {
    if (Tok.isOneOf(tok::plus, tok::minus)) {
        if (Tok.getKind() == tok::plus) {
            consumeToken();
        }
        else if (Tok.getKind() == tok::minus) {
            consumeToken();
        }
    }
    parseTerm();
    while (Tok.isOneOf(tok::plus, tok::minus, tok::kw_OR)) {
        parseAddOperator();
        parseTerm();
    }
}

void Parser::parseAddOperator() {
    if (Tok.getKind() == tok::plus) {
        consumeToken();
    }
    else if (Tok.getKind() == tok::minus) {
        consumeToken();
    }
    else if (Tok.getKind() == tok::kw_OR) {
        consumeToken();
    }
}

void Parser::parseTerm() {
    parseFactor();
    while (Tok.isOneOf(tok::star, tok::slash, tok::kw_AND, tok::kw_DIV, tok::kw_MOD)) {
        parseMulOperator();
        parseFactor();
    }
}

void Parser::parseMulOperator() {
    if (Tok.getKind() == tok::star) {
        consumeToken();
    }
    else if (Tok.getKind() == tok::slash) {
        consumeToken();
    }
    else if (Tok.getKind() == tok::kw_DIV) {
        consumeToken();
    }
    else if (Tok.getKind() == tok::kw_MOD) {
        consumeToken();
    }
    else if (Tok.getKind() == tok::kw_AND) {
        consumeToken();
    }
}

void Parser::parseFactor() {
    if (Tok.isOneOf(tok::integer_literal, tok::real_literal)) {
        parseNumber();
    }
    else if (Tok.isOneOf(tok::char_literal, tok::string_literal)) {
        parseString();
    }
    else if (Tok.getKind() == tok::identifier) {
        /* Resolution for the LL(1) between set and procedure constant / call.
         * The rule to parse the designator is split and the decision between
         * set and procedure is delayed after parsing the qualified identifier.
         * PIM also allows a set value constructor without a type name. This
         * is handled below separately.
         */
        parseQualident();
        if (Tok.getKind() == tok::l_brace) {
            parseSetValues();
        }
        else if (Tok.isOneOf(tok::hash, tok::l_paren, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::period, tok::ellipsis, tok::slash, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::l_square, tok::r_square, tok::caret, tok::pipe, tok::r_brace)) {
            /* Parsing the selector to form a designator. */
            while (Tok.isOneOf(tok::period, tok::l_square, tok::caret)) {
                Parser::parseSelector();
            }
            if (Tok.getKind() == tok::l_paren) {
                parseActualParameters();
            }
        }
    }
    else if (getLangOpts().PIM && Tok.getKind() == tok::l_brace) {
        parseSetValues();
    }
    else if (Tok.getKind() == tok::l_paren) {
        consumeToken();
        parseExpression();
        expectAndConsume(tok::r_paren);
    }
    else if (Tok.getKind() == tok::kw_NOT) {
        consumeToken();
        parseFactor();
    }
}

void Parser::parseSetValues() {
    if (Tok.getKind() == tok::identifier) {
        parseQualident();
    }
    expectAndConsume(tok::l_brace);
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::l_brace, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
        parseElement();
        while (Tok.getKind() == tok::comma) {
            consumeToken();
            parseElement();
        }
    }
    expectAndConsume(tok::r_brace);
}

void Parser::parseElement() {
    parseExpression();
    if (Tok.getKind() == tok::ellipsis) {
        consumeToken();
        parseExpression();
    }
}

void Parser::parseActualParameters() {
    expectAndConsume(tok::l_paren);
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::l_brace, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
        parseExpList();
    }
    expectAndConsume(tok::r_paren);
}

void Parser::parseStatement() {
    if (Tok.isOneOf(tok::kw_CASE, tok::kw_EXIT, tok::kw_FOR, tok::kw_IF, tok::kw_LOOP, tok::kw_REPEAT, tok::kw_RETURN, tok::kw_WHILE, tok::kw_WITH, tok::identifier)) {
        if (Tok.getKind() == tok::identifier) {
             /* Resolved LL(1) conflict by delaying decision after parsing designator */
            parseDesignator();
            if (Tok.getKind() == tok::colonequal) {
                /* Assignment */
                consumeToken();
                parseExpression();
            }
            else if (Tok.isOneOf(tok::l_paren, tok::semi, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_UNTIL, tok::pipe)) {
                /* Procedure call */
                if (Tok.getKind() == tok::l_paren) {
                    parseActualParameters();
                }
            }
        }
        else if (Tok.getKind() == tok::kw_IF) {
            parseIfStatement();
        }
        else if (Tok.getKind() == tok::kw_CASE) {
            parseCaseStatement();
        }
        else if (Tok.getKind() == tok::kw_WHILE) {
            parseWhileStatement();
        }
        else if (Tok.getKind() == tok::kw_REPEAT) {
            parseRepeatStatement();
        }
        else if (Tok.getKind() == tok::kw_LOOP) {
            parseLoopStatement();
        }
        else if (Tok.getKind() == tok::kw_FOR) {
            parseForStatement();
        }
        else if (Tok.getKind() == tok::kw_WITH) {
            parseWithStatement();
        }
        else if (Tok.getKind() == tok::kw_EXIT) {
            consumeToken();
            Actions.actOnExitStmt();
        }
        else if (Tok.getKind() == tok::kw_RETURN) {
            consumeToken();
            if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::l_brace, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
                parseExpression();
            }
            Actions.actOnReturnStmt();
        }
    }
}

void Parser::parseStatementSequence() {
    parseStatement();
    while (Tok.getKind() == tok::semi) {
        consumeToken();
        parseStatement();
    }
}

void Parser::parseIfStatement() {
    expectAndConsume(tok::kw_IF);
    parseExpression();
    expectAndConsume(tok::kw_THEN);
    parseStatementSequence();
    while (Tok.getKind() == tok::kw_ELSIF) {
        consumeToken();
        parseExpression();
        expectAndConsume(tok::kw_THEN);
        parseStatementSequence();
    }
    if (Tok.getKind() == tok::kw_ELSE) {
        consumeToken();
        parseStatementSequence();
    }
    expectAndConsume(tok::kw_END);
    Actions.actOnIfStmt();
}

void Parser::parseCaseStatement() {
    expectAndConsume(tok::kw_CASE);
    parseExpression();
    expectAndConsume(tok::kw_OF);
    parseCase();
    while (Tok.getKind() == tok::pipe) {
        consumeToken();
        parseCase();
    }
    if (Tok.getKind() == tok::kw_ELSE) {
        consumeToken();
        parseStatementSequence();
    }
    expectAndConsume(tok::kw_END);
    Actions.actOnCaseStmt();
}

void Parser::parseCase() {
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::l_brace, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
        parseCaseLabelList();
        expectAndConsume(tok::colon);
        parseStatementSequence();
    }
}

void Parser::parseWhileStatement() {
    expectAndConsume(tok::kw_WHILE);
    parseExpression();
    expectAndConsume(tok::kw_DO);
    parseStatementSequence();
    expectAndConsume(tok::kw_END);
    Actions.actOnWhileStmt();
}

void Parser::parseRepeatStatement() {
    expectAndConsume(tok::kw_REPEAT);
    parseStatementSequence();
    expectAndConsume(tok::kw_UNTIL);
    parseExpression();
    Actions.actOnRepeatStmt();
}

void Parser::parseForStatement() {
    expectAndConsume(tok::kw_FOR);
    expectAndConsume(tok::identifier);
    expectAndConsume(tok::colonequal);
    parseExpression();
    expectAndConsume(tok::kw_TO);
    parseExpression();
    if (Tok.getKind() == tok::kw_BY) {
        consumeToken();
        parseConstExpression();
    }
    expectAndConsume(tok::kw_DO);
    parseStatementSequence();
    expectAndConsume(tok::kw_END);
    Actions.actOnForStmt();
}

void Parser::parseLoopStatement() {
    expectAndConsume(tok::kw_LOOP);
    parseStatementSequence();
    expectAndConsume(tok::kw_END);
    Actions.actOnLoopStmt();
}

void Parser::parseWithStatement() {
    expectAndConsume(tok::kw_WITH);
    parseDesignator();
    expectAndConsume(tok::kw_DO);
    parseStatementSequence();
    expectAndConsume(tok::kw_END);
    Actions.actOnWithStmt();
}

void Parser::parseProcedureDeclaration() {
    parseProcedureHeading();
    expectAndConsume(tok::semi);
    parseBlock();
    expectAndConsume(tok::identifier);
    Actions.actOnProcedureDecl();
}

void Parser::parseProcedureHeading() {
    expectAndConsume(tok::kw_PROCEDURE);
    expectAndConsume(tok::identifier);
    if (Tok.getKind() == tok::l_paren) {
        parseFormalParameters();
    }
}

void Parser::parseBlock() {
    while (Tok.isOneOf(tok::kw_CONST, tok::kw_MODULE, tok::kw_PROCEDURE, tok::kw_TYPE, tok::kw_VAR)) {
        parseDeclaration();
    }
    if (Tok.getKind() == tok::kw_BEGIN) {
        consumeToken();
        parseStatementSequence();
    }
    expectAndConsume(tok::kw_END);
}

void Parser::parseDeclaration() {
    if (Tok.getKind() == tok::kw_CONST) {
        consumeToken();
        while (Tok.getKind() == tok::identifier) {
            parseConstantDeclaration();
            expectAndConsume(tok::semi);
        }
    }
    else if (Tok.getKind() == tok::kw_TYPE) {
        consumeToken();
        while (Tok.getKind() == tok::identifier) {
            parseTypeDeclaration();
            expectAndConsume(tok::semi);
        }
    }
    else if (Tok.getKind() == tok::kw_VAR) {
        consumeToken();
        while (Tok.getKind() == tok::identifier) {
            parseVariableDeclaration();
            expectAndConsume(tok::semi);
        }
    }
    else if (Tok.getKind() == tok::kw_PROCEDURE) {
        parseProcedureDeclaration();
        expectAndConsume(tok::semi);
    }
    else if (Tok.getKind() == tok::kw_MODULE) {
        parseModuleDeclaration();
        expectAndConsume(tok::semi);
    }
}

void Parser::parseFormalParameters() {
    expectAndConsume(tok::l_paren);
    if (Tok.isOneOf(tok::kw_VAR, tok::identifier)) {
        parseFPSection();
        while (Tok.getKind() == tok::semi) {
            consumeToken();
            parseFPSection();
        }
    }
    expectAndConsume(tok::r_paren);
    if (Tok.getKind() == tok::colon) {
        consumeToken();
        parseQualident();
    }
}

void Parser::parseFPSection() {
    if (Tok.getKind() == tok::kw_VAR) {
        consumeToken();
    }
    parseIdentList();
    expectAndConsume(tok::colon);
    parseFormalType();
}

void Parser::parseFormalType() {
    if (Tok.getKind() == tok::kw_ARRAY) {
        consumeToken();
        expectAndConsume(tok::kw_OF);
    }
    parseQualident();
}

void Parser::parseModuleDeclaration() {
    expectAndConsume(tok::kw_MODULE);
    expectAndConsume(tok::identifier);
    if (Tok.getKind() == tok::l_square) {
        parsePriority();
    }
    expectAndConsume(tok::semi);
    while (Tok.isOneOf(tok::kw_FROM, tok::kw_IMPORT)) {
        parseImport();
    }
    if (Tok.getKind() == tok::kw_EXPORT) {
        parseExport();
    }
    parseBlock();
    expectAndConsume(tok::identifier);
    Actions.actOnModuleDecl();
}

void Parser::parsePriority() {
    expectAndConsume(tok::l_square);
    parseConstExpression();
    expectAndConsume(tok::r_square);
}

void Parser::parseExport() {
    expectAndConsume(tok::kw_EXPORT);
    if (Tok.getKind() == tok::kw_QUALIFIED) {
        consumeToken();
    }
    parseIdentList();
    expectAndConsume(tok::semi);
}

void Parser::parseImport() {
    if (Tok.getKind() == tok::kw_FROM) {
        consumeToken();
        expectAndConsume(tok::identifier);
    }
    expectAndConsume(tok::kw_IMPORT);
    parseIdentList();
    expectAndConsume(tok::semi);
}

void Parser::parseActualParameter() {
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::l_brace, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
        parseConstExpression();
    }
    else {
        // type parameter
        // parseTypeParameter();
    }
}

void Parser::parseActualModuleParameters() {
    expectAndConsume(tok::l_paren);
    parseActualParameter();
    while (Tok.getKind() == tok::comma) {
        consumeToken();
        parseActualParameter();
    }
    expectAndConsume(tok::r_paren);
}

void Parser::parseFormalModuleParameter() {
    parseIdentList();
    expectAndConsume(tok::colon);
    if (Tok.getKind() == tok::kw_TYPE) {
        consumeToken();
    }
    else if (Tok.isOneOf(tok::kw_ARRAY, tok::identifier)) {
        parseFormalType();
    }
}

void Parser::parseFormalModuleParameters() {
    expectAndConsume(tok::l_paren);
    parseFormalModuleParameter();
    while (Tok.getKind() == tok::semi) {
        consumeToken();
        parseFormalModuleParameter();
    }
    expectAndConsume(tok::r_paren);
}

void Parser::parseDefinitionModule(bool IsGenericModule) {
    expectAndConsume(tok::kw_DEFINITION);
    expectAndConsume(tok::kw_MODULE);
    expectAndConsume(tok::identifier);
    if (getLangOpts().ISOGenerics && !IsGenericModule && Tok.getKind() == tok::equal) {
        consumeToken();
        expectAndConsume(tok::identifier);
        if (Tok.getKind() == tok::l_paren) {
            parseActualModuleParameters();
        }
        expectAndConsume(tok::semi);
    }
    else if (Tok.isOneOf(tok::l_paren, tok::semi)) {
        if (IsGenericModule && Tok.getKind() == tok::l_paren) {
            parseFormalModuleParameters();
        }
        expectAndConsume(tok::semi);
        while (Tok.isOneOf(tok::kw_FROM, tok::kw_IMPORT)) {
            parseImport();
        }
        while (Tok.isOneOf(tok::kw_CONST, tok::kw_PROCEDURE, tok::kw_TYPE, tok::kw_VAR)) {
            parseDefinition();
        }
    }
    expectAndConsume(tok::kw_END);
    expectAndConsume(tok::identifier);
    expectAndConsume(tok::period);
}

void Parser::parseDefinition() {
    if (Tok.getKind() == tok::kw_CONST) {
        consumeToken();
        while (Tok.getKind() == tok::identifier) {
            parseConstantDeclaration();
            expectAndConsume(tok::semi);
        }
    }
    else if (Tok.getKind() == tok::kw_TYPE) {
        consumeToken();
        while (Tok.getKind() == tok::identifier) {
            consumeToken();
            if (Tok.getKind() == tok::equal) {
                consumeToken();
                parseType();
            }
            expectAndConsume(tok::semi);
        }
    }
    else if (Tok.getKind() == tok::kw_VAR) {
        consumeToken();
        while (Tok.getKind() == tok::identifier) {
            parseVariableDeclaration();
            expectAndConsume(tok::semi);
        }
    }
    else if (Tok.getKind() == tok::kw_PROCEDURE) {
        parseProcedureHeading();
        expectAndConsume(tok::semi);
    }
}

void Parser::parseProgramModule(bool IsImplModule, bool IsGenericModule) {
    expectAndConsume(tok::kw_MODULE);
    expectAndConsume(tok::identifier);
    if (getLangOpts().ISOGenerics && !IsGenericModule && Tok.getKind() == tok::equal) {
        consumeToken();
        expectAndConsume(tok::identifier);
        if (Tok.getKind() == tok::l_paren) {
            parseActualModuleParameters();
        }
        expectAndConsume(tok::semi);
        expectAndConsume(tok::kw_END);
    }
    else if (Tok.isOneOf(tok::l_square, tok::semi) || (IsGenericModule && Tok.getKind() == tok::l_paren)) {
        if (Tok.getKind() == tok::l_square) {
            parsePriority();
        }
        if (IsGenericModule && Tok.getKind() == tok::l_paren) {
            parseFormalModuleParameters();
        }
        expectAndConsume(tok::semi);
        while (Tok.isOneOf(tok::kw_FROM, tok::kw_IMPORT)) {
            parseImport();
        }
        parseBlock();
    }
    expectAndConsume(tok::identifier);
    expectAndConsume(tok::period);
}

void Parser::parseCompilationUnit() {
    bool IsImplModule = false;
    bool IsGenericModule = false;
    if (getLangOpts().ISOGenerics && Tok.getKind() == tok::kw_GENERIC) {
        consumeToken();
        IsGenericModule = true;
    }
    if (Tok.getKind() == tok::kw_DEFINITION) {
        parseDefinitionModule(IsGenericModule);
    }
    else if (Tok.isOneOf(tok::kw_IMPLEMENTATION, tok::kw_MODULE)) {
        if (Tok.getKind() == tok::kw_IMPLEMENTATION) {
            consumeToken();
            IsImplModule = true;
        }
        else if (IsGenericModule) {
            // There are no generic program modules.
            // TODO Emit error
            IsImplModule = true;
        }
        parseProgramModule(IsImplModule, IsGenericModule);
    }
}