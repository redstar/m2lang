#ifdef M2PARSER_DECLARATION
void parseCompilationModule();
void parseProgramModule(bool HasUnsafeGuarded);
void parseModuleIdentifier();
void parseProtection();
void parseProtectionExpression();
void parseDefinitionModule(bool HasUnsafeGuarded);
void parseImplementationModule(bool HasUnsafeGuarded);
void parseImportLists();
void parseImportList();
void parseSimpleImport();
void parseUnqualifiedImport();
void parseExportList();
void parseQualifiedIdentifier();
void parseGenericDefinitionModule();
void parseGenericImplementationModule();
void parseGenericSeparateModuleIdentifier();
void parseFormalModuleParameters();
void parseFormalModuleParameterList();
void parseFormalModuleParameter();
void parseConstantValueParameterSpecification();
void parseTypeParameterSpecification();
void parseActualModuleParameters();
void parseActualModuleParameterList();
void parseActualModuleParameter();
void parseDefinitions();
void parseProcedureHeading();
void parseTypeDefinition();
void parseOpaqueTypeDefinition();
void parseProperProcedureHeading();
void parseFormalParameters();
void parseFormalParameterList();
void parseFunctionProcedureHeading();
void parseFunctionResultType();
void parseFormalParameter();
void parseValueParameterSpecification();
void parseVariableParameterSpecification();
void parseDeclarations();
void parseConstantDeclaration(DeclList &Decls);
void parseTypeDeclaration();
void parseVariableDeclaration();
void parseVariableIdentifierList();
void parseMachineAddress();
void parseValueOfAddressType();
void parseProcedureDeclaration();
void parseProcedureIdentifier();
void parseLocalModuleDeclaration();
void parseTypeDenoter();
void parseOrdinalTypeDenoter();
void parseTypeIdentifier();
void parseOrdinalTypeIdentifier();
void parseNewType();
void parseNewOrdinalType();
void parseEnumerationType();
void parseIdentifierList();
void parseSubrangeType();
void parseRangeType();
void parseSetType();
void parseBaseType();
void parsePackedsetType();
void parsePointerType();
void parseBoundType();
void parseProcedureType();
void parseFormalParameterTypeList();
void parseFormalParameterType();
void parseVariableFormalType();
void parseValueFormalType();
void parseFormalType();
void parseOpenArrayFormalType();
void parseArrayType();
void parseIndexType();
void parseComponentType();
void parseRecordType();
void parseFieldList();
void parseFields();
void parseFixedFields();
void parseFieldType();
void parseVariantFields();
void parseTagIdentifier();
void parseTagType();
void parseVariantList();
void parseVariantElsePart();
void parseVariant();
void parseVariantLabelList();
void parseVariantLabel();
void parseProperProcedureBlock(bool IsFunction);
void parseModuleBlock();
void parseModuleBody();
void parseInitializationBody();
void parseFinalizationBody();
void parseBlockBody();
void parseNormalPart(StmtList &Stmts);
void parseExceptionalPart(StmtList &Stmts);
void parseStatement(Stmt *&S);
void parseStatementSequence(StmtList &Stmts);
void parseAssignmentStatement(Stmt *&S);
void parseProcedureCall(Stmt *&S);
void parseProcedureDesignator();
void parseReturnStatement(Stmt *&S);
void parseRetryStatement(Stmt *&S);
void parseWithStatement(Stmt *&S);
void parseRecordDesignator();
void parseIfStatement(Stmt *&S);
void parseGuardedStatements();
void parseIfElsePart();
void parseBooleanExpression();
void parseCaseStatement(Stmt *&S);
void parseCaseSelector();
void parseCaseList();
void parseCaseElsePart();
void parseCaseAlternative();
void parseCaseLabelList();
void parseCaseLabel();
void parseWhileStatement(Stmt *&S);
void parseRepeatStatement(Stmt *&S);
void parseLoopStatement(Stmt *&S);
void parseExitStatement(Stmt *&S);
void parseForStatement(Stmt *&S);
void parseControlVariableIdentifier();
void parseInitialValue();
void parseFinalValue();
void parseStepSize();
void parseVariableDesignator();
void parseEntireDesignator();
void parseIndexedDesignator();
void parseArrayVariableDesignator();
void parseIndexExpression();
void parseSelectedDesignator();
void parseRecordVariableDesignator();
void parseFieldIdentifier();
void parseDereferencedDesignator();
void parsePointerVariableDesignator();
void parseExpression(Expr *&E);
void parseSimpleExpression(Expr *&E);
void parseTerm(Expr *&E);
void parseFactor(Expr *&E);
void parseOrdinalExpression();
void parseRelationalOperator(OperatorInfo &Op);
void parseTermOperator(OperatorInfo &Op);
void parseFactorOperator(OperatorInfo &Op);
void parseValueDesignator();
void parseEntireValue();
void parseIndexedValue();
void parseArrayValue();
void parseSelectedValue();
void parseRecordValue();
void parseDereferencedValue();
void parsePointerValue();
void parseFunctionCall();
void parseFunctionDesignator();
void parseValueConstructor();
void parseArrayConstructor();
void parseArrayTypeIdentifier();
void parseArrayConstructedValue();
void parseRepeatedStructureComponent();
void parseRepetitionFactor();
void parseStructureComponent();
void parseRecordConstructor();
void parseRecordTypeIdentifier();
void parseRecordConstructedValue();
void parseSetConstructor();
void parseSetTypeIdentifier();
void parseSetConstructedValue();
void parseMember();
void parseInterval();
void parseSingleton();
void parseConstantLiteral();
void parseStringLiteral();
void parseConstantExpression();
void parseActualParameters();
void parseActualParameterList();
void parseActualParameter();
void parseTypeParameter();
void parseClassDefinition();
void parseUntracedClassDefinition();
void parseTracedClassDefinition();
void parseNormalClassDefinition();
void parseNormalClassHeader();
void parseNormalClassDefinitionBody();
void parseAbstractClassDefinition();
void parseAbstractClassHeader();
void parseAbstractClassDefinitionBody();
void parseClassIdentifier();
void parseNormalClassComponentDefinitions();
void parseNormalComponentDefinition();
void parseAbstractClassComponentDefinitions();
void parseAbstractComponentDefinition();
void parseClassVariableDeclaration();
void parseNormalMethodDefinition();
void parseOverridingMethodDefinition();
void parseAbstractMethodDefinition();
void parseClassDeclaration();
void parseUntracedClassDeclaration();
void parseNormalClassDeclaration();
void parseNormalClassDeclarationBody();
void parseAbstractClassDeclaration();
void parseAbstractClassDeclarationBody();
void parseClassBody();
void parseNormalClassComponentDeclarations();
void parseNormalComponentDeclaration();
void parseAbstractClassComponentDeclarations();
void parseAbstractComponentDeclaration();
void parseNormalMethodDeclarations();
void parseNormalMethodDeclaration();
void parseOverridingMethodDeclaration();
void parseAbstractMethodDeclarations();
void parseTracedClassDeclaration();
void parseNormalTracedClassDeclaration();
void parseNormalTracedClassHeader();
void parseNormalTracedClassDeclarationBody();
void parseAbstractTracedClassDeclaration();
void parseAbstractTracedClassHeader();
void parseAbstractTracedClassDeclarationBody();
void parseTracedClassBody();
void parseRevealList();
void parseRevealedComponentList();
void parseRevealedComponent();
void parseClassVariableIdentifier();
void parseInheritClause();
void parseClassTypeIdentifier();
void parseObjectSelectedDesignator();
void parseObjectVariableDesignator();
void parseObjectSelectedValue();
void parseObjectValueDesignator();
void parseEntityIdentifier();
void parseGuardStatement(Stmt *&S);
void parseGuardSelector();
void parseGuardedList();
void parseGuardedStatementSequence();
void parseGuardedClassType();
void parseObjectDenoter();
#endif // of M2PARSER_DECLARATION
#ifdef M2PARSER_DEFINITION
void M2Parser::parseCompilationModule() {
  {
    if (Tok.is(tok::kw_UNSAFEGUARDED)) {
      advance();
      if (Tok.is(tok::kw_MODULE)) {
        parseProgramModule(true);
      }
      else if (Tok.is(tok::kw_DEFINITION)) {
        parseDefinitionModule(true);
      }
      else if (Tok.is(tok::kw_IMPLEMENTATION)) {
        parseImplementationModule(true);
      }
      else {
        /*ERROR*/
        goto _error;
      }
    }
    else if (Tok.is(tok::kw_GENERIC)) {
      advance();
      if (Tok.is(tok::kw_DEFINITION)) {
        parseGenericDefinitionModule();
      }
      else if (Tok.is(tok::kw_IMPLEMENTATION)) {
        parseGenericImplementationModule();
      }
      else {
        /*ERROR*/
        goto _error;
      }
    }
    else if (Tok.is(tok::kw_MODULE)) {
      parseProgramModule(false);
    }
    else if (Tok.is(tok::kw_DEFINITION)) {
      parseDefinitionModule(false);
    }
    else if (Tok.is(tok::kw_IMPLEMENTATION)) {
      parseImplementationModule(false);
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.is(tok::eof))
    advance();
}

void M2Parser::parseProgramModule(bool HasUnsafeGuarded) {
  {
    if (consume(tok::kw_MODULE))
      goto _error;
    parseModuleIdentifier();
    if (Tok.is(tok::l_square)) {
      parseProtection();
    }
    if (consume(tok::semi))
      goto _error;
    parseImportLists();
    parseModuleBlock();
    parseModuleIdentifier();
    if (consume(tok::period))
      goto _error;
    return;
  }
_error:
  while (!Tok.is(tok::eof))
    advance();
}

void M2Parser::parseModuleIdentifier() {
  {
    if (consume(tok::identifier))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::l_paren, tok::period, tok::semi, tok::equal, tok::kw_ABSTRACT, tok::kw_CLASS, tok::kw_CONST, tok::kw_END, tok::kw_FROM, tok::kw_IMPORT, tok::kw_PROCEDURE, tok::kw_TRACED, tok::kw_TYPE, tok::kw_VAR, tok::l_square, tok::eof))
    advance();
}

void M2Parser::parseProtection() {
  {
    if (consume(tok::l_square))
      goto _error;
    parseProtectionExpression();
    if (consume(tok::r_square))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::l_paren, tok::semi, tok::eof))
    advance();
}

void M2Parser::parseProtectionExpression() {
  {
    parseConstantExpression();
    return;
  }
}

void M2Parser::parseDefinitionModule(bool HasUnsafeGuarded) {
  {
    if (consume(tok::kw_DEFINITION))
      goto _error;
    if (consume(tok::kw_MODULE))
      goto _error;
    parseModuleIdentifier();
    if (Tok.is(tok::equal) && (!HasUnsafeGuarded && getLangOpts().ISOGenerics)) {
      advance();
      parseGenericSeparateModuleIdentifier();
      if (Tok.is(tok::l_paren)) {
        parseActualModuleParameters();
      }
      if (consume(tok::semi))
        goto _error;
    }
    else if (Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_CLASS, tok::kw_CONST, tok::kw_FROM, tok::kw_IMPORT, tok::kw_PROCEDURE, tok::kw_TRACED, tok::kw_TYPE, tok::kw_VAR)) {
      parseImportLists();
      parseDefinitions();
    }
    if (consume(tok::kw_END))
      goto _error;
    parseModuleIdentifier();
    if (consume(tok::period))
      goto _error;
    return;
  }
_error:
  while (!Tok.is(tok::eof))
    advance();
}

void M2Parser::parseImplementationModule(bool HasUnsafeGuarded) {
  {
    if (consume(tok::kw_IMPLEMENTATION))
      goto _error;
    if (consume(tok::kw_MODULE))
      goto _error;
    parseModuleIdentifier();
    if (Tok.is(tok::equal)) {
       if (!(!HasUnsafeGuarded && getLangOpts().ISOGenerics)) {
        /* ERROR */
        goto _error;
      }
      advance();
      parseGenericSeparateModuleIdentifier();
      if (Tok.is(tok::l_paren)) {
        parseActualModuleParameters();
      }
      if (consume(tok::semi))
        goto _error;
      if (consume(tok::kw_END))
        goto _error;
    }
    else if (Tok.isOneOf(tok::semi, tok::l_square)) {
      if (Tok.is(tok::l_square)) {
        parseProtection();
      }
      if (consume(tok::semi))
        goto _error;
      parseImportLists();
      parseModuleBlock();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    parseModuleIdentifier();
    if (consume(tok::period))
      goto _error;
    return;
  }
_error:
  while (!Tok.is(tok::eof))
    advance();
}

void M2Parser::parseImportLists() {
  {
    while (Tok.isOneOf(tok::kw_FROM, tok::kw_IMPORT)) {
      parseImportList();
    }
    return;
  }
}

void M2Parser::parseImportList() {
  {
    if (Tok.is(tok::kw_IMPORT)) {
      parseSimpleImport();
    }
    else if (Tok.is(tok::kw_FROM)) {
      parseUnqualifiedImport();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_BEGIN, tok::kw_CLASS, tok::kw_CONST, tok::kw_END, tok::kw_EXPORT, tok::kw_FROM, tok::kw_IMPORT, tok::kw_MODULE, tok::kw_PROCEDURE, tok::kw_TRACED, tok::kw_TYPE, tok::kw_VAR, tok::eof))
    advance();
}

void M2Parser::parseSimpleImport() {
  {
    if (consume(tok::kw_IMPORT))
      goto _error;
    parseIdentifierList();
    if (consume(tok::semi))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_BEGIN, tok::kw_CLASS, tok::kw_CONST, tok::kw_END, tok::kw_EXPORT, tok::kw_FROM, tok::kw_IMPORT, tok::kw_MODULE, tok::kw_PROCEDURE, tok::kw_TRACED, tok::kw_TYPE, tok::kw_VAR, tok::eof))
    advance();
}

void M2Parser::parseUnqualifiedImport() {
  {
    if (consume(tok::kw_FROM))
      goto _error;
    parseModuleIdentifier();
    if (consume(tok::kw_IMPORT))
      goto _error;
    parseIdentifierList();
    if (consume(tok::semi))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_BEGIN, tok::kw_CLASS, tok::kw_CONST, tok::kw_END, tok::kw_EXPORT, tok::kw_FROM, tok::kw_IMPORT, tok::kw_MODULE, tok::kw_PROCEDURE, tok::kw_TRACED, tok::kw_TYPE, tok::kw_VAR, tok::eof))
    advance();
}

void M2Parser::parseExportList() {
  {
    if (consume(tok::kw_EXPORT))
      goto _error;
    if (Tok.is(tok::kw_QUALIFIED)) {
      advance();
    }
    parseIdentifierList();
    if (consume(tok::semi))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_BEGIN, tok::kw_CLASS, tok::kw_CONST, tok::kw_END, tok::kw_MODULE, tok::kw_PROCEDURE, tok::kw_TRACED, tok::kw_TYPE, tok::kw_VAR, tok::eof))
    advance();
}

void M2Parser::parseQualifiedIdentifier() {
  {
    while (Tok.is(tok::identifier)) {
      parseModuleIdentifier();
      if (consume(tok::period))
        goto _error;
    }
    if (Tok.is(tok::identifier) && (getLangOpts().ISOObjects)) {
      parseClassIdentifier();
    }
    if (consume(tok::identifier))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::l_paren, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::period, tok::ellipsis, tok::slash, tok::colon, tok::colonequal, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::l_square, tok::r_square, tok::caret, tok::l_brace, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseGenericDefinitionModule() {
  {
    if (consume(tok::kw_DEFINITION))
      goto _error;
    if (consume(tok::kw_MODULE))
      goto _error;
    parseModuleIdentifier();
    if (Tok.is(tok::l_paren)) {
      parseFormalModuleParameters();
    }
    if (consume(tok::semi))
      goto _error;
    parseImportLists();
    parseDefinitions();
    if (consume(tok::kw_END))
      goto _error;
    parseModuleIdentifier();
    if (consume(tok::period))
      goto _error;
    return;
  }
_error:
  while (!Tok.is(tok::eof))
    advance();
}

void M2Parser::parseGenericImplementationModule() {
  {
    if (consume(tok::kw_IMPLEMENTATION))
      goto _error;
    if (consume(tok::kw_MODULE))
      goto _error;
    parseModuleIdentifier();
    if (Tok.is(tok::l_square)) {
      parseProtection();
    }
    if (Tok.is(tok::l_paren)) {
      parseFormalModuleParameters();
    }
    if (consume(tok::semi))
      goto _error;
    parseImportLists();
    parseModuleBlock();
    parseModuleIdentifier();
    if (consume(tok::period))
      goto _error;
    return;
  }
_error:
  while (!Tok.is(tok::eof))
    advance();
}

void M2Parser::parseGenericSeparateModuleIdentifier() {
  {
    if (consume(tok::identifier))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::l_paren, tok::semi, tok::eof))
    advance();
}

void M2Parser::parseFormalModuleParameters() {
  {
    if (consume(tok::l_paren))
      goto _error;
    parseFormalModuleParameterList();
    if (consume(tok::r_paren))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseFormalModuleParameterList() {
  {
    parseFormalModuleParameter();
    while (Tok.is(tok::semi)) {
      advance();
      parseFormalModuleParameter();
    }
    return;
  }
}

void M2Parser::parseFormalModuleParameter() {
  {
    if (Tok.is(tok::identifier)) {
      parseConstantValueParameterSpecification();
    }
    else if (Tok.is(tok::identifier)) {
      parseTypeParameterSpecification();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::semi, tok::eof))
    advance();
}

void M2Parser::parseConstantValueParameterSpecification() {
  {
    parseIdentifierList();
    if (consume(tok::colon))
      goto _error;
    parseFormalType();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::semi, tok::eof))
    advance();
}

void M2Parser::parseTypeParameterSpecification() {
  {
    parseIdentifierList();
    if (consume(tok::colon))
      goto _error;
    if (consume(tok::kw_TYPE))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::semi, tok::eof))
    advance();
}

void M2Parser::parseActualModuleParameters() {
  {
    if (consume(tok::l_paren))
      goto _error;
    parseActualModuleParameterList();
    if (consume(tok::r_paren))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseActualModuleParameterList() {
  {
    parseActualModuleParameter();
    while (Tok.is(tok::comma)) {
      advance();
      parseActualModuleParameter();
    }
    return;
  }
}

void M2Parser::parseActualModuleParameter() {
  {
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
      parseConstantExpression();
    }
    else if (Tok.is(tok::identifier)) {
      parseTypeParameter();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::comma, tok::eof))
    advance();
}

void M2Parser::parseDefinitions() {
  {
    DeclList Decls;
    while (Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_CLASS, tok::kw_CONST, tok::kw_PROCEDURE, tok::kw_TRACED, tok::kw_TYPE, tok::kw_VAR)) {
      if (Tok.is(tok::kw_CONST)) {
        advance();
        while (Tok.is(tok::identifier)) {
          parseConstantDeclaration(Decls);
          if (consume(tok::semi))
            goto _error;
        }
      }
      else if (Tok.is(tok::kw_TYPE)) {
        advance();
        while (Tok.is(tok::identifier)) {
          parseTypeDefinition();
          if (consume(tok::semi))
            goto _error;
        }
      }
      else if (Tok.is(tok::kw_VAR)) {
        advance();
        while (Tok.is(tok::identifier)) {
          parseVariableDeclaration();
          if (consume(tok::semi))
            goto _error;
        }
      }
      else if (Tok.is(tok::kw_PROCEDURE)) {
        parseProcedureHeading();
        if (consume(tok::semi))
          goto _error;
      }
      else if (Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_CLASS, tok::kw_TRACED) && (getLangOpts().ISOObjects)) {
        parseClassDefinition();
        if (consume(tok::semi))
          goto _error;
      }
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_END, tok::eof))
    advance();
}

void M2Parser::parseProcedureHeading() {
  {
    if (Tok.is(tok::kw_PROCEDURE)) {
      parseProperProcedureHeading();
    }
    else if (Tok.is(tok::kw_PROCEDURE)) {
      parseFunctionProcedureHeading();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseTypeDefinition() {
  {
    if (Tok.is(tok::identifier)) {
      parseTypeDeclaration();
    }
    else if (Tok.is(tok::identifier)) {
      parseOpaqueTypeDefinition();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseOpaqueTypeDefinition() {
  {
    if (consume(tok::identifier))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseProperProcedureHeading() {
  {
    if (consume(tok::kw_PROCEDURE))
      goto _error;
    parseProcedureIdentifier();
    if (Tok.is(tok::l_paren)) {
      parseFormalParameters();
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseFormalParameters() {
  {
    if (consume(tok::l_paren))
      goto _error;
    if (Tok.isOneOf(tok::kw_VAR, tok::identifier)) {
      parseFormalParameterList();
    }
    if (consume(tok::r_paren))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::colon, tok::semi, tok::eof))
    advance();
}

void M2Parser::parseFormalParameterList() {
  {
    parseFormalParameter();
    while (Tok.is(tok::semi)) {
      advance();
      parseFormalParameter();
    }
    return;
  }
}

void M2Parser::parseFunctionProcedureHeading() {
  {
    if (consume(tok::kw_PROCEDURE))
      goto _error;
    parseProcedureIdentifier();
    parseFormalParameters();
    if (consume(tok::colon))
      goto _error;
    parseFunctionResultType();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseFunctionResultType() {
  {
    parseTypeIdentifier();
    return;
  }
}

void M2Parser::parseFormalParameter() {
  {
    if (Tok.is(tok::identifier)) {
      parseValueParameterSpecification();
    }
    else if (Tok.is(tok::kw_VAR)) {
      parseVariableParameterSpecification();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::semi, tok::eof))
    advance();
}

void M2Parser::parseValueParameterSpecification() {
  {
    parseIdentifierList();
    if (consume(tok::colon))
      goto _error;
    parseFormalType();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::semi, tok::eof))
    advance();
}

void M2Parser::parseVariableParameterSpecification() {
  {
    if (consume(tok::kw_VAR))
      goto _error;
    parseIdentifierList();
    if (consume(tok::colon))
      goto _error;
    parseFormalType();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::semi, tok::eof))
    advance();
}

void M2Parser::parseDeclarations() {
  {
    DeclList Decls;
    while (Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_CLASS, tok::kw_CONST, tok::kw_MODULE, tok::kw_PROCEDURE, tok::kw_TRACED, tok::kw_TYPE, tok::kw_VAR)) {
      if (Tok.is(tok::kw_CONST)) {
        advance();
        while (Tok.is(tok::identifier)) {
          parseConstantDeclaration(Decls);
          if (consume(tok::semi))
            goto _error;
        }
      }
      else if (Tok.is(tok::kw_TYPE)) {
        advance();
        while (Tok.is(tok::identifier)) {
          parseTypeDeclaration();
          if (consume(tok::semi))
            goto _error;
        }
      }
      else if (Tok.is(tok::kw_VAR)) {
        advance();
        while (Tok.is(tok::identifier)) {
          parseVariableDeclaration();
          if (consume(tok::semi))
            goto _error;
        }
      }
      else if (Tok.is(tok::kw_PROCEDURE)) {
        parseProcedureDeclaration();
        if (consume(tok::semi))
          goto _error;
      }
      else if (Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_CLASS, tok::kw_TRACED) && (getLangOpts().ISOObjects)) {
        parseClassDeclaration();
        if (consume(tok::semi))
          goto _error;
      }
      else if (Tok.is(tok::kw_MODULE)) {
        parseLocalModuleDeclaration();
        if (consume(tok::semi))
          goto _error;
      }
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_BEGIN, tok::kw_END, tok::eof))
    advance();
}

void M2Parser::parseConstantDeclaration(DeclList &Decls) {
  {
    SourceLocation Loc; StringRef Name;
    if (expect(tok::identifier))
      goto _error;
    Loc = Tok.getLocation(); Name = Tok.getIdentifier();
    advance();
    if (expect(tok::equal))
      goto _error;
    Expr *E = nullptr;
    advance();
    parseExpression(E);
    Decl *D = Actions.actOnConstantDecl(Loc, Name, E);
                                 Decls.push_back(D);
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseTypeDeclaration() {
  {
    SourceLocation Loc; StringRef Name;
    if (expect(tok::identifier))
      goto _error;
    Loc = Tok.getLocation(); Name = Tok.getIdentifier();
    advance();
    if (consume(tok::equal))
      goto _error;
    parseTypeDenoter();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseVariableDeclaration() {
  {
    parseVariableIdentifierList();
    if (consume(tok::colon))
      goto _error;
    parseTypeDenoter();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseVariableIdentifierList() {
  {
    if (consume(tok::identifier))
      goto _error;
    if (Tok.is(tok::l_square)) {
      parseMachineAddress();
    }
    while (Tok.is(tok::comma)) {
      advance();
      if (consume(tok::identifier))
        goto _error;
      if (Tok.is(tok::l_square)) {
        parseMachineAddress();
      }
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::colon, tok::eof))
    advance();
}

void M2Parser::parseMachineAddress() {
  {
    if (consume(tok::l_square))
      goto _error;
    parseValueOfAddressType();
    if (consume(tok::r_square))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::comma, tok::colon, tok::eof))
    advance();
}

void M2Parser::parseValueOfAddressType() {
  {
    parseConstantExpression();
    return;
  }
}

void M2Parser::parseProcedureDeclaration() {
  {
    bool HasParen = false;
    if (consume(tok::kw_PROCEDURE))
      goto _error;
    parseProcedureIdentifier();
    if (Tok.is(tok::l_paren)) {
      HasParen=true;
      advance();
      if (Tok.isOneOf(tok::kw_VAR, tok::identifier)) {
        parseFormalParameterList();
      }
      if (consume(tok::r_paren))
        goto _error;
    }
    if (consume(tok::semi))
      goto _error;
    if (Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_BEGIN, tok::kw_CLASS, tok::kw_CONST, tok::kw_END, tok::kw_MODULE, tok::kw_PROCEDURE, tok::kw_TRACED, tok::kw_TYPE, tok::kw_VAR)) {
      parseProperProcedureBlock(HasParen);
      parseProcedureIdentifier();
    }
    else if (Tok.is(tok::kw_FORWARD)) {
      advance();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseProcedureIdentifier() {
  {
    if (consume(tok::identifier))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::l_paren, tok::semi, tok::eof))
    advance();
}

void M2Parser::parseLocalModuleDeclaration() {
  {
    if (consume(tok::kw_MODULE))
      goto _error;
    parseModuleIdentifier();
    if (Tok.is(tok::equal)) {
       if (!(getLangOpts().ISOGenerics)) {
        /* ERROR */
        goto _error;
      }
      advance();
      parseGenericSeparateModuleIdentifier();
      if (Tok.is(tok::l_paren)) {
        parseActualModuleParameters();
      }
      if (consume(tok::semi))
        goto _error;
      if (Tok.is(tok::kw_EXPORT)) {
        parseExportList();
      }
      if (consume(tok::kw_END))
        goto _error;
    }
    else if (Tok.isOneOf(tok::semi, tok::l_square)) {
      if (Tok.is(tok::l_square)) {
        parseProtection();
      }
      if (consume(tok::semi))
        goto _error;
      parseImportLists();
      if (Tok.is(tok::kw_EXPORT)) {
        parseExportList();
      }
      parseModuleBlock();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    parseModuleIdentifier();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseTypeDenoter() {
  {
    if (Tok.is(tok::identifier)) {
      parseTypeIdentifier();
    }
    else if (Tok.isOneOf(tok::l_paren, tok::kw_ARRAY, tok::kw_PACKEDSET, tok::kw_POINTER, tok::kw_PROCEDURE, tok::kw_RECORD, tok::kw_SET, tok::l_square, tok::identifier)) {
      parseNewType();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_END, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseOrdinalTypeDenoter() {
  {
    if (Tok.is(tok::identifier)) {
      parseOrdinalTypeIdentifier();
    }
    else if (Tok.isOneOf(tok::l_paren, tok::l_square, tok::identifier)) {
      parseNewOrdinalType();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::comma, tok::semi, tok::kw_ELSE, tok::kw_END, tok::kw_OF, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseTypeIdentifier() {
  {
    parseQualifiedIdentifier();
    return;
  }
}

void M2Parser::parseOrdinalTypeIdentifier() {
  {
    parseTypeIdentifier();
    return;
  }
}

void M2Parser::parseNewType() {
  {
    if (Tok.isOneOf(tok::l_paren, tok::l_square, tok::identifier)) {
      parseNewOrdinalType();
    }
    else if (Tok.is(tok::kw_SET)) {
      parseSetType();
    }
    else if (Tok.is(tok::kw_PACKEDSET)) {
      parsePackedsetType();
    }
    else if (Tok.is(tok::kw_POINTER)) {
      parsePointerType();
    }
    else if (Tok.is(tok::kw_PROCEDURE)) {
      parseProcedureType();
    }
    else if (Tok.is(tok::kw_ARRAY)) {
      parseArrayType();
    }
    else if (Tok.is(tok::kw_RECORD)) {
      parseRecordType();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_END, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseNewOrdinalType() {
  {
    if (Tok.is(tok::l_paren)) {
      parseEnumerationType();
    }
    else if (Tok.isOneOf(tok::l_square, tok::identifier)) {
      parseSubrangeType();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::comma, tok::semi, tok::kw_ELSE, tok::kw_END, tok::kw_OF, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseEnumerationType() {
  {
    if (consume(tok::l_paren))
      goto _error;
    parseIdentifierList();
    if (consume(tok::r_paren))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::comma, tok::semi, tok::kw_ELSE, tok::kw_END, tok::kw_OF, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseIdentifierList() {
  {
    if (consume(tok::identifier))
      goto _error;
    while (Tok.is(tok::comma)) {
      advance();
      if (consume(tok::identifier))
        goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::colon, tok::semi, tok::eof))
    advance();
}

void M2Parser::parseSubrangeType() {
  {
    if (Tok.is(tok::identifier)) {
      parseRangeType();
    }
    if (consume(tok::l_square))
      goto _error;
    parseConstantExpression();
    if (consume(tok::ellipsis))
      goto _error;
    parseConstantExpression();
    if (consume(tok::r_square))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::comma, tok::semi, tok::kw_ELSE, tok::kw_END, tok::kw_OF, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseRangeType() {
  {
    parseOrdinalTypeIdentifier();
    return;
  }
}

void M2Parser::parseSetType() {
  {
    if (consume(tok::kw_SET))
      goto _error;
    if (consume(tok::kw_OF))
      goto _error;
    parseBaseType();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_END, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseBaseType() {
  {
    parseOrdinalTypeDenoter();
    return;
  }
}

void M2Parser::parsePackedsetType() {
  {
    if (consume(tok::kw_PACKEDSET))
      goto _error;
    if (consume(tok::kw_OF))
      goto _error;
    parseBaseType();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_END, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parsePointerType() {
  {
    if (consume(tok::kw_POINTER))
      goto _error;
    if (consume(tok::kw_TO))
      goto _error;
    parseBoundType();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_END, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseBoundType() {
  {
    parseTypeDenoter();
    return;
  }
}

void M2Parser::parseProcedureType() {
  {
    bool HasParen = false;
    if (consume(tok::kw_PROCEDURE))
      goto _error;
    if (Tok.is(tok::l_paren)) {
      HasParen = true;
      advance();
      if (Tok.isOneOf(tok::kw_ARRAY, tok::kw_VAR, tok::identifier)) {
        parseFormalParameterTypeList();
      }
      if (consume(tok::r_paren))
        goto _error;
    }
    if (Tok.is(tok::colon) && (HasParen)) {
      advance();
      parseFunctionResultType();
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_END, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseFormalParameterTypeList() {
  {
    parseFormalParameterType();
    while (Tok.is(tok::comma)) {
      advance();
      parseFormalParameterType();
    }
    return;
  }
}

void M2Parser::parseFormalParameterType() {
  {
    if (Tok.is(tok::kw_VAR)) {
      parseVariableFormalType();
    }
    else if (Tok.isOneOf(tok::kw_ARRAY, tok::identifier)) {
      parseValueFormalType();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::comma, tok::eof))
    advance();
}

void M2Parser::parseVariableFormalType() {
  {
    if (consume(tok::kw_VAR))
      goto _error;
    parseFormalType();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::comma, tok::eof))
    advance();
}

void M2Parser::parseValueFormalType() {
  {
    parseFormalType();
    return;
  }
}

void M2Parser::parseFormalType() {
  {
    if (Tok.is(tok::identifier)) {
      parseTypeIdentifier();
    }
    else if (Tok.is(tok::kw_ARRAY)) {
      parseOpenArrayFormalType();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::comma, tok::semi, tok::eof))
    advance();
}

void M2Parser::parseOpenArrayFormalType() {
  {
    if (consume(tok::kw_ARRAY))
      goto _error;
    if (consume(tok::kw_OF))
      goto _error;
    while (Tok.is(tok::kw_ARRAY)) {
      advance();
      if (consume(tok::kw_OF))
        goto _error;
    }
    parseTypeIdentifier();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::comma, tok::semi, tok::eof))
    advance();
}

void M2Parser::parseArrayType() {
  {
    if (consume(tok::kw_ARRAY))
      goto _error;
    parseIndexType();
    while (Tok.is(tok::comma)) {
      advance();
      parseIndexType();
    }
    if (consume(tok::kw_OF))
      goto _error;
    parseComponentType();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_END, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseIndexType() {
  {
    parseOrdinalTypeDenoter();
    return;
  }
}

void M2Parser::parseComponentType() {
  {
    parseTypeDenoter();
    return;
  }
}

void M2Parser::parseRecordType() {
  {
    if (consume(tok::kw_RECORD))
      goto _error;
    parseFieldList();
    if (consume(tok::kw_END))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_END, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseFieldList() {
  {
    parseFields();
    while (Tok.is(tok::semi)) {
      advance();
      parseFields();
    }
    return;
  }
}

void M2Parser::parseFields() {
  {
    if (Tok.isOneOf(tok::kw_CASE, tok::identifier)) {
      if (Tok.is(tok::identifier)) {
        parseFixedFields();
      }
      else if (Tok.is(tok::kw_CASE)) {
        parseVariantFields();
      }
    }
    return;
  }
}

void M2Parser::parseFixedFields() {
  {
    parseIdentifierList();
    if (consume(tok::colon))
      goto _error;
    parseFieldType();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_END, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseFieldType() {
  {
    parseTypeDenoter();
    return;
  }
}

void M2Parser::parseVariantFields() {
  {
    if (consume(tok::kw_CASE))
      goto _error;
    if (Tok.is(tok::identifier)) {
      parseTagIdentifier();
    }
    if (consume(tok::colon))
      goto _error;
    parseTagType();
    if (consume(tok::kw_OF))
      goto _error;
    parseVariantList();
    if (consume(tok::kw_END))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_END, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseTagIdentifier() {
  {
    if (consume(tok::identifier))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::colon, tok::eof))
    advance();
}

void M2Parser::parseTagType() {
  {
    parseOrdinalTypeIdentifier();
    return;
  }
}

void M2Parser::parseVariantList() {
  {
    parseVariant();
    while (Tok.is(tok::pipe)) {
      advance();
      parseVariant();
    }
    if (Tok.is(tok::kw_ELSE)) {
      parseVariantElsePart();
    }
    return;
  }
}

void M2Parser::parseVariantElsePart() {
  {
    if (consume(tok::kw_ELSE))
      goto _error;
    parseFieldList();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_END, tok::eof))
    advance();
}

void M2Parser::parseVariant() {
  {
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
      parseVariantLabelList();
      if (consume(tok::colon))
        goto _error;
      parseFieldList();
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_ELSE, tok::kw_END, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseVariantLabelList() {
  {
    parseVariantLabel();
    while (Tok.is(tok::comma)) {
      advance();
      parseVariantLabel();
    }
    return;
  }
}

void M2Parser::parseVariantLabel() {
  {
    parseConstantExpression();
    if (Tok.is(tok::ellipsis)) {
      advance();
      parseConstantExpression();
    }
    return;
  }
}

void M2Parser::parseProperProcedureBlock(bool IsFunction) {
  {
    parseDeclarations();
    if (Tok.is(tok::kw_BEGIN)) {
      advance();
      parseBlockBody();
    }
    else if (Tok.is(tok::kw_END) && (IsFunction)) {
    }
    if (consume(tok::kw_END))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::eof, tok::identifier))
    advance();
}

void M2Parser::parseModuleBlock() {
  {
    parseDeclarations();
    if (Tok.is(tok::kw_BEGIN)) {
      parseModuleBody();
    }
    if (consume(tok::kw_END))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::eof, tok::identifier))
    advance();
}

void M2Parser::parseModuleBody() {
  {
    parseInitializationBody();
    if (Tok.is(tok::kw_FINALLY)) {
      parseFinalizationBody();
    }
    return;
  }
}

void M2Parser::parseInitializationBody() {
  {
    if (consume(tok::kw_BEGIN))
      goto _error;
    parseBlockBody();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_END, tok::kw_FINALLY, tok::eof))
    advance();
}

void M2Parser::parseFinalizationBody() {
  {
    if (consume(tok::kw_FINALLY))
      goto _error;
    parseBlockBody();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_END, tok::eof))
    advance();
}

void M2Parser::parseBlockBody() {
  {
    StmtList Stmts; /*ERROR*/
    parseNormalPart(Stmts);
    if (Tok.is(tok::kw_EXCEPT)) {
      advance();
      parseExceptionalPart(Stmts);
    }
    return;
  }
}

void M2Parser::parseNormalPart(StmtList &Stmts) {
  {
    parseStatementSequence(Stmts);
    return;
  }
}

void M2Parser::parseExceptionalPart(StmtList &Stmts) {
  {
    parseStatementSequence(Stmts);
    return;
  }
}

void M2Parser::parseStatement(Stmt *&S) {
  {
    if (Tok.isOneOf(tok::kw_CASE, tok::kw_EXIT, tok::kw_FOR, tok::kw_GUARD, tok::kw_IF, tok::kw_LOOP, tok::kw_REPEAT, tok::kw_RETRY, tok::kw_RETURN, tok::kw_WHILE, tok::kw_WITH, tok::identifier)) {
      if (Tok.is(tok::identifier)) {
        parseAssignmentStatement(S);
      }
      else if (Tok.is(tok::identifier)) {
        parseProcedureCall(S);
      }
      else if (Tok.is(tok::kw_RETURN)) {
        parseReturnStatement(S);
      }
      else if (Tok.is(tok::kw_RETRY)) {
        parseRetryStatement(S);
      }
      else if (Tok.is(tok::kw_WITH)) {
        parseWithStatement(S);
      }
      else if (Tok.is(tok::kw_IF)) {
        parseIfStatement(S);
      }
      else if (Tok.is(tok::kw_CASE)) {
        parseCaseStatement(S);
      }
      else if (Tok.is(tok::kw_WHILE)) {
        parseWhileStatement(S);
      }
      else if (Tok.is(tok::kw_REPEAT)) {
        parseRepeatStatement(S);
      }
      else if (Tok.is(tok::kw_LOOP)) {
        parseLoopStatement(S);
      }
      else if (Tok.is(tok::kw_EXIT)) {
        parseExitStatement(S);
      }
      else if (Tok.is(tok::kw_FOR)) {
        parseForStatement(S);
      }
      else if (Tok.is(tok::kw_GUARD) && (getLangOpts().ISOObjects)) {
        parseGuardStatement(S);
      }
    }
    return;
  }
}

void M2Parser::parseStatementSequence(StmtList &Stmts) {
  {
    Stmt *S = nullptr;
    parseStatement(S);
    if (S) Stmts.push_back(S);
    while (Tok.is(tok::semi)) {
      S = nullptr;
      advance();
      parseStatement(S);
      if (S) Stmts.push_back(S);
    }
    return;
  }
}

void M2Parser::parseAssignmentStatement(Stmt *&S) {
  {
    Expr *E;
    parseVariableDesignator();
    if (consume(tok::colonequal))
      goto _error;
    parseExpression(E);
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_UNTIL, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseProcedureCall(Stmt *&S) {
  {
    parseProcedureDesignator();
    if (Tok.is(tok::l_paren)) {
      parseActualParameters();
    }
    return;
  }
}

void M2Parser::parseProcedureDesignator() {
  {
    parseValueDesignator();
    return;
  }
}

void M2Parser::parseReturnStatement(Stmt *&S) {
  {
    Expr *E = nullptr;
    if (consume(tok::kw_RETURN))
      goto _error;
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
      parseExpression(E);
    }
    S = Actions.actOnReturnStmt(E);
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_UNTIL, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseRetryStatement(Stmt *&S) {
  {
    if (expect(tok::kw_RETRY))
      goto _error;
    S = Actions.actOnRetryStmt();
    advance();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_UNTIL, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseWithStatement(Stmt *&S) {
  {
    StmtList Stmts;
    if (consume(tok::kw_WITH))
      goto _error;
    parseRecordDesignator();
    if (consume(tok::kw_DO))
      goto _error;
    parseStatementSequence(Stmts);
    if (consume(tok::kw_END))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_UNTIL, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseRecordDesignator() {
  {
    if (Tok.is(tok::identifier)) {
      parseVariableDesignator();
    }
    else if (Tok.is(tok::identifier)) {
      parseValueDesignator();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_DO, tok::eof))
    advance();
}

void M2Parser::parseIfStatement(Stmt *&S) {
  {
    parseGuardedStatements();
    if (Tok.is(tok::kw_ELSE)) {
      parseIfElsePart();
    }
    if (consume(tok::kw_END))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_UNTIL, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseGuardedStatements() {
  {
    StmtList Stmts; /* ERROR */
    if (consume(tok::kw_IF))
      goto _error;
    parseBooleanExpression();
    if (consume(tok::kw_THEN))
      goto _error;
    parseStatementSequence(Stmts);
    while (Tok.is(tok::kw_ELSIF)) {
      advance();
      parseBooleanExpression();
      if (consume(tok::kw_THEN))
        goto _error;
      parseStatementSequence(Stmts);
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_ELSE, tok::kw_END, tok::eof))
    advance();
}

void M2Parser::parseIfElsePart() {
  {
    StmtList Stmts; /* ERROR */
    if (consume(tok::kw_ELSE))
      goto _error;
    parseStatementSequence(Stmts);
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_END, tok::eof))
    advance();
}

void M2Parser::parseBooleanExpression() {
  {
    Expr *E;
    parseExpression(E);
    return;
  }
}

void M2Parser::parseCaseStatement(Stmt *&S) {
  {
    if (consume(tok::kw_CASE))
      goto _error;
    parseCaseSelector();
    if (consume(tok::kw_OF))
      goto _error;
    parseCaseList();
    if (consume(tok::kw_END))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_UNTIL, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseCaseSelector() {
  {
    parseOrdinalExpression();
    return;
  }
}

void M2Parser::parseCaseList() {
  {
    parseCaseAlternative();
    while (Tok.is(tok::pipe)) {
      advance();
      parseCaseAlternative();
    }
    if (Tok.is(tok::kw_ELSE)) {
      parseCaseElsePart();
    }
    return;
  }
}

void M2Parser::parseCaseElsePart() {
  {
    StmtList Stmts; /* ERROR */
    if (consume(tok::kw_ELSE))
      goto _error;
    parseStatementSequence(Stmts);
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_END, tok::eof))
    advance();
}

void M2Parser::parseCaseAlternative() {
  {
    StmtList Stmts; /* ERROR */
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
      parseCaseLabelList();
      if (consume(tok::colon))
        goto _error;
      parseStatementSequence(Stmts);
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_ELSE, tok::kw_END, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseCaseLabelList() {
  {
    parseCaseLabel();
    while (Tok.is(tok::comma)) {
      advance();
      parseCaseLabel();
    }
    return;
  }
}

void M2Parser::parseCaseLabel() {
  {
    parseConstantExpression();
    if (Tok.is(tok::ellipsis)) {
      advance();
      parseConstantExpression();
    }
    return;
  }
}

void M2Parser::parseWhileStatement(Stmt *&S) {
  {
    if (expect(tok::kw_WHILE))
      goto _error;
    Expr *Cond = nullptr;
    advance();
    parseExpression(Cond);
    if (expect(tok::kw_DO))
      goto _error;
    StmtList Stmts;
    advance();
    parseStatementSequence(Stmts);
    if (expect(tok::kw_END))
      goto _error;
    S = Actions.actOnWhileStmt(Cond);
    advance();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_UNTIL, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseRepeatStatement(Stmt *&S) {
  {
    StmtList Stmts; /* ERROR */
    if (consume(tok::kw_REPEAT))
      goto _error;
    parseStatementSequence(Stmts);
    if (consume(tok::kw_UNTIL))
      goto _error;
    parseBooleanExpression();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_UNTIL, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseLoopStatement(Stmt *&S) {
  {
    StmtList Stmts; /* ERROR */
    if (consume(tok::kw_LOOP))
      goto _error;
    parseStatementSequence(Stmts);
    if (consume(tok::kw_END))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_UNTIL, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseExitStatement(Stmt *&S) {
  {
    if (consume(tok::kw_EXIT))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_UNTIL, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseForStatement(Stmt *&S) {
  {
    StmtList Stmts; /* ERROR */
    if (consume(tok::kw_FOR))
      goto _error;
    parseControlVariableIdentifier();
    if (consume(tok::colonequal))
      goto _error;
    parseInitialValue();
    if (consume(tok::kw_TO))
      goto _error;
    parseFinalValue();
    if (Tok.is(tok::kw_BY)) {
      advance();
      parseStepSize();
    }
    if (consume(tok::kw_DO))
      goto _error;
    parseStatementSequence(Stmts);
    if (consume(tok::kw_END))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_UNTIL, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseControlVariableIdentifier() {
  {
    if (consume(tok::identifier))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::colonequal, tok::eof))
    advance();
}

void M2Parser::parseInitialValue() {
  {
    parseOrdinalExpression();
    return;
  }
}

void M2Parser::parseFinalValue() {
  {
    parseOrdinalExpression();
    return;
  }
}

void M2Parser::parseStepSize() {
  {
    parseConstantExpression();
    return;
  }
}

void M2Parser::parseVariableDesignator() {
  {
    if (Tok.is(tok::identifier)) {
      parseEntireDesignator();
    }
    else if (Tok.is(tok::identifier)) {
      parseIndexedDesignator();
    }
    else if (Tok.is(tok::identifier)) {
      parseSelectedDesignator();
    }
    else if (Tok.is(tok::identifier)) {
      parseDereferencedDesignator();
    }
    else if (Tok.is(tok::identifier)) {
       if (!(getLangOpts().ISOObjects)) {
        /* ERROR */
        goto _error;
      }
      parseObjectSelectedDesignator();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::comma, tok::period, tok::colonequal, tok::kw_DO, tok::l_square, tok::caret, tok::eof))
    advance();
}

void M2Parser::parseEntireDesignator() {
  {
    parseQualifiedIdentifier();
    return;
  }
}

void M2Parser::parseIndexedDesignator() {
  {
    parseArrayVariableDesignator();
    if (consume(tok::l_square))
      goto _error;
    parseIndexExpression();
    while (Tok.is(tok::comma)) {
      advance();
      parseIndexExpression();
    }
    if (consume(tok::r_square))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::comma, tok::period, tok::colonequal, tok::kw_DO, tok::l_square, tok::caret, tok::eof))
    advance();
}

void M2Parser::parseArrayVariableDesignator() {
  {
    parseVariableDesignator();
    return;
  }
}

void M2Parser::parseIndexExpression() {
  {
    parseOrdinalExpression();
    return;
  }
}

void M2Parser::parseSelectedDesignator() {
  {
    parseRecordVariableDesignator();
    if (consume(tok::period))
      goto _error;
    parseFieldIdentifier();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::comma, tok::period, tok::colonequal, tok::kw_DO, tok::l_square, tok::caret, tok::eof))
    advance();
}

void M2Parser::parseRecordVariableDesignator() {
  {
    parseVariableDesignator();
    return;
  }
}

void M2Parser::parseFieldIdentifier() {
  {
    if (consume(tok::identifier))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::l_paren, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::period, tok::ellipsis, tok::slash, tok::colon, tok::colonequal, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::l_square, tok::r_square, tok::caret, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseDereferencedDesignator() {
  {
    parsePointerVariableDesignator();
    if (consume(tok::caret))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::comma, tok::period, tok::colonequal, tok::kw_DO, tok::l_square, tok::caret, tok::eof))
    advance();
}

void M2Parser::parsePointerVariableDesignator() {
  {
    parseVariableDesignator();
    return;
  }
}

void M2Parser::parseExpression(Expr *&E) {
  {
    parseSimpleExpression(E);
    if (Tok.isOneOf(tok::hash, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_IN)) {
      OperatorInfo Op;
      parseRelationalOperator(Op);
      Expr *Right;
      parseSimpleExpression(Right);
      E = Actions.actOnExpression(E, Right, Op);
    }
    return;
  }
}

void M2Parser::parseSimpleExpression(Expr *&E) {
  {
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::kw_NOT, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
      OperatorInfo Op;
      if (Tok.is(tok::plus)) {
        Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
        advance();
      }
      parseTerm(E);
      if (Op.getKind() != tok::unknown)
                                   E = Actions.actOnFactor(E, Op);
      while (Tok.isOneOf(tok::plus, tok::minus, tok::kw_OR)) {
        OperatorInfo Op;
        parseTermOperator(Op);
        Expr *Right;
        parseTerm(Right);
        E = Actions.actOnSimpleExpression(E, Right, Op);
      }
    }
    else if (Tok.is(tok::minus)) {
      OperatorInfo Op(Tok.getLocation(), Tok.getKind());
      advance();
      parseFactor(E);
      E = Actions.actOnFactor(E, Op);
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::r_paren, tok::comma, tok::ellipsis, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AS, tok::kw_BY, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_OF, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::r_square, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseTerm(Expr *&E) {
  {
    parseFactor(E);
    while (Tok.isOneOf(tok::star, tok::slash, tok::kw_AND, tok::kw_DIV, tok::kw_MOD, tok::kw_REM)) {
      OperatorInfo Op;
      parseFactorOperator(Op);
      Expr *Right;
      parseFactor(Right);
      E = Actions.actOnTerm(E, Right, Op);
    }
    return;
  }
}

void M2Parser::parseFactor(Expr *&E) {
  {
    if (Tok.is(tok::l_paren)) {
      advance();
      parseExpression(E);
      if (consume(tok::r_paren))
        goto _error;
    }
    else if (Tok.is(tok::kw_NOT)) {
      OperatorInfo Op(Tok.getLocation(), Tok.getKind());
      advance();
      parseFactor(E);
      E = Actions.actOnFactor(E, Op);
    }
    else if (Tok.is(tok::identifier)) {
      parseValueDesignator();
    }
    else if (Tok.is(tok::identifier)) {
      parseFunctionCall();
    }
    else if (Tok.is(tok::identifier)) {
      parseValueConstructor();
    }
    else if (Tok.isOneOf(tok::char_literal, tok::integer_literal, tok::real_literal, tok::string_literal)) {
      parseConstantLiteral();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::ellipsis, tok::slash, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::r_square, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseOrdinalExpression() {
  {
    Expr *E;
    parseExpression(E);
    return;
  }
}

void M2Parser::parseRelationalOperator(OperatorInfo &Op) {
  {
    if (Tok.is(tok::equal)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else if (Tok.is(tok::hash)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else if (Tok.is(tok::less)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else if (Tok.is(tok::greater)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else if (Tok.is(tok::lessequal)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else if (Tok.is(tok::greaterequal)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else if (Tok.is(tok::kw_IN)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::eof, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal))
    advance();
}

void M2Parser::parseTermOperator(OperatorInfo &Op) {
  {
    if (Tok.is(tok::plus)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else if (Tok.is(tok::minus)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else if (Tok.is(tok::kw_OR)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::l_paren, tok::kw_NOT, tok::eof, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal))
    advance();
}

void M2Parser::parseFactorOperator(OperatorInfo &Op) {
  {
    if (Tok.is(tok::star)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else if (Tok.is(tok::slash)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else if (Tok.is(tok::kw_REM)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else if (Tok.is(tok::kw_DIV)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else if (Tok.is(tok::kw_MOD)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else if (Tok.is(tok::kw_AND)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::l_paren, tok::kw_NOT, tok::eof, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal))
    advance();
}

void M2Parser::parseValueDesignator() {
  {
    if (Tok.is(tok::identifier)) {
      parseEntireValue();
    }
    else if (Tok.is(tok::identifier)) {
      parseIndexedValue();
    }
    else if (Tok.is(tok::identifier)) {
      parseSelectedValue();
    }
    else if (Tok.is(tok::identifier)) {
      parseDereferencedValue();
    }
    else if (Tok.is(tok::identifier)) {
       if (!(getLangOpts().ISOObjects)) {
        /* ERROR */
        goto _error;
      }
      parseObjectSelectedValue();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::l_paren, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::period, tok::ellipsis, tok::slash, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::l_square, tok::r_square, tok::caret, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseEntireValue() {
  {
    parseQualifiedIdentifier();
    return;
  }
}

void M2Parser::parseIndexedValue() {
  {
    parseArrayValue();
    if (consume(tok::l_square))
      goto _error;
    parseIndexExpression();
    while (Tok.is(tok::comma)) {
      advance();
      parseIndexExpression();
    }
    if (consume(tok::r_square))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::l_paren, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::period, tok::ellipsis, tok::slash, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::l_square, tok::r_square, tok::caret, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseArrayValue() {
  {
    parseValueDesignator();
    return;
  }
}

void M2Parser::parseSelectedValue() {
  {
    parseRecordValue();
    if (consume(tok::period))
      goto _error;
    parseFieldIdentifier();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::l_paren, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::period, tok::ellipsis, tok::slash, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::l_square, tok::r_square, tok::caret, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseRecordValue() {
  {
    parseValueDesignator();
    return;
  }
}

void M2Parser::parseDereferencedValue() {
  {
    parsePointerValue();
    if (consume(tok::caret))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::l_paren, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::period, tok::ellipsis, tok::slash, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::l_square, tok::r_square, tok::caret, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parsePointerValue() {
  {
    parseValueDesignator();
    return;
  }
}

void M2Parser::parseFunctionCall() {
  {
    parseFunctionDesignator();
    parseActualParameters();
    return;
  }
}

void M2Parser::parseFunctionDesignator() {
  {
    parseValueDesignator();
    return;
  }
}

void M2Parser::parseValueConstructor() {
  {
    if (Tok.is(tok::identifier)) {
      parseArrayConstructor();
    }
    else if (Tok.is(tok::identifier)) {
      parseRecordConstructor();
    }
    else if (Tok.is(tok::identifier)) {
      parseSetConstructor();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::ellipsis, tok::slash, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::r_square, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseArrayConstructor() {
  {
    parseArrayTypeIdentifier();
    parseArrayConstructedValue();
    return;
  }
}

void M2Parser::parseArrayTypeIdentifier() {
  {
    parseTypeIdentifier();
    return;
  }
}

void M2Parser::parseArrayConstructedValue() {
  {
    if (consume(tok::l_brace))
      goto _error;
    parseRepeatedStructureComponent();
    while (Tok.is(tok::comma)) {
      advance();
      parseRepeatedStructureComponent();
    }
    if (consume(tok::r_brace))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::ellipsis, tok::slash, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::r_square, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseRepeatedStructureComponent() {
  {
    parseStructureComponent();
    if (Tok.is(tok::kw_BY)) {
      advance();
      parseRepetitionFactor();
    }
    return;
  }
}

void M2Parser::parseRepetitionFactor() {
  {
    parseConstantExpression();
    return;
  }
}

void M2Parser::parseStructureComponent() {
  {
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
      Expr *E;
      parseExpression(E);
    }
    else if (Tok.is(tok::l_brace)) {
      parseArrayConstructedValue();
    }
    else if (Tok.is(tok::l_brace)) {
      parseRecordConstructedValue();
    }
    else if (Tok.is(tok::l_brace)) {
      parseSetConstructedValue();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::comma, tok::kw_BY, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseRecordConstructor() {
  {
    parseRecordTypeIdentifier();
    parseRecordConstructedValue();
    return;
  }
}

void M2Parser::parseRecordTypeIdentifier() {
  {
    parseTypeIdentifier();
    return;
  }
}

void M2Parser::parseRecordConstructedValue() {
  {
    if (consume(tok::l_brace))
      goto _error;
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::l_brace, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
      parseStructureComponent();
      while (Tok.is(tok::comma)) {
        advance();
        parseStructureComponent();
      }
    }
    if (consume(tok::r_brace))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::ellipsis, tok::slash, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::r_square, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseSetConstructor() {
  {
    parseSetTypeIdentifier();
    parseSetConstructedValue();
    return;
  }
}

void M2Parser::parseSetTypeIdentifier() {
  {
    parseTypeIdentifier();
    return;
  }
}

void M2Parser::parseSetConstructedValue() {
  {
    if (consume(tok::l_brace))
      goto _error;
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
      parseMember();
      while (Tok.is(tok::comma)) {
        advance();
        parseMember();
      }
    }
    if (consume(tok::r_brace))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::ellipsis, tok::slash, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::r_square, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseMember() {
  {
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
      parseInterval();
    }
    else if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
      parseSingleton();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::comma, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseInterval() {
  {
    parseOrdinalExpression();
    if (consume(tok::ellipsis))
      goto _error;
    parseOrdinalExpression();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::comma, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseSingleton() {
  {
    parseOrdinalExpression();
    return;
  }
}

void M2Parser::parseConstantLiteral() {
  {
    if (Tok.is(tok::integer_literal)) {
      advance();
    }
    else if (Tok.is(tok::real_literal)) {
      advance();
    }
    else if (Tok.isOneOf(tok::char_literal, tok::string_literal)) {
      parseStringLiteral();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::ellipsis, tok::slash, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::r_square, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseStringLiteral() {
  {
    if (Tok.is(tok::string_literal)) {
      advance();
    }
    else if (Tok.is(tok::char_literal)) {
      advance();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::ellipsis, tok::slash, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::r_square, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseConstantExpression() {
  {
    Expr *E;
    parseExpression(E);
    return;
  }
}

void M2Parser::parseActualParameters() {
  {
    if (consume(tok::l_paren))
      goto _error;
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
      parseActualParameterList();
    }
    if (consume(tok::r_paren))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::ellipsis, tok::slash, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::r_square, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseActualParameterList() {
  {
    parseActualParameter();
    while (Tok.is(tok::comma)) {
      advance();
      parseActualParameter();
    }
    return;
  }
}

void M2Parser::parseActualParameter() {
  {
    Expr *E;
    if (Tok.is(tok::identifier)) {
      parseVariableDesignator();
    }
    else if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::char_literal, tok::identifier, tok::integer_literal, tok::real_literal, tok::string_literal)) {
      parseExpression(E);
    }
    else if (Tok.is(tok::identifier)) {
      parseTypeParameter();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::comma, tok::eof))
    advance();
}

void M2Parser::parseTypeParameter() {
  {
    parseTypeIdentifier();
    return;
  }
}

void M2Parser::parseClassDefinition() {
  {
    if (Tok.is(tok::kw_TRACED)) {
      parseTracedClassDefinition();
    }
    else if (Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_CLASS)) {
      parseUntracedClassDefinition();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseUntracedClassDefinition() {
  {
    if (Tok.is(tok::kw_CLASS)) {
      parseNormalClassDefinition();
    }
    else if (Tok.is(tok::kw_ABSTRACT)) {
      parseAbstractClassDefinition();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseTracedClassDefinition() {
  {
    if (consume(tok::kw_TRACED))
      goto _error;
    if (Tok.is(tok::kw_CLASS)) {
      parseNormalClassDefinition();
    }
    else if (Tok.is(tok::kw_ABSTRACT)) {
      parseAbstractClassDefinition();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseNormalClassDefinition() {
  {
    parseNormalClassHeader();
    if (Tok.isOneOf(tok::kw_CONST, tok::kw_END, tok::kw_INHERIT, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_REVEAL, tok::kw_TYPE, tok::kw_VAR)) {
      parseNormalClassDefinitionBody();
    }
    else if (Tok.is(tok::kw_FORWARD)) {
      advance();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseNormalClassHeader() {
  {
    if (consume(tok::kw_CLASS))
      goto _error;
    parseClassIdentifier();
    if (consume(tok::semi))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_BEGIN, tok::kw_CONST, tok::kw_END, tok::kw_FORWARD, tok::kw_INHERIT, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_REVEAL, tok::kw_TYPE, tok::kw_VAR, tok::eof))
    advance();
}

void M2Parser::parseNormalClassDefinitionBody() {
  {
    if (Tok.is(tok::kw_INHERIT)) {
      parseInheritClause();
    }
    if (Tok.is(tok::kw_REVEAL)) {
      parseRevealList();
    }
    parseNormalClassComponentDefinitions();
    if (consume(tok::kw_END))
      goto _error;
    parseClassIdentifier();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseAbstractClassDefinition() {
  {
    parseAbstractClassHeader();
    if (Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_CONST, tok::kw_END, tok::kw_INHERIT, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_REVEAL, tok::kw_TYPE, tok::kw_VAR)) {
      parseAbstractClassDefinitionBody();
    }
    else if (Tok.is(tok::kw_FORWARD)) {
      advance();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseAbstractClassHeader() {
  {
    if (consume(tok::kw_ABSTRACT))
      goto _error;
    if (consume(tok::kw_CLASS))
      goto _error;
    parseClassIdentifier();
    if (consume(tok::semi))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_BEGIN, tok::kw_CONST, tok::kw_END, tok::kw_FORWARD, tok::kw_INHERIT, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_REVEAL, tok::kw_TYPE, tok::kw_VAR, tok::eof))
    advance();
}

void M2Parser::parseAbstractClassDefinitionBody() {
  {
    if (Tok.is(tok::kw_INHERIT)) {
      parseInheritClause();
    }
    if (Tok.is(tok::kw_REVEAL)) {
      parseRevealList();
    }
    parseAbstractClassComponentDefinitions();
    if (consume(tok::kw_END))
      goto _error;
    parseClassIdentifier();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseClassIdentifier() {
  {
    if (consume(tok::identifier))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::period, tok::semi, tok::eof, tok::identifier))
    advance();
}

void M2Parser::parseNormalClassComponentDefinitions() {
  {
    while (Tok.isOneOf(tok::kw_CONST, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_TYPE, tok::kw_VAR)) {
      parseNormalComponentDefinition();
    }
    return;
  }
}

void M2Parser::parseNormalComponentDefinition() {
  {
    DeclList Decls;
    if (Tok.is(tok::kw_CONST)) {
      advance();
      while (Tok.is(tok::identifier)) {
        parseConstantDeclaration(Decls);
        if (consume(tok::semi))
          goto _error;
      }
    }
    else if (Tok.is(tok::kw_TYPE)) {
      advance();
      while (Tok.is(tok::identifier)) {
        parseTypeDefinition();
        if (consume(tok::semi))
          goto _error;
      }
    }
    else if (Tok.is(tok::kw_VAR)) {
      advance();
      if (Tok.is(tok::identifier)) {
        parseClassVariableDeclaration();
        if (consume(tok::semi))
          goto _error;
      }
    }
    else if (Tok.isOneOf(tok::kw_OVERRIDE, tok::kw_PROCEDURE)) {
      if (Tok.is(tok::kw_PROCEDURE)) {
        parseNormalMethodDefinition();
      }
      else if (Tok.is(tok::kw_OVERRIDE)) {
        parseOverridingMethodDefinition();
      }
      else {
        /*ERROR*/
        goto _error;
      }
      if (consume(tok::semi))
        goto _error;
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_CONST, tok::kw_END, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_TYPE, tok::kw_VAR, tok::eof))
    advance();
}

void M2Parser::parseAbstractClassComponentDefinitions() {
  {
    while (Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_CONST, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_TYPE, tok::kw_VAR)) {
      parseAbstractComponentDefinition();
    }
    return;
  }
}

void M2Parser::parseAbstractComponentDefinition() {
  {
    DeclList Decls;
    if (Tok.is(tok::kw_CONST)) {
      advance();
      while (Tok.is(tok::identifier)) {
        parseConstantDeclaration(Decls);
        if (consume(tok::semi))
          goto _error;
      }
    }
    else if (Tok.is(tok::kw_TYPE)) {
      advance();
      while (Tok.is(tok::identifier)) {
        parseTypeDefinition();
        if (consume(tok::semi))
          goto _error;
      }
    }
    else if (Tok.is(tok::kw_VAR)) {
      advance();
      while (Tok.is(tok::identifier)) {
        parseClassVariableDeclaration();
        if (consume(tok::semi))
          goto _error;
      }
    }
    else if (Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_OVERRIDE, tok::kw_PROCEDURE)) {
      if (Tok.is(tok::kw_PROCEDURE)) {
        parseNormalMethodDefinition();
      }
      else if (Tok.is(tok::kw_ABSTRACT)) {
        parseAbstractMethodDefinition();
      }
      else if (Tok.is(tok::kw_OVERRIDE)) {
        parseOverridingMethodDefinition();
      }
      else {
        /*ERROR*/
        goto _error;
      }
      if (consume(tok::semi))
        goto _error;
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_CONST, tok::kw_END, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_TYPE, tok::kw_VAR, tok::eof))
    advance();
}

void M2Parser::parseClassVariableDeclaration() {
  {
    parseIdentifierList();
    if (consume(tok::colon))
      goto _error;
    parseTypeDenoter();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseNormalMethodDefinition() {
  {
    parseProcedureHeading();
    return;
  }
}

void M2Parser::parseOverridingMethodDefinition() {
  {
    if (consume(tok::kw_OVERRIDE))
      goto _error;
    parseProcedureHeading();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseAbstractMethodDefinition() {
  {
    if (consume(tok::kw_ABSTRACT))
      goto _error;
    parseProcedureHeading();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseClassDeclaration() {
  {
    if (Tok.is(tok::kw_TRACED)) {
      parseTracedClassDeclaration();
    }
    else if (Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_CLASS)) {
      parseUntracedClassDeclaration();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseUntracedClassDeclaration() {
  {
    if (Tok.is(tok::kw_CLASS)) {
      parseNormalClassDeclaration();
    }
    else if (Tok.is(tok::kw_ABSTRACT)) {
      parseAbstractClassDeclaration();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseNormalClassDeclaration() {
  {
    parseNormalClassHeader();
    if (Tok.isOneOf(tok::kw_BEGIN, tok::kw_CONST, tok::kw_END, tok::kw_INHERIT, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_REVEAL, tok::kw_TYPE, tok::kw_VAR)) {
      parseNormalClassDeclarationBody();
    }
    else if (Tok.is(tok::kw_FORWARD)) {
      advance();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseNormalClassDeclarationBody() {
  {
    if (Tok.is(tok::kw_INHERIT)) {
      parseInheritClause();
    }
    if (Tok.is(tok::kw_REVEAL)) {
      parseRevealList();
    }
    parseNormalClassComponentDeclarations();
    if (Tok.is(tok::kw_BEGIN)) {
      parseClassBody();
    }
    if (consume(tok::kw_END))
      goto _error;
    parseClassIdentifier();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseAbstractClassDeclaration() {
  {
    parseAbstractClassHeader();
    if (Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_BEGIN, tok::kw_CONST, tok::kw_END, tok::kw_INHERIT, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_REVEAL, tok::kw_TYPE, tok::kw_VAR)) {
      parseAbstractClassDeclarationBody();
    }
    else if (Tok.is(tok::kw_FORWARD)) {
      advance();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseAbstractClassDeclarationBody() {
  {
    if (Tok.is(tok::kw_INHERIT)) {
      parseInheritClause();
    }
    if (Tok.is(tok::kw_REVEAL)) {
      parseRevealList();
    }
    parseAbstractClassComponentDeclarations();
    if (Tok.is(tok::kw_BEGIN)) {
      parseClassBody();
    }
    if (consume(tok::kw_END))
      goto _error;
    parseClassIdentifier();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseClassBody() {
  {
    parseModuleBody();
    return;
  }
}

void M2Parser::parseNormalClassComponentDeclarations() {
  {
    while (Tok.isOneOf(tok::kw_CONST, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_TYPE, tok::kw_VAR)) {
      parseNormalComponentDeclaration();
    }
    return;
  }
}

void M2Parser::parseNormalComponentDeclaration() {
  {
    if (Tok.is(tok::kw_CONST)) {
      DeclList Decls;
      advance();
      while (Tok.is(tok::identifier)) {
        parseConstantDeclaration(Decls);
        if (consume(tok::semi))
          goto _error;
      }
    }
    else if (Tok.is(tok::kw_TYPE)) {
      advance();
      while (Tok.is(tok::identifier)) {
        parseTypeDeclaration();
        if (consume(tok::semi))
          goto _error;
      }
    }
    else if (Tok.is(tok::kw_VAR)) {
      advance();
      while (Tok.is(tok::identifier)) {
        parseClassVariableDeclaration();
        if (consume(tok::semi))
          goto _error;
      }
    }
    else if (Tok.isOneOf(tok::kw_OVERRIDE, tok::kw_PROCEDURE)) {
      parseNormalMethodDeclarations();
      if (consume(tok::semi))
        goto _error;
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_BEGIN, tok::kw_CONST, tok::kw_END, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_TYPE, tok::kw_VAR, tok::eof))
    advance();
}

void M2Parser::parseAbstractClassComponentDeclarations() {
  {
    while (Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_CONST, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_TYPE, tok::kw_VAR)) {
      parseAbstractComponentDeclaration();
    }
    return;
  }
}

void M2Parser::parseAbstractComponentDeclaration() {
  {
    DeclList Decls;
    if (Tok.is(tok::kw_CONST)) {
      advance();
      while (Tok.is(tok::identifier)) {
        parseConstantDeclaration(Decls);
        if (consume(tok::semi))
          goto _error;
      }
    }
    else if (Tok.is(tok::kw_TYPE)) {
      advance();
      while (Tok.is(tok::identifier)) {
        parseTypeDeclaration();
        if (consume(tok::semi))
          goto _error;
      }
    }
    else if (Tok.is(tok::kw_VAR)) {
      advance();
      while (Tok.is(tok::identifier)) {
        parseClassVariableDeclaration();
        if (consume(tok::semi))
          goto _error;
      }
    }
    else if (Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_OVERRIDE, tok::kw_PROCEDURE)) {
      parseAbstractMethodDeclarations();
      if (consume(tok::semi))
        goto _error;
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_BEGIN, tok::kw_CONST, tok::kw_END, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_TYPE, tok::kw_VAR, tok::eof))
    advance();
}

void M2Parser::parseNormalMethodDeclarations() {
  {
    if (Tok.is(tok::kw_PROCEDURE)) {
      parseNormalMethodDeclaration();
    }
    else if (Tok.is(tok::kw_OVERRIDE)) {
      parseOverridingMethodDeclaration();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseNormalMethodDeclaration() {
  {
    parseProcedureDeclaration();
    return;
  }
}

void M2Parser::parseOverridingMethodDeclaration() {
  {
    if (consume(tok::kw_OVERRIDE))
      goto _error;
    parseProcedureDeclaration();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseAbstractMethodDeclarations() {
  {
    if (Tok.is(tok::kw_PROCEDURE)) {
      parseNormalMethodDeclaration();
    }
    else if (Tok.is(tok::kw_ABSTRACT)) {
      parseAbstractMethodDefinition();
    }
    else if (Tok.is(tok::kw_OVERRIDE)) {
      parseOverridingMethodDeclaration();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseTracedClassDeclaration() {
  {
    if (consume(tok::kw_TRACED))
      goto _error;
    if (Tok.is(tok::kw_CLASS)) {
      parseNormalTracedClassDeclaration();
    }
    else if (Tok.is(tok::kw_ABSTRACT)) {
      parseAbstractTracedClassDeclaration();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseNormalTracedClassDeclaration() {
  {
    parseNormalTracedClassHeader();
    if (Tok.isOneOf(tok::kw_BEGIN, tok::kw_CONST, tok::kw_END, tok::kw_INHERIT, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_REVEAL, tok::kw_TYPE, tok::kw_VAR)) {
      parseNormalTracedClassDeclarationBody();
    }
    else if (Tok.is(tok::kw_FORWARD)) {
      advance();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseNormalTracedClassHeader() {
  {
    if (consume(tok::kw_CLASS))
      goto _error;
    parseClassIdentifier();
    if (consume(tok::semi))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_BEGIN, tok::kw_CONST, tok::kw_END, tok::kw_FORWARD, tok::kw_INHERIT, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_REVEAL, tok::kw_TYPE, tok::kw_VAR, tok::eof))
    advance();
}

void M2Parser::parseNormalTracedClassDeclarationBody() {
  {
    if (Tok.is(tok::kw_INHERIT)) {
      parseInheritClause();
    }
    if (Tok.is(tok::kw_REVEAL)) {
      parseRevealList();
    }
    parseNormalClassComponentDeclarations();
    if (Tok.is(tok::kw_BEGIN)) {
      parseTracedClassBody();
    }
    if (consume(tok::kw_END))
      goto _error;
    parseClassIdentifier();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseAbstractTracedClassDeclaration() {
  {
    parseAbstractTracedClassHeader();
    if (Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_BEGIN, tok::kw_CONST, tok::kw_END, tok::kw_INHERIT, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_REVEAL, tok::kw_TYPE, tok::kw_VAR)) {
      parseAbstractTracedClassDeclarationBody();
    }
    else if (Tok.is(tok::kw_FORWARD)) {
      advance();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseAbstractTracedClassHeader() {
  {
    if (consume(tok::kw_ABSTRACT))
      goto _error;
    if (consume(tok::kw_CLASS))
      goto _error;
    parseClassIdentifier();
    if (consume(tok::semi))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_BEGIN, tok::kw_CONST, tok::kw_END, tok::kw_FORWARD, tok::kw_INHERIT, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_REVEAL, tok::kw_TYPE, tok::kw_VAR, tok::eof))
    advance();
}

void M2Parser::parseAbstractTracedClassDeclarationBody() {
  {
    if (Tok.is(tok::kw_INHERIT)) {
      parseInheritClause();
    }
    if (Tok.is(tok::kw_REVEAL)) {
      parseRevealList();
    }
    parseAbstractClassComponentDeclarations();
    if (Tok.is(tok::kw_BEGIN)) {
      parseTracedClassBody();
    }
    if (consume(tok::kw_END))
      goto _error;
    parseClassIdentifier();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::eof))
    advance();
}

void M2Parser::parseTracedClassBody() {
  {
    if (consume(tok::kw_BEGIN))
      goto _error;
    parseBlockBody();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_END, tok::eof))
    advance();
}

void M2Parser::parseRevealList() {
  {
    if (consume(tok::kw_REVEAL))
      goto _error;
    parseRevealedComponentList();
    if (consume(tok::semi))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_BEGIN, tok::kw_CONST, tok::kw_END, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_TYPE, tok::kw_VAR, tok::eof))
    advance();
}

void M2Parser::parseRevealedComponentList() {
  {
    parseRevealedComponent();
    while (Tok.is(tok::comma)) {
      advance();
      parseRevealedComponent();
    }
    return;
  }
}

void M2Parser::parseRevealedComponent() {
  {
    if (Tok.is(tok::identifier)) {
      advance();
    }
    else if (Tok.is(tok::kw_READONLY)) {
      advance();
      parseClassVariableIdentifier();
    }
    else {
      /*ERROR*/
      goto _error;
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::comma, tok::semi, tok::eof))
    advance();
}

void M2Parser::parseClassVariableIdentifier() {
  {
    if (consume(tok::identifier))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::comma, tok::period, tok::colonequal, tok::semi, tok::kw_DO, tok::l_square, tok::caret, tok::eof))
    advance();
}

void M2Parser::parseInheritClause() {
  {
    if (consume(tok::kw_INHERIT))
      goto _error;
    parseClassTypeIdentifier();
    if (consume(tok::semi))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_ABSTRACT, tok::kw_BEGIN, tok::kw_CONST, tok::kw_END, tok::kw_OVERRIDE, tok::kw_PROCEDURE, tok::kw_REVEAL, tok::kw_TYPE, tok::kw_VAR, tok::eof))
    advance();
}

void M2Parser::parseClassTypeIdentifier() {
  {
    parseTypeIdentifier();
    return;
  }
}

void M2Parser::parseObjectSelectedDesignator() {
  {
    parseObjectVariableDesignator();
    if (consume(tok::period))
      goto _error;
    if (Tok.is(tok::identifier)) {
      parseClassIdentifier();
      if (consume(tok::period))
        goto _error;
    }
    parseClassVariableIdentifier();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::r_paren, tok::comma, tok::period, tok::colonequal, tok::kw_DO, tok::l_square, tok::caret, tok::eof))
    advance();
}

void M2Parser::parseObjectVariableDesignator() {
  {
    parseVariableDesignator();
    return;
  }
}

void M2Parser::parseObjectSelectedValue() {
  {
    parseObjectValueDesignator();
    if (consume(tok::period))
      goto _error;
    if (Tok.is(tok::identifier)) {
      parseClassIdentifier();
      if (consume(tok::period))
        goto _error;
    }
    parseEntityIdentifier();
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::l_paren, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::period, tok::ellipsis, tok::slash, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::l_square, tok::r_square, tok::caret, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseObjectValueDesignator() {
  {
    parseValueDesignator();
    return;
  }
}

void M2Parser::parseEntityIdentifier() {
  {
    if (consume(tok::identifier))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::hash, tok::l_paren, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus, tok::period, tok::ellipsis, tok::slash, tok::colon, tok::semi, tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_AS, tok::kw_BY, tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_IN, tok::kw_MOD, tok::kw_OF, tok::kw_OR, tok::kw_REM, tok::kw_THEN, tok::kw_TO, tok::kw_UNTIL, tok::l_square, tok::r_square, tok::caret, tok::pipe, tok::r_brace, tok::eof))
    advance();
}

void M2Parser::parseGuardStatement(Stmt *&S) {
  {
    StmtList Stmts; /* ERROR */
    if (consume(tok::kw_GUARD))
      goto _error;
    parseGuardSelector();
    if (consume(tok::kw_AS))
      goto _error;
    parseGuardedList();
    if (Tok.is(tok::kw_ELSE)) {
      advance();
      parseStatementSequence(Stmts);
    }
    if (consume(tok::kw_END))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::semi, tok::kw_ELSE, tok::kw_ELSIF, tok::kw_END, tok::kw_EXCEPT, tok::kw_FINALLY, tok::kw_UNTIL, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseGuardSelector() {
  {
    Expr *E;
    parseExpression(E);
    return;
  }
}

void M2Parser::parseGuardedList() {
  {
    parseGuardedStatementSequence();
    if (Tok.is(tok::pipe)) {
      advance();
      parseGuardedStatementSequence();
    }
    return;
  }
}

void M2Parser::parseGuardedStatementSequence() {
  {
    StmtList Stmts; /* ERROR */
    if (Tok.isOneOf(tok::colon, tok::identifier)) {
      if (Tok.is(tok::identifier)) {
        parseObjectDenoter();
      }
      if (consume(tok::colon))
        goto _error;
      parseGuardedClassType();
      if (consume(tok::kw_DO))
        goto _error;
      parseStatementSequence(Stmts);
    }
    return;
  }
_error:
  while (!Tok.isOneOf(tok::kw_ELSE, tok::kw_END, tok::pipe, tok::eof))
    advance();
}

void M2Parser::parseGuardedClassType() {
  {
    parseClassTypeIdentifier();
    return;
  }
}

void M2Parser::parseObjectDenoter() {
  {
    if (consume(tok::identifier))
      goto _error;
    return;
  }
_error:
  while (!Tok.isOneOf(tok::colon, tok::eof))
    advance();
}
#endif // of M2PARSER_DEFINITION
