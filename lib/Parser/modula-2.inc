#ifdef M2PARSER_DECLARATION
template <unsigned NBits> struct BitSet {
  typedef uintptr_t BitWord;

  enum { BITWORD_SIZE = (unsigned)sizeof(BitWord) * CHAR_BIT };
  enum { MEM_SIZE = (NBits + BITWORD_SIZE - 1) / BITWORD_SIZE };

  BitWord Data[MEM_SIZE];

  BitSet() { clear(); }

  template <typename... Ts> BitSet(Ts... BitsToSet) {
    clear();
    set(BitsToSet...);
  }

  void clear() { std::memset(Data, 0, sizeof(Data)); }

  template <typename T> bool contains(T Idx) const {
    return (Data[Idx / BITWORD_SIZE] & BitWord(1) << (Idx % BITWORD_SIZE)) != 0;
  }

  BitSet<NBits> &operator|=(const BitSet<NBits> &Other) {
    for (unsigned I = 0; I < MEM_SIZE; ++I)
      Data[I] |= Other.Data[I];
    return *this;
  }

  BitSet<NBits> operator|(const BitSet<NBits> &Other) const {
    BitSet<NBits> Result;
    for (unsigned I = 0; I < MEM_SIZE; ++I)
      Result.Data[I] = Data[I] | Other.Data[I];
    return Result;
  }

  template <typename T> void set(T Idx) {
    Data[Idx / BITWORD_SIZE] |= BitWord(1) << (Idx % BITWORD_SIZE);
  }
  template <typename T, typename... Ts> void set(T Idx, Ts... Idxs) {
    set(Idx);
    set(Idxs...);
  }
};
using __TokenBitSet = BitSet<tok::NUM_TOKENS>;
static const __TokenBitSet __FollowSets[];bool __skipUntil(const __TokenBitSet &ActiveSets, const __TokenBitSet &CurrentSet);
bool parseCompilationModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM);
bool parseProgramModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool HasUnsafeGuarded);
bool parseModuleIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseProtection(const __TokenBitSet &__FollowSetCallers, Expression *&Expr);
bool parseDefinitionModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool HasUnsafeGuarded);
bool parseImplementationModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool HasUnsafeGuarded);
bool parseImportLists(const __TokenBitSet &__FollowSetCallers);
bool parseImportList(const __TokenBitSet &__FollowSetCallers);
bool parseSimpleImport(const __TokenBitSet &__FollowSetCallers);
bool parseUnqualifiedImport(const __TokenBitSet &__FollowSetCallers);
bool parseExportList(const __TokenBitSet &__FollowSetCallers);
bool parseQualifiedIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseGenericDefinitionModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM);
bool parseGenericImplementationModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM);
bool parseGenericSeparateModuleIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseFormalModuleParameters(const __TokenBitSet &__FollowSetCallers);
bool parseFormalModuleParameterList(const __TokenBitSet &__FollowSetCallers);
bool parseFormalModuleParameter(const __TokenBitSet &__FollowSetCallers);
bool parseConstantValueParameterSpecification(const __TokenBitSet &__FollowSetCallers);
bool parseTypeParameterSpecification(const __TokenBitSet &__FollowSetCallers);
bool parseActualModuleParameters(const __TokenBitSet &__FollowSetCallers);
bool parseActualModuleParameterList(const __TokenBitSet &__FollowSetCallers);
bool parseActualModuleParameter(const __TokenBitSet &__FollowSetCallers);
bool parseDefinitions(const __TokenBitSet &__FollowSetCallers);
bool parseProcedureHeading(const __TokenBitSet &__FollowSetCallers);
bool parseTypeDefinition(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseOpaqueTypeDefinition(const __TokenBitSet &__FollowSetCallers);
bool parseFormalParameters(const __TokenBitSet &__FollowSetCallers);
bool parseFormalParameterList(const __TokenBitSet &__FollowSetCallers);
bool parseFunctionResultType(const __TokenBitSet &__FollowSetCallers);
bool parseFormalParameter(const __TokenBitSet &__FollowSetCallers);
bool parseValueParameterSpecification(const __TokenBitSet &__FollowSetCallers);
bool parseVariableParameterSpecification(const __TokenBitSet &__FollowSetCallers);
bool parseDeclarations(const __TokenBitSet &__FollowSetCallers);
bool parseConstantDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseTypeDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseVariableDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseVariableIdentifierList(const __TokenBitSet &__FollowSetCallers);
bool parseMachineAddress(const __TokenBitSet &__FollowSetCallers);
bool parseValueOfAddressType(const __TokenBitSet &__FollowSetCallers);
bool parseProcedureDeclaration(const __TokenBitSet &__FollowSetCallers);
bool parseProcedureIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseLocalModuleDeclaration(const __TokenBitSet &__FollowSetCallers);
bool parseTypeDenoter(const __TokenBitSet &__FollowSetCallers);
bool parseOrdinalTypeDenoter(const __TokenBitSet &__FollowSetCallers);
bool parseTypeIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseOrdinalTypeIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseNewType(const __TokenBitSet &__FollowSetCallers);
bool parseNewOrdinalType(const __TokenBitSet &__FollowSetCallers);
bool parseEnumerationType(const __TokenBitSet &__FollowSetCallers);
bool parseIdentifierList(const __TokenBitSet &__FollowSetCallers);
bool parseSubrangeType(const __TokenBitSet &__FollowSetCallers);
bool parseRangeType(const __TokenBitSet &__FollowSetCallers);
bool parseSetType(const __TokenBitSet &__FollowSetCallers);
bool parseBaseType(const __TokenBitSet &__FollowSetCallers);
bool parsePackedsetType(const __TokenBitSet &__FollowSetCallers);
bool parsePointerType(const __TokenBitSet &__FollowSetCallers);
bool parseBoundType(const __TokenBitSet &__FollowSetCallers);
bool parseProcedureType(const __TokenBitSet &__FollowSetCallers);
bool parseFormalParameterTypeList(const __TokenBitSet &__FollowSetCallers);
bool parseFormalParameterType(const __TokenBitSet &__FollowSetCallers);
bool parseVariableFormalType(const __TokenBitSet &__FollowSetCallers);
bool parseValueFormalType(const __TokenBitSet &__FollowSetCallers);
bool parseFormalType(const __TokenBitSet &__FollowSetCallers);
bool parseOpenArrayFormalType(const __TokenBitSet &__FollowSetCallers);
bool parseArrayType(const __TokenBitSet &__FollowSetCallers);
bool parseIndexType(const __TokenBitSet &__FollowSetCallers);
bool parseComponentType(const __TokenBitSet &__FollowSetCallers);
bool parseRecordType(const __TokenBitSet &__FollowSetCallers);
bool parseFieldList(const __TokenBitSet &__FollowSetCallers);
bool parseFields(const __TokenBitSet &__FollowSetCallers);
bool parseFixedFields(const __TokenBitSet &__FollowSetCallers);
bool parseFieldType(const __TokenBitSet &__FollowSetCallers);
bool parseVariantFields(const __TokenBitSet &__FollowSetCallers);
bool parseTagIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseTagType(const __TokenBitSet &__FollowSetCallers);
bool parseVariantList(const __TokenBitSet &__FollowSetCallers);
bool parseVariantElsePart(const __TokenBitSet &__FollowSetCallers);
bool parseVariant(const __TokenBitSet &__FollowSetCallers);
bool parseVariantLabelList(const __TokenBitSet &__FollowSetCallers);
bool parseVariantLabel(const __TokenBitSet &__FollowSetCallers);
bool parseProperProcedureBlock(const __TokenBitSet &__FollowSetCallers, bool IsFunction);
bool parseModuleBlock(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls, Block &InitBlk, Block &FinalBlk);
bool parseModuleBody(const __TokenBitSet &__FollowSetCallers, Block &InitBlk, Block &FinalBlk);
bool parseInitializationBody(const __TokenBitSet &__FollowSetCallers, Block &InitBlk);
bool parseFinalizationBody(const __TokenBitSet &__FollowSetCallers, Block &FinalBlk);
bool parseBlockBody(const __TokenBitSet &__FollowSetCallers, Block &Blk);
bool parseNormalPart(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseExceptionalPart(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S);
bool parseStatementSequence(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseAssignmentStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S);
bool parseProcedureCall(const __TokenBitSet &__FollowSetCallers, Statement *&S);
bool parseProcedureDesignator(const __TokenBitSet &__FollowSetCallers);
bool parseReturnStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S);
bool parseRetryStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S);
bool parseWithStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S);
bool parseRecordDesignator(const __TokenBitSet &__FollowSetCallers);
bool parseIfStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S);
bool parseGuardedStatements(const __TokenBitSet &__FollowSetCallers);
bool parseIfElsePart(const __TokenBitSet &__FollowSetCallers);
bool parseBooleanExpression(const __TokenBitSet &__FollowSetCallers);
bool parseCaseStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S);
bool parseCaseSelector(const __TokenBitSet &__FollowSetCallers);
bool parseCaseList(const __TokenBitSet &__FollowSetCallers);
bool parseCaseElsePart(const __TokenBitSet &__FollowSetCallers);
bool parseCaseAlternative(const __TokenBitSet &__FollowSetCallers);
bool parseCaseLabelList(const __TokenBitSet &__FollowSetCallers);
bool parseCaseLabel(const __TokenBitSet &__FollowSetCallers);
bool parseWhileStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S);
bool parseRepeatStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S);
bool parseLoopStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S);
bool parseExitStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S);
bool parseForStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S);
bool parseControlVariableIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseInitialValue(const __TokenBitSet &__FollowSetCallers);
bool parseFinalValue(const __TokenBitSet &__FollowSetCallers);
bool parseStepSize(const __TokenBitSet &__FollowSetCallers);
bool parseVariableDesignator(const __TokenBitSet &__FollowSetCallers);
bool parseEntireDesignator(const __TokenBitSet &__FollowSetCallers);
bool parseIndexedDesignator(const __TokenBitSet &__FollowSetCallers);
bool parseArrayVariableDesignator(const __TokenBitSet &__FollowSetCallers);
bool parseIndexExpression(const __TokenBitSet &__FollowSetCallers);
bool parseSelectedDesignator(const __TokenBitSet &__FollowSetCallers);
bool parseRecordVariableDesignator(const __TokenBitSet &__FollowSetCallers);
bool parseFieldIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseDereferencedDesignator(const __TokenBitSet &__FollowSetCallers);
bool parsePointerVariableDesignator(const __TokenBitSet &__FollowSetCallers);
bool parseExpression(const __TokenBitSet &__FollowSetCallers, Expression *&E);
bool parseSimpleExpression(const __TokenBitSet &__FollowSetCallers, Expression *&E);
bool parseTerm(const __TokenBitSet &__FollowSetCallers, Expression *&E);
bool parseFactor(const __TokenBitSet &__FollowSetCallers, Expression *&E);
bool parseOrdinalExpression(const __TokenBitSet &__FollowSetCallers);
bool parseRelationalOperator(const __TokenBitSet &__FollowSetCallers, OperatorInfo &Op);
bool parseTermOperator(const __TokenBitSet &__FollowSetCallers, OperatorInfo &Op);
bool parseFactorOperator(const __TokenBitSet &__FollowSetCallers, OperatorInfo &Op);
bool parseValueDesignator(const __TokenBitSet &__FollowSetCallers);
bool parseEntireValue(const __TokenBitSet &__FollowSetCallers);
bool parseIndexedValue(const __TokenBitSet &__FollowSetCallers);
bool parseArrayValue(const __TokenBitSet &__FollowSetCallers);
bool parseSelectedValue(const __TokenBitSet &__FollowSetCallers);
bool parseRecordValue(const __TokenBitSet &__FollowSetCallers);
bool parseDereferencedValue(const __TokenBitSet &__FollowSetCallers);
bool parsePointerValue(const __TokenBitSet &__FollowSetCallers);
bool parseFunctionCall(const __TokenBitSet &__FollowSetCallers);
bool parseFunctionDesignator(const __TokenBitSet &__FollowSetCallers);
bool parseValueConstructor(const __TokenBitSet &__FollowSetCallers);
bool parseArrayConstructor(const __TokenBitSet &__FollowSetCallers);
bool parseArrayTypeIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseArrayConstructedValue(const __TokenBitSet &__FollowSetCallers);
bool parseRepeatedStructureComponent(const __TokenBitSet &__FollowSetCallers);
bool parseRepetitionFactor(const __TokenBitSet &__FollowSetCallers);
bool parseStructureComponent(const __TokenBitSet &__FollowSetCallers);
bool parseRecordConstructor(const __TokenBitSet &__FollowSetCallers);
bool parseRecordTypeIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseRecordConstructedValue(const __TokenBitSet &__FollowSetCallers);
bool parseSetConstructor(const __TokenBitSet &__FollowSetCallers);
bool parseSetTypeIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseSetConstructedValue(const __TokenBitSet &__FollowSetCallers);
bool parseMember(const __TokenBitSet &__FollowSetCallers);
bool parseInterval(const __TokenBitSet &__FollowSetCallers);
bool parseSingleton(const __TokenBitSet &__FollowSetCallers);
bool parseConstantLiteral(const __TokenBitSet &__FollowSetCallers);
bool parseStringLiteral(const __TokenBitSet &__FollowSetCallers);
bool parseConstantExpression(const __TokenBitSet &__FollowSetCallers);
bool parseActualParameters(const __TokenBitSet &__FollowSetCallers);
bool parseActualParameterList(const __TokenBitSet &__FollowSetCallers);
bool parseActualParameter(const __TokenBitSet &__FollowSetCallers);
bool parseTypeParameter(const __TokenBitSet &__FollowSetCallers);
bool parseClassDefinition(const __TokenBitSet &__FollowSetCallers);
bool parseUntracedClassDefinition(const __TokenBitSet &__FollowSetCallers);
bool parseTracedClassDefinition(const __TokenBitSet &__FollowSetCallers);
bool parseNormalClassDefinition(const __TokenBitSet &__FollowSetCallers);
bool parseNormalClassHeader(const __TokenBitSet &__FollowSetCallers);
bool parseNormalClassDefinitionBody(const __TokenBitSet &__FollowSetCallers);
bool parseAbstractClassDefinition(const __TokenBitSet &__FollowSetCallers);
bool parseAbstractClassHeader(const __TokenBitSet &__FollowSetCallers);
bool parseAbstractClassDefinitionBody(const __TokenBitSet &__FollowSetCallers);
bool parseClassIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseNormalClassComponentDefinitions(const __TokenBitSet &__FollowSetCallers);
bool parseNormalComponentDefinition(const __TokenBitSet &__FollowSetCallers);
bool parseAbstractClassComponentDefinitions(const __TokenBitSet &__FollowSetCallers);
bool parseAbstractComponentDefinition(const __TokenBitSet &__FollowSetCallers);
bool parseClassVariableDeclaration(const __TokenBitSet &__FollowSetCallers);
bool parseNormalMethodDefinition(const __TokenBitSet &__FollowSetCallers);
bool parseOverridingMethodDefinition(const __TokenBitSet &__FollowSetCallers);
bool parseAbstractMethodDefinition(const __TokenBitSet &__FollowSetCallers);
bool parseClassDeclaration(const __TokenBitSet &__FollowSetCallers);
bool parseUntracedClassDeclaration(const __TokenBitSet &__FollowSetCallers);
bool parseNormalClassDeclaration(const __TokenBitSet &__FollowSetCallers);
bool parseNormalClassDeclarationBody(const __TokenBitSet &__FollowSetCallers);
bool parseAbstractClassDeclaration(const __TokenBitSet &__FollowSetCallers);
bool parseAbstractClassDeclarationBody(const __TokenBitSet &__FollowSetCallers);
bool parseClassBody(const __TokenBitSet &__FollowSetCallers);
bool parseNormalClassComponentDeclarations(const __TokenBitSet &__FollowSetCallers);
bool parseNormalComponentDeclaration(const __TokenBitSet &__FollowSetCallers);
bool parseAbstractClassComponentDeclarations(const __TokenBitSet &__FollowSetCallers);
bool parseAbstractComponentDeclaration(const __TokenBitSet &__FollowSetCallers);
bool parseNormalMethodDeclarations(const __TokenBitSet &__FollowSetCallers);
bool parseNormalMethodDeclaration(const __TokenBitSet &__FollowSetCallers);
bool parseOverridingMethodDeclaration(const __TokenBitSet &__FollowSetCallers);
bool parseAbstractMethodDeclarations(const __TokenBitSet &__FollowSetCallers);
bool parseTracedClassDeclaration(const __TokenBitSet &__FollowSetCallers);
bool parseNormalTracedClassDeclaration(const __TokenBitSet &__FollowSetCallers);
bool parseNormalTracedClassHeader(const __TokenBitSet &__FollowSetCallers);
bool parseNormalTracedClassDeclarationBody(const __TokenBitSet &__FollowSetCallers);
bool parseAbstractTracedClassDeclaration(const __TokenBitSet &__FollowSetCallers);
bool parseAbstractTracedClassHeader(const __TokenBitSet &__FollowSetCallers);
bool parseAbstractTracedClassDeclarationBody(const __TokenBitSet &__FollowSetCallers);
bool parseTracedClassBody(const __TokenBitSet &__FollowSetCallers);
bool parseRevealList(const __TokenBitSet &__FollowSetCallers);
bool parseRevealedComponentList(const __TokenBitSet &__FollowSetCallers);
bool parseRevealedComponent(const __TokenBitSet &__FollowSetCallers);
bool parseClassVariableIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseInheritClause(const __TokenBitSet &__FollowSetCallers);
bool parseClassTypeIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseObjectSelectedDesignator(const __TokenBitSet &__FollowSetCallers);
bool parseObjectVariableDesignator(const __TokenBitSet &__FollowSetCallers);
bool parseObjectSelectedValue(const __TokenBitSet &__FollowSetCallers);
bool parseObjectValueDesignator(const __TokenBitSet &__FollowSetCallers);
bool parseEntityIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseGuardStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S);
bool parseGuardSelector(const __TokenBitSet &__FollowSetCallers);
bool parseGuardedList(const __TokenBitSet &__FollowSetCallers);
bool parseGuardedStatementSequence(const __TokenBitSet &__FollowSetCallers);
bool parseGuardedClassType(const __TokenBitSet &__FollowSetCallers);
bool parseObjectDenoter(const __TokenBitSet &__FollowSetCallers);
#endif
#ifdef M2PARSER_DEFINITION
const M2Parser::__TokenBitSet M2Parser::__FollowSets[] = {
  { tok::eof },
  { tok::semi, tok::period, tok::l_square, tok::equal, tok::kw_END, tok::kw_IMPORT, tok::kw_FROM, tok::l_paren, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT },
  { tok::semi, tok::l_paren },
  { tok::kw_MODULE, tok::kw_END, tok::kw_EXPORT, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT },
  { tok::kw_MODULE, tok::kw_END, tok::kw_IMPORT, tok::kw_FROM, tok::kw_EXPORT, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT },
  { tok::kw_MODULE, tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT },
  { tok::semi, tok::period, tok::l_square, tok::r_square, tok::equal, tok::kw_END, tok::l_paren, tok::r_paren, tok::colon, tok::comma, tok::ellipsis, tok::kw_OF, tok::kw_TO, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::colonequal, tok::kw_DO, tok::kw_THEN, tok::kw_ELSIF, tok::kw_UNTIL, tok::kw_BY, tok::caret, tok::plus, tok::minus, tok::hash, tok::less, tok::greater, tok::lessequal, tok::greaterequal, tok::kw_IN, tok::kw_OR, tok::star, tok::slash, tok::kw_REM, tok::kw_DIV, tok::kw_MOD, tok::kw_AND, tok::l_brace, tok::r_brace, tok::kw_AS },
  { tok::semi },
  { tok::r_paren },
  { tok::semi, tok::r_paren },
  { tok::r_paren, tok::comma },
  { tok::kw_END },
  { tok::semi, tok::colon },
  { tok::semi, tok::kw_END, tok::pipe, tok::kw_ELSE },
  { tok::kw_END, tok::kw_BEGIN },
  { tok::colon },
  { tok::colon, tok::comma },
  { tok::r_square },
  { tok::semi, tok::kw_END, tok::comma, tok::kw_OF, tok::pipe, tok::kw_ELSE },
  { tok::semi, tok::l_square, tok::kw_END, tok::r_paren, tok::comma, tok::kw_OF, tok::pipe, tok::kw_ELSE, tok::kw_DO, tok::l_brace },
  { tok::semi, tok::l_square, tok::kw_END, tok::comma, tok::kw_OF, tok::pipe, tok::kw_ELSE },
  { tok::semi, tok::r_paren, tok::colon },
  { tok::l_square },
  { tok::semi, tok::r_paren, tok::comma },
  { tok::comma, tok::kw_OF },
  { tok::kw_END, tok::pipe, tok::kw_ELSE },
  { tok::kw_OF },
  { tok::identifier },
  { tok::kw_END, tok::kw_FINALLY },
  { tok::kw_END, tok::kw_FINALLY, tok::kw_EXCEPT },
  { tok::semi, tok::kw_END, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::kw_ELSIF, tok::kw_UNTIL },
  { tok::kw_END, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::kw_ELSIF, tok::kw_UNTIL },
  { tok::semi, tok::kw_END, tok::l_paren, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::kw_ELSIF, tok::kw_UNTIL },
  { tok::kw_DO },
  { tok::kw_END, tok::kw_ELSE },
  { tok::kw_THEN },
  { tok::colonequal },
  { tok::kw_TO },
  { tok::kw_DO, tok::kw_BY },
  { tok::period, tok::l_square, tok::r_paren, tok::comma, tok::colonequal, tok::kw_DO, tok::caret },
  { tok::r_square, tok::comma },
  { tok::period },
  { tok::semi, tok::period, tok::l_square, tok::r_square, tok::equal, tok::kw_END, tok::l_paren, tok::r_paren, tok::colon, tok::comma, tok::ellipsis, tok::kw_OF, tok::kw_TO, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::colonequal, tok::kw_DO, tok::kw_THEN, tok::kw_ELSIF, tok::kw_UNTIL, tok::kw_BY, tok::caret, tok::plus, tok::minus, tok::hash, tok::less, tok::greater, tok::lessequal, tok::greaterequal, tok::kw_IN, tok::kw_OR, tok::star, tok::slash, tok::kw_REM, tok::kw_DIV, tok::kw_MOD, tok::kw_AND, tok::r_brace, tok::kw_AS },
  { tok::caret },
  { tok::semi, tok::r_square, tok::kw_END, tok::r_paren, tok::colon, tok::comma, tok::ellipsis, tok::kw_OF, tok::kw_TO, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::kw_DO, tok::kw_THEN, tok::kw_ELSIF, tok::kw_UNTIL, tok::kw_BY, tok::r_brace, tok::kw_AS },
  { tok::semi, tok::r_square, tok::equal, tok::kw_END, tok::r_paren, tok::colon, tok::comma, tok::ellipsis, tok::kw_OF, tok::kw_TO, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::kw_DO, tok::kw_THEN, tok::kw_ELSIF, tok::kw_UNTIL, tok::kw_BY, tok::hash, tok::less, tok::greater, tok::lessequal, tok::greaterequal, tok::kw_IN, tok::r_brace, tok::kw_AS },
  { tok::semi, tok::r_square, tok::equal, tok::kw_END, tok::r_paren, tok::colon, tok::comma, tok::ellipsis, tok::kw_OF, tok::kw_TO, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::kw_DO, tok::kw_THEN, tok::kw_ELSIF, tok::kw_UNTIL, tok::kw_BY, tok::plus, tok::minus, tok::hash, tok::less, tok::greater, tok::lessequal, tok::greaterequal, tok::kw_IN, tok::kw_OR, tok::r_brace, tok::kw_AS },
  { tok::semi, tok::r_square, tok::equal, tok::kw_END, tok::r_paren, tok::colon, tok::comma, tok::ellipsis, tok::kw_OF, tok::kw_TO, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::kw_DO, tok::kw_THEN, tok::kw_ELSIF, tok::kw_UNTIL, tok::kw_BY, tok::plus, tok::minus, tok::hash, tok::less, tok::greater, tok::lessequal, tok::greaterequal, tok::kw_IN, tok::kw_OR, tok::star, tok::slash, tok::kw_REM, tok::kw_DIV, tok::kw_MOD, tok::kw_AND, tok::r_brace, tok::kw_AS },
  { tok::r_square, tok::comma, tok::ellipsis, tok::kw_OF, tok::kw_TO, tok::kw_DO, tok::kw_BY, tok::r_brace },
  { tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT },
  { tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::kw_NOT },
  { tok::semi, tok::period, tok::l_square, tok::r_square, tok::equal, tok::kw_END, tok::l_paren, tok::r_paren, tok::colon, tok::comma, tok::ellipsis, tok::kw_OF, tok::kw_TO, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::kw_DO, tok::kw_THEN, tok::kw_ELSIF, tok::kw_UNTIL, tok::kw_BY, tok::caret, tok::plus, tok::minus, tok::hash, tok::less, tok::greater, tok::lessequal, tok::greaterequal, tok::kw_IN, tok::kw_OR, tok::star, tok::slash, tok::kw_REM, tok::kw_DIV, tok::kw_MOD, tok::kw_AND, tok::r_brace, tok::kw_AS },
  { tok::l_paren },
  { tok::l_brace },
  { tok::comma, tok::r_brace },
  { tok::comma, tok::kw_BY, tok::r_brace },
  { tok::r_square, tok::r_paren, tok::colon, tok::comma, tok::ellipsis, tok::kw_DO, tok::r_brace },
  { tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_FORWARD, tok::kw_BEGIN, tok::kw_OVERRIDE, tok::kw_REVEAL, tok::kw_INHERIT },
  { tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_FORWARD, tok::kw_BEGIN, tok::kw_ABSTRACT, tok::kw_OVERRIDE, tok::kw_REVEAL, tok::kw_INHERIT },
  { tok::identifier, tok::semi, tok::period },
  { tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_OVERRIDE },
  { tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_ABSTRACT, tok::kw_OVERRIDE },
  { tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_OVERRIDE },
  { tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_ABSTRACT, tok::kw_OVERRIDE },
  { tok::semi, tok::comma },
  { tok::semi, tok::period, tok::l_square, tok::r_paren, tok::comma, tok::colonequal, tok::kw_DO, tok::caret },
  { tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_ABSTRACT, tok::kw_OVERRIDE, tok::kw_REVEAL },
  { tok::semi, tok::kw_DO },
  { tok::kw_AS },
};
bool M2Parser::__skipUntil(const M2Parser::__TokenBitSet &ActiveSets, const M2Parser::__TokenBitSet &CurrentSet) {
  M2Parser::__TokenBitSet StopSets = ActiveSets | tok::eof;
  while (!StopSets.contains(Tok.getKind())) {
    advance();
  }
  return CurrentSet.contains(Tok.getKind());
}
bool M2Parser::parseCompilationModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  if (Tok.is(tok::kw_UNSAFEGUARDED)) {
    advance();
    if (Tok.is(tok::kw_MODULE)) {
      if (M2Parser::parseProgramModule(__FollowSet, CM, true))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_DEFINITION)) {
      if (M2Parser::parseDefinitionModule(__FollowSet, CM, true))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_IMPLEMENTATION)) {
      if (M2Parser::parseImplementationModule(__FollowSet, CM, true))
        return __errorhandler();
    }
    else {
      error();
      return __errorhandler();
    }
  }
  else if (Tok.is(tok::kw_GENERIC)) {
    advance();
    if (Tok.is(tok::kw_DEFINITION)) {
      if (M2Parser::parseGenericDefinitionModule(__FollowSet, CM))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_IMPLEMENTATION)) {
      if (M2Parser::parseGenericImplementationModule(__FollowSet, CM))
        return __errorhandler();
    }
    else {
      error();
      return __errorhandler();
    }
  }
  else if (Tok.is(tok::kw_MODULE)) {
    if (M2Parser::parseProgramModule(__FollowSet, CM, false))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_DEFINITION)) {
    if (M2Parser::parseDefinitionModule(__FollowSet, CM, false))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_IMPLEMENTATION)) {
    if (M2Parser::parseImplementationModule(__FollowSet, CM, false))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseProgramModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool HasUnsafeGuarded) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  if (consume(tok::kw_MODULE))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  ProgramModule *PM = Actions.actOnProgramModule(Tok.getLocation(), Tok.getIdentifier());
  EnterDeclScope S(Actions, PM);
  DeclarationList Decls; Block InitBlk, FinalBlk;
  Expression *ProtectionExpr = nullptr;
  advance();
  if (Tok.is(tok::l_square)) {
    if (M2Parser::parseProtection(__FollowSet, ProtectionExpr))
      return __errorhandler();
  }
  if (consume(tok::semi))
    return __errorhandler();
  if (M2Parser::parseImportLists(__FollowSet))
    return __errorhandler();
  if (M2Parser::parseModuleBlock(__FollowSet, Decls, InitBlk, FinalBlk))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  Actions.actOnProgramModule(PM, Tok.getLocation(), Tok.getIdentifier(), Decls, InitBlk, FinalBlk);
  advance();
  if (expect(tok::period))
    return __errorhandler();
  CM = PM;
  advance();
  return false;
}
bool M2Parser::parseModuleIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[1] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[1]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool M2Parser::parseProtection(const __TokenBitSet &__FollowSetCallers, Expression *&Expr) {
  const __TokenBitSet __FollowSet = __FollowSets[2] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[2]);
  };
  if (consume(tok::l_square))
    return __errorhandler();
  if (M2Parser::parseExpression(__FollowSet, Expr))
    return __errorhandler();
  if (consume(tok::r_square))
    return __errorhandler();
  return false;
}
bool M2Parser::parseDefinitionModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool HasUnsafeGuarded) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  if (consume(tok::kw_DEFINITION))
    return __errorhandler();
  if (consume(tok::kw_MODULE))
    return __errorhandler();
  if (M2Parser::parseModuleIdentifier(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::equal) && (!HasUnsafeGuarded && getLangOpts().ISOGenerics)) {
    advance();
    if (M2Parser::parseGenericSeparateModuleIdentifier(__FollowSet))
      return __errorhandler();
    if (Tok.is(tok::l_paren)) {
      if (M2Parser::parseActualModuleParameters(__FollowSet))
        return __errorhandler();
    }
    if (consume(tok::semi))
      return __errorhandler();
  }
  else if ((__TokenBitSet{tok::kw_IMPORT, tok::kw_FROM, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT, }).contains(Tok.getKind())) {
    if (M2Parser::parseImportLists(__FollowSet))
      return __errorhandler();
    if (M2Parser::parseDefinitions(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::kw_END))
    return __errorhandler();
  if (M2Parser::parseModuleIdentifier(__FollowSet))
    return __errorhandler();
  if (consume(tok::period))
    return __errorhandler();
  return false;
}
bool M2Parser::parseImplementationModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool HasUnsafeGuarded) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  DeclarationList Decls; Block InitBlk, FinalBlk;
  Expression *ProtectionExpr = nullptr;
  if (consume(tok::kw_IMPLEMENTATION))
    return __errorhandler();
  if (consume(tok::kw_MODULE))
    return __errorhandler();
  if (M2Parser::parseModuleIdentifier(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::equal)) {
    if (!(!HasUnsafeGuarded && getLangOpts().ISOGenerics)) {
      error();
      return __errorhandler();
    }
    advance();
    if (M2Parser::parseGenericSeparateModuleIdentifier(__FollowSet))
      return __errorhandler();
    if (Tok.is(tok::l_paren)) {
      if (M2Parser::parseActualModuleParameters(__FollowSet))
        return __errorhandler();
    }
    if (consume(tok::semi))
      return __errorhandler();
    if (consume(tok::kw_END))
      return __errorhandler();
  }
  else if ((__TokenBitSet{tok::semi, tok::l_square, }).contains(Tok.getKind())) {
    if (Tok.is(tok::l_square)) {
      if (M2Parser::parseProtection(__FollowSet, ProtectionExpr))
        return __errorhandler();
    }
    if (consume(tok::semi))
      return __errorhandler();
    if (M2Parser::parseImportLists(__FollowSet))
      return __errorhandler();
    if (M2Parser::parseModuleBlock(__FollowSet, Decls, InitBlk, FinalBlk))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  if (M2Parser::parseModuleIdentifier(__FollowSet))
    return __errorhandler();
  if (consume(tok::period))
    return __errorhandler();
  return false;
}
bool M2Parser::parseImportLists(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[3] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[3]);
  };
  while ((__TokenBitSet{tok::kw_IMPORT, tok::kw_FROM, }).contains(Tok.getKind())) {
    if (M2Parser::parseImportList(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseImportList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[4] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[4]);
  };
  if (Tok.is(tok::kw_IMPORT)) {
    if (M2Parser::parseSimpleImport(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_FROM)) {
    if (M2Parser::parseUnqualifiedImport(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseSimpleImport(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[4] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[4]);
  };
  if (consume(tok::kw_IMPORT))
    return __errorhandler();
  if (M2Parser::parseIdentifierList(__FollowSet))
    return __errorhandler();
  if (consume(tok::semi))
    return __errorhandler();
  return false;
}
bool M2Parser::parseUnqualifiedImport(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[4] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[4]);
  };
  if (consume(tok::kw_FROM))
    return __errorhandler();
  if (M2Parser::parseModuleIdentifier(__FollowSet))
    return __errorhandler();
  if (consume(tok::kw_IMPORT))
    return __errorhandler();
  if (M2Parser::parseIdentifierList(__FollowSet))
    return __errorhandler();
  if (consume(tok::semi))
    return __errorhandler();
  return false;
}
bool M2Parser::parseExportList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[5] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[5]);
  };
  if (consume(tok::kw_EXPORT))
    return __errorhandler();
  if (Tok.is(tok::kw_QUALIFIED)) {
    advance();
  }
  if (M2Parser::parseIdentifierList(__FollowSet))
    return __errorhandler();
  if (consume(tok::semi))
    return __errorhandler();
  return false;
}
bool M2Parser::parseQualifiedIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[6] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[6]);
  };
  while (Tok.is(tok::identifier) && (Actions.isModule(Tok.getIdentifier()))) {
    if (M2Parser::parseModuleIdentifier(__FollowSet))
      return __errorhandler();
    if (consume(tok::period))
      return __errorhandler();
  }
  if (Tok.is(tok::identifier) && (getLangOpts().ISOObjects && Actions.isClass(Tok.getIdentifier()))) {
    if (M2Parser::parseClassIdentifier(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool M2Parser::parseGenericDefinitionModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  if (consume(tok::kw_DEFINITION))
    return __errorhandler();
  if (consume(tok::kw_MODULE))
    return __errorhandler();
  if (M2Parser::parseModuleIdentifier(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::l_paren)) {
    if (M2Parser::parseFormalModuleParameters(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::semi))
    return __errorhandler();
  if (M2Parser::parseImportLists(__FollowSet))
    return __errorhandler();
  if (M2Parser::parseDefinitions(__FollowSet))
    return __errorhandler();
  if (consume(tok::kw_END))
    return __errorhandler();
  if (M2Parser::parseModuleIdentifier(__FollowSet))
    return __errorhandler();
  if (consume(tok::period))
    return __errorhandler();
  return false;
}
bool M2Parser::parseGenericImplementationModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  DeclarationList Decls; Block InitBlk, FinalBlk;
  Expression *ProtectionExpr = nullptr;
  if (consume(tok::kw_IMPLEMENTATION))
    return __errorhandler();
  if (consume(tok::kw_MODULE))
    return __errorhandler();
  if (M2Parser::parseModuleIdentifier(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::l_square)) {
    if (M2Parser::parseProtection(__FollowSet, ProtectionExpr))
      return __errorhandler();
  }
  if (Tok.is(tok::l_paren)) {
    if (M2Parser::parseFormalModuleParameters(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::semi))
    return __errorhandler();
  if (M2Parser::parseImportLists(__FollowSet))
    return __errorhandler();
  if (M2Parser::parseModuleBlock(__FollowSet, Decls, InitBlk, FinalBlk))
    return __errorhandler();
  if (M2Parser::parseModuleIdentifier(__FollowSet))
    return __errorhandler();
  if (consume(tok::period))
    return __errorhandler();
  return false;
}
bool M2Parser::parseGenericSeparateModuleIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[2] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[2]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool M2Parser::parseFormalModuleParameters(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::l_paren))
    return __errorhandler();
  if (M2Parser::parseFormalModuleParameterList(__FollowSet))
    return __errorhandler();
  if (consume(tok::r_paren))
    return __errorhandler();
  return false;
}
bool M2Parser::parseFormalModuleParameterList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[8] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[8]);
  };
  if (M2Parser::parseFormalModuleParameter(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::semi)) {
    advance();
    if (M2Parser::parseFormalModuleParameter(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseFormalModuleParameter(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[9] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[9]);
  };
  if (Tok.is(tok::identifier)) {
    if (M2Parser::parseConstantValueParameterSpecification(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseTypeParameterSpecification(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseConstantValueParameterSpecification(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[9] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[9]);
  };
  if (M2Parser::parseIdentifierList(__FollowSet))
    return __errorhandler();
  if (consume(tok::colon))
    return __errorhandler();
  if (M2Parser::parseFormalType(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseTypeParameterSpecification(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[9] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[9]);
  };
  if (M2Parser::parseIdentifierList(__FollowSet))
    return __errorhandler();
  if (consume(tok::colon))
    return __errorhandler();
  if (consume(tok::kw_TYPE))
    return __errorhandler();
  return false;
}
bool M2Parser::parseActualModuleParameters(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::l_paren))
    return __errorhandler();
  if (M2Parser::parseActualModuleParameterList(__FollowSet))
    return __errorhandler();
  if (consume(tok::r_paren))
    return __errorhandler();
  return false;
}
bool M2Parser::parseActualModuleParameterList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[8] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[8]);
  };
  if (M2Parser::parseActualModuleParameter(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (M2Parser::parseActualModuleParameter(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseActualModuleParameter(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[10] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[10]);
  };
  if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, }).contains(Tok.getKind())) {
    if (M2Parser::parseConstantExpression(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseTypeParameter(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseDefinitions(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[11] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[11]);
  };
  DeclarationList Decls;
  while ((__TokenBitSet{tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT, }).contains(Tok.getKind())) {
    if (Tok.is(tok::kw_CONST)) {
      advance();
      while (Tok.is(tok::identifier)) {
        if (M2Parser::parseConstantDeclaration(__FollowSet, Decls))
          return __errorhandler();
        if (consume(tok::semi))
          return __errorhandler();
      }
    }
    else if (Tok.is(tok::kw_TYPE)) {
      advance();
      while (Tok.is(tok::identifier)) {
        if (M2Parser::parseTypeDefinition(__FollowSet, Decls))
          return __errorhandler();
        if (consume(tok::semi))
          return __errorhandler();
      }
    }
    else if (Tok.is(tok::kw_VAR)) {
      advance();
      while (Tok.is(tok::identifier)) {
        if (M2Parser::parseVariableDeclaration(__FollowSet, Decls))
          return __errorhandler();
        if (consume(tok::semi))
          return __errorhandler();
      }
    }
    else if (Tok.is(tok::kw_PROCEDURE)) {
      if (M2Parser::parseProcedureHeading(__FollowSet))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
    else if ((__TokenBitSet{tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT, }).contains(Tok.getKind()) && (getLangOpts().ISOObjects)) {
      if (M2Parser::parseClassDefinition(__FollowSet))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
  }
  return false;
}
bool M2Parser::parseProcedureHeading(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::kw_PROCEDURE))
    return __errorhandler();
  if (M2Parser::parseProcedureIdentifier(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::l_paren)) {
    if (M2Parser::parseFormalParameters(__FollowSet))
      return __errorhandler();
    if (Tok.is(tok::colon)) {
      advance();
      if (M2Parser::parseFunctionResultType(__FollowSet))
        return __errorhandler();
    }
  }
  return false;
}
bool M2Parser::parseTypeDefinition(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (Tok.is(tok::identifier)) {
    if (M2Parser::parseTypeDeclaration(__FollowSet, Decls))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseOpaqueTypeDefinition(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseOpaqueTypeDefinition(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool M2Parser::parseFormalParameters(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[12] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[12]);
  };
  if (consume(tok::l_paren))
    return __errorhandler();
  if ((__TokenBitSet{tok::identifier, tok::kw_VAR, }).contains(Tok.getKind())) {
    if (M2Parser::parseFormalParameterList(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::r_paren))
    return __errorhandler();
  return false;
}
bool M2Parser::parseFormalParameterList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[8] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[8]);
  };
  if (M2Parser::parseFormalParameter(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::semi)) {
    advance();
    if (M2Parser::parseFormalParameter(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseFunctionResultType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if (M2Parser::parseTypeIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseFormalParameter(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[9] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[9]);
  };
  if (Tok.is(tok::identifier)) {
    if (M2Parser::parseValueParameterSpecification(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_VAR)) {
    if (M2Parser::parseVariableParameterSpecification(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseValueParameterSpecification(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[9] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[9]);
  };
  if (M2Parser::parseIdentifierList(__FollowSet))
    return __errorhandler();
  if (consume(tok::colon))
    return __errorhandler();
  if (M2Parser::parseFormalType(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseVariableParameterSpecification(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[9] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[9]);
  };
  if (consume(tok::kw_VAR))
    return __errorhandler();
  if (M2Parser::parseIdentifierList(__FollowSet))
    return __errorhandler();
  if (consume(tok::colon))
    return __errorhandler();
  if (M2Parser::parseFormalType(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseDeclarations(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[14] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[14]);
  };
  DeclarationList Decls;
  while ((__TokenBitSet{tok::kw_MODULE, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT, }).contains(Tok.getKind())) {
    if (Tok.is(tok::kw_CONST)) {
      advance();
      while (Tok.is(tok::identifier)) {
        if (M2Parser::parseConstantDeclaration(__FollowSet, Decls))
          return __errorhandler();
        if (consume(tok::semi))
          return __errorhandler();
      }
    }
    else if (Tok.is(tok::kw_TYPE)) {
      advance();
      while (Tok.is(tok::identifier)) {
        if (M2Parser::parseTypeDeclaration(__FollowSet, Decls))
          return __errorhandler();
        if (consume(tok::semi))
          return __errorhandler();
      }
    }
    else if (Tok.is(tok::kw_VAR)) {
      advance();
      while (Tok.is(tok::identifier)) {
        if (M2Parser::parseVariableDeclaration(__FollowSet, Decls))
          return __errorhandler();
        if (consume(tok::semi))
          return __errorhandler();
      }
    }
    else if (Tok.is(tok::kw_PROCEDURE)) {
      if (M2Parser::parseProcedureDeclaration(__FollowSet))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
    else if ((__TokenBitSet{tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT, }).contains(Tok.getKind()) && (getLangOpts().ISOObjects)) {
      if (M2Parser::parseClassDeclaration(__FollowSet))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_MODULE)) {
      if (M2Parser::parseLocalModuleDeclaration(__FollowSet))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
  }
  return false;
}
bool M2Parser::parseConstantDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  SMLoc Loc; StringRef Name;
  if (expect(tok::identifier))
    return __errorhandler();
  Loc = Tok.getLocation(); Name = Tok.getIdentifier();
  advance();
  if (expect(tok::equal))
    return __errorhandler();
  Expression *E = nullptr;
  advance();
  if (M2Parser::parseExpression(__FollowSet, E))
    return __errorhandler();
  Actions.actOnConstant(Decls, Loc, Name, E);
  return false;
}
bool M2Parser::parseTypeDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  SMLoc Loc; StringRef Name;
  if (expect(tok::identifier))
    return __errorhandler();
  Identifier TypeName = fromToken<Identifier>(Tok);
  advance();
  if (consume(tok::equal))
    return __errorhandler();
  if (M2Parser::parseTypeDenoter(__FollowSet))
    return __errorhandler();
  Actions.actOnType(Decls, TypeName);
  return false;
}
bool M2Parser::parseVariableDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (M2Parser::parseVariableIdentifierList(__FollowSet))
    return __errorhandler();
  if (consume(tok::colon))
    return __errorhandler();
  if (M2Parser::parseTypeDenoter(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseVariableIdentifierList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[15] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[15]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  if (Tok.is(tok::l_square)) {
    if (M2Parser::parseMachineAddress(__FollowSet))
      return __errorhandler();
  }
  while (Tok.is(tok::comma)) {
    advance();
    if (consume(tok::identifier))
      return __errorhandler();
    if (Tok.is(tok::l_square)) {
      if (M2Parser::parseMachineAddress(__FollowSet))
        return __errorhandler();
    }
  }
  return false;
}
bool M2Parser::parseMachineAddress(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[16] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[16]);
  };
  if (consume(tok::l_square))
    return __errorhandler();
  if (M2Parser::parseValueOfAddressType(__FollowSet))
    return __errorhandler();
  if (consume(tok::r_square))
    return __errorhandler();
  return false;
}
bool M2Parser::parseValueOfAddressType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[17] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[17]);
  };
  if (M2Parser::parseConstantExpression(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseProcedureDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::kw_PROCEDURE))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  Procedure *P = Actions.actOnProcedure(Tok.getLocation(), Tok.getIdentifier());
  EnterDeclScope S(Actions, P);
  bool IsFunction = false;
  advance();
  if (Tok.is(tok::l_paren)) {
    advance();
    if ((__TokenBitSet{tok::identifier, tok::kw_VAR, }).contains(Tok.getKind())) {
      if (M2Parser::parseFormalParameterList(__FollowSet))
        return __errorhandler();
    }
    if (consume(tok::r_paren))
      return __errorhandler();
    if (Tok.is(tok::colon)) {
      IsFunction=true;
      advance();
      if (M2Parser::parseFunctionResultType(__FollowSet))
        return __errorhandler();
    }
  }
  if (consume(tok::semi))
    return __errorhandler();
  if ((__TokenBitSet{tok::kw_MODULE, tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT, }).contains(Tok.getKind())) {
    if (M2Parser::parseProperProcedureBlock(__FollowSet, IsFunction))
      return __errorhandler();
    if (expect(tok::identifier))
      return __errorhandler();
    Actions.actOnProcedure(Tok.getLocation(), Tok.getIdentifier());
    advance();
  }
  else if (Tok.is(tok::kw_FORWARD)) {
    Actions.actOnForwardProcedure(P);
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseProcedureIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[2] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[2]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool M2Parser::parseLocalModuleDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::kw_MODULE))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  LocalModule *LM = Actions.actOnLocalModule(Tok.getLocation(), Tok.getIdentifier());
  EnterDeclScope S(Actions, LM);
  DeclarationList Decls; Block InitBlk, FinalBlk;
  Expression *ProtectionExpr = nullptr;
  advance();
  if (Tok.is(tok::equal)) {
    if (!(getLangOpts().ISOGenerics)) {
      error();
      return __errorhandler();
    }
    advance();
    if (M2Parser::parseGenericSeparateModuleIdentifier(__FollowSet))
      return __errorhandler();
    if (Tok.is(tok::l_paren)) {
      if (M2Parser::parseActualModuleParameters(__FollowSet))
        return __errorhandler();
    }
    if (consume(tok::semi))
      return __errorhandler();
    if (Tok.is(tok::kw_EXPORT)) {
      if (M2Parser::parseExportList(__FollowSet))
        return __errorhandler();
    }
    if (consume(tok::kw_END))
      return __errorhandler();
  }
  else if ((__TokenBitSet{tok::semi, tok::l_square, }).contains(Tok.getKind())) {
    if (Tok.is(tok::l_square)) {
      if (M2Parser::parseProtection(__FollowSet, ProtectionExpr))
        return __errorhandler();
    }
    if (consume(tok::semi))
      return __errorhandler();
    if (M2Parser::parseImportLists(__FollowSet))
      return __errorhandler();
    if (Tok.is(tok::kw_EXPORT)) {
      if (M2Parser::parseExportList(__FollowSet))
        return __errorhandler();
    }
    if (M2Parser::parseModuleBlock(__FollowSet, Decls, InitBlk, FinalBlk))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  if (M2Parser::parseModuleIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseTypeDenoter(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if (Tok.is(tok::identifier)) {
    if (M2Parser::parseTypeIdentifier(__FollowSet))
      return __errorhandler();
  }
  else if ((__TokenBitSet{tok::identifier, tok::l_square, tok::l_paren, tok::kw_PROCEDURE, tok::kw_SET, tok::kw_PACKEDSET, tok::kw_POINTER, tok::kw_ARRAY, tok::kw_RECORD, }).contains(Tok.getKind())) {
    if (M2Parser::parseNewType(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseOrdinalTypeDenoter(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[18] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[18]);
  };
  if (Tok.is(tok::identifier)) {
    if (M2Parser::parseOrdinalTypeIdentifier(__FollowSet))
      return __errorhandler();
  }
  else if ((__TokenBitSet{tok::identifier, tok::l_square, tok::l_paren, }).contains(Tok.getKind())) {
    if (M2Parser::parseNewOrdinalType(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseTypeIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[19] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[19]);
  };
  if (M2Parser::parseQualifiedIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseOrdinalTypeIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[20] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[20]);
  };
  if (M2Parser::parseTypeIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseNewType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if ((__TokenBitSet{tok::identifier, tok::l_square, tok::l_paren, }).contains(Tok.getKind())) {
    if (M2Parser::parseNewOrdinalType(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_SET)) {
    if (M2Parser::parseSetType(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_PACKEDSET)) {
    if (M2Parser::parsePackedsetType(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_POINTER)) {
    if (M2Parser::parsePointerType(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_PROCEDURE)) {
    if (M2Parser::parseProcedureType(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_ARRAY)) {
    if (M2Parser::parseArrayType(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_RECORD)) {
    if (M2Parser::parseRecordType(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseNewOrdinalType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[18] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[18]);
  };
  if (Tok.is(tok::l_paren)) {
    if (M2Parser::parseEnumerationType(__FollowSet))
      return __errorhandler();
  }
  else if ((__TokenBitSet{tok::identifier, tok::l_square, }).contains(Tok.getKind())) {
    if (M2Parser::parseSubrangeType(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseEnumerationType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[18] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[18]);
  };
  if (consume(tok::l_paren))
    return __errorhandler();
  if (M2Parser::parseIdentifierList(__FollowSet))
    return __errorhandler();
  if (consume(tok::r_paren))
    return __errorhandler();
  return false;
}
bool M2Parser::parseIdentifierList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[21] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[21]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (consume(tok::identifier))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseSubrangeType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[18] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[18]);
  };
  if (Tok.is(tok::identifier)) {
    if (M2Parser::parseRangeType(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::l_square))
    return __errorhandler();
  if (M2Parser::parseConstantExpression(__FollowSet))
    return __errorhandler();
  if (consume(tok::ellipsis))
    return __errorhandler();
  if (M2Parser::parseConstantExpression(__FollowSet))
    return __errorhandler();
  if (consume(tok::r_square))
    return __errorhandler();
  return false;
}
bool M2Parser::parseRangeType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[22] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[22]);
  };
  if (M2Parser::parseOrdinalTypeIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseSetType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if (consume(tok::kw_SET))
    return __errorhandler();
  if (consume(tok::kw_OF))
    return __errorhandler();
  if (M2Parser::parseBaseType(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseBaseType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if (M2Parser::parseOrdinalTypeDenoter(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parsePackedsetType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if (consume(tok::kw_PACKEDSET))
    return __errorhandler();
  if (consume(tok::kw_OF))
    return __errorhandler();
  if (M2Parser::parseBaseType(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parsePointerType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if (consume(tok::kw_POINTER))
    return __errorhandler();
  if (consume(tok::kw_TO))
    return __errorhandler();
  if (M2Parser::parseBoundType(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseBoundType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if (M2Parser::parseTypeDenoter(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseProcedureType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if (consume(tok::kw_PROCEDURE))
    return __errorhandler();
  if (Tok.is(tok::l_paren)) {
    advance();
    if ((__TokenBitSet{tok::identifier, tok::kw_VAR, tok::kw_ARRAY, }).contains(Tok.getKind())) {
      if (M2Parser::parseFormalParameterTypeList(__FollowSet))
        return __errorhandler();
    }
    if (consume(tok::r_paren))
      return __errorhandler();
    if (Tok.is(tok::colon)) {
      advance();
      if (M2Parser::parseFunctionResultType(__FollowSet))
        return __errorhandler();
    }
  }
  return false;
}
bool M2Parser::parseFormalParameterTypeList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[8] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[8]);
  };
  if (M2Parser::parseFormalParameterType(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (M2Parser::parseFormalParameterType(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseFormalParameterType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[10] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[10]);
  };
  if (Tok.is(tok::kw_VAR)) {
    if (M2Parser::parseVariableFormalType(__FollowSet))
      return __errorhandler();
  }
  else if ((__TokenBitSet{tok::identifier, tok::kw_ARRAY, }).contains(Tok.getKind())) {
    if (M2Parser::parseValueFormalType(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseVariableFormalType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[10] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[10]);
  };
  if (consume(tok::kw_VAR))
    return __errorhandler();
  if (M2Parser::parseFormalType(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseValueFormalType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[10] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[10]);
  };
  if (M2Parser::parseFormalType(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseFormalType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[23] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[23]);
  };
  if (Tok.is(tok::identifier)) {
    if (M2Parser::parseTypeIdentifier(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_ARRAY)) {
    if (M2Parser::parseOpenArrayFormalType(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseOpenArrayFormalType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[23] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[23]);
  };
  if (consume(tok::kw_ARRAY))
    return __errorhandler();
  if (consume(tok::kw_OF))
    return __errorhandler();
  while (Tok.is(tok::kw_ARRAY)) {
    advance();
    if (consume(tok::kw_OF))
      return __errorhandler();
  }
  if (M2Parser::parseTypeIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseArrayType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if (consume(tok::kw_ARRAY))
    return __errorhandler();
  if (M2Parser::parseIndexType(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (M2Parser::parseIndexType(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::kw_OF))
    return __errorhandler();
  if (M2Parser::parseComponentType(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseIndexType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[24] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[24]);
  };
  if (M2Parser::parseOrdinalTypeDenoter(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseComponentType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if (M2Parser::parseTypeDenoter(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseRecordType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if (consume(tok::kw_RECORD))
    return __errorhandler();
  if (M2Parser::parseFieldList(__FollowSet))
    return __errorhandler();
  if (consume(tok::kw_END))
    return __errorhandler();
  return false;
}
bool M2Parser::parseFieldList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[25] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[25]);
  };
  if (M2Parser::parseFields(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::semi)) {
    advance();
    if (M2Parser::parseFields(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseFields(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if ((__TokenBitSet{tok::identifier, tok::kw_CASE, }).contains(Tok.getKind())) {
    if (Tok.is(tok::identifier)) {
      if (M2Parser::parseFixedFields(__FollowSet))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_CASE)) {
      if (M2Parser::parseVariantFields(__FollowSet))
        return __errorhandler();
    }
  }
  return false;
}
bool M2Parser::parseFixedFields(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if (M2Parser::parseIdentifierList(__FollowSet))
    return __errorhandler();
  if (consume(tok::colon))
    return __errorhandler();
  if (M2Parser::parseFieldType(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseFieldType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if (M2Parser::parseTypeDenoter(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseVariantFields(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  if (consume(tok::kw_CASE))
    return __errorhandler();
  if (Tok.is(tok::identifier)) {
    if (M2Parser::parseTagIdentifier(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::colon))
    return __errorhandler();
  if (M2Parser::parseTagType(__FollowSet))
    return __errorhandler();
  if (consume(tok::kw_OF))
    return __errorhandler();
  if (M2Parser::parseVariantList(__FollowSet))
    return __errorhandler();
  if (consume(tok::kw_END))
    return __errorhandler();
  return false;
}
bool M2Parser::parseTagIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[15] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[15]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool M2Parser::parseTagType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[26] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[26]);
  };
  if (M2Parser::parseOrdinalTypeIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseVariantList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[11] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[11]);
  };
  if (M2Parser::parseVariant(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::pipe)) {
    advance();
    if (M2Parser::parseVariant(__FollowSet))
      return __errorhandler();
  }
  if (Tok.is(tok::kw_ELSE)) {
    if (M2Parser::parseVariantElsePart(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseVariantElsePart(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[11] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[11]);
  };
  if (consume(tok::kw_ELSE))
    return __errorhandler();
  if (M2Parser::parseFieldList(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseVariant(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[25] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[25]);
  };
  if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, }).contains(Tok.getKind())) {
    if (M2Parser::parseVariantLabelList(__FollowSet))
      return __errorhandler();
    if (consume(tok::colon))
      return __errorhandler();
    if (M2Parser::parseFieldList(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseVariantLabelList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[15] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[15]);
  };
  if (M2Parser::parseVariantLabel(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (M2Parser::parseVariantLabel(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseVariantLabel(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[16] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[16]);
  };
  if (M2Parser::parseConstantExpression(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::ellipsis)) {
    advance();
    if (M2Parser::parseConstantExpression(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseProperProcedureBlock(const __TokenBitSet &__FollowSetCallers, bool IsFunction) {
  const __TokenBitSet __FollowSet = __FollowSets[27] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[27]);
  };
  Block Body;
  if (M2Parser::parseDeclarations(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::kw_BEGIN)) {
    advance();
    if (M2Parser::parseBlockBody(__FollowSet, Body))
      return __errorhandler();
  }
  else if ((__TokenBitSet{}).contains(Tok.getKind()) && (IsFunction)) {
  }
  if (consume(tok::kw_END))
    return __errorhandler();
  return false;
}
bool M2Parser::parseModuleBlock(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls, Block &InitBlk, Block &FinalBlk) {
  const __TokenBitSet __FollowSet = __FollowSets[27] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[27]);
  };
  if (M2Parser::parseDeclarations(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::kw_BEGIN)) {
    if (M2Parser::parseModuleBody(__FollowSet, InitBlk, FinalBlk))
      return __errorhandler();
  }
  if (consume(tok::kw_END))
    return __errorhandler();
  return false;
}
bool M2Parser::parseModuleBody(const __TokenBitSet &__FollowSetCallers, Block &InitBlk, Block &FinalBlk) {
  const __TokenBitSet __FollowSet = __FollowSets[11] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[11]);
  };
  if (M2Parser::parseInitializationBody(__FollowSet, InitBlk))
    return __errorhandler();
  if (Tok.is(tok::kw_FINALLY)) {
    if (M2Parser::parseFinalizationBody(__FollowSet, FinalBlk))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseInitializationBody(const __TokenBitSet &__FollowSetCallers, Block &InitBlk) {
  const __TokenBitSet __FollowSet = __FollowSets[28] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[28]);
  };
  if (consume(tok::kw_BEGIN))
    return __errorhandler();
  if (M2Parser::parseBlockBody(__FollowSet, InitBlk))
    return __errorhandler();
  return false;
}
bool M2Parser::parseFinalizationBody(const __TokenBitSet &__FollowSetCallers, Block &FinalBlk) {
  const __TokenBitSet __FollowSet = __FollowSets[11] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[11]);
  };
  if (consume(tok::kw_FINALLY))
    return __errorhandler();
  if (M2Parser::parseBlockBody(__FollowSet, FinalBlk))
    return __errorhandler();
  return false;
}
bool M2Parser::parseBlockBody(const __TokenBitSet &__FollowSetCallers, Block &Blk) {
  const __TokenBitSet __FollowSet = __FollowSets[28] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[28]);
  };
  StatementList Stmts, ExceptStmts;
  if (M2Parser::parseNormalPart(__FollowSet, Stmts))
    return __errorhandler();
  if (Tok.is(tok::kw_EXCEPT)) {
    advance();
    if (M2Parser::parseExceptionalPart(__FollowSet, Stmts))
      return __errorhandler();
  }
  Blk = Block(Stmts, ExceptStmts);
  return false;
}
bool M2Parser::parseNormalPart(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[29] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[29]);
  };
  if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
    return __errorhandler();
  return false;
}
bool M2Parser::parseExceptionalPart(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[28] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[28]);
  };
  if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
    return __errorhandler();
  return false;
}
bool M2Parser::parseStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  if ((__TokenBitSet{tok::identifier, tok::kw_CASE, tok::kw_RETURN, tok::kw_RETRY, tok::kw_WITH, tok::kw_IF, tok::kw_WHILE, tok::kw_REPEAT, tok::kw_LOOP, tok::kw_EXIT, tok::kw_FOR, tok::kw_GUARD, }).contains(Tok.getKind())) {
    if (Tok.is(tok::identifier)) {
      if (M2Parser::parseAssignmentStatement(__FollowSet, S))
        return __errorhandler();
    }
    else if (Tok.is(tok::identifier)) {
      if (M2Parser::parseProcedureCall(__FollowSet, S))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_RETURN)) {
      if (M2Parser::parseReturnStatement(__FollowSet, S))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_RETRY)) {
      if (M2Parser::parseRetryStatement(__FollowSet, S))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_WITH)) {
      if (M2Parser::parseWithStatement(__FollowSet, S))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_IF)) {
      if (M2Parser::parseIfStatement(__FollowSet, S))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_CASE)) {
      if (M2Parser::parseCaseStatement(__FollowSet, S))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_WHILE)) {
      if (M2Parser::parseWhileStatement(__FollowSet, S))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_REPEAT)) {
      if (M2Parser::parseRepeatStatement(__FollowSet, S))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_LOOP)) {
      if (M2Parser::parseLoopStatement(__FollowSet, S))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_EXIT)) {
      if (M2Parser::parseExitStatement(__FollowSet, S))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_FOR)) {
      if (M2Parser::parseForStatement(__FollowSet, S))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_GUARD) && (getLangOpts().ISOObjects)) {
      if (M2Parser::parseGuardStatement(__FollowSet, S))
        return __errorhandler();
    }
  }
  return false;
}
bool M2Parser::parseStatementSequence(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[31] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[31]);
  };
  Statement *S = nullptr;
  if (M2Parser::parseStatement(__FollowSet, S))
    return __errorhandler();
  if (S) Stmts.push_back(S);
  while (Tok.is(tok::semi)) {
    S = nullptr;
    advance();
    if (M2Parser::parseStatement(__FollowSet, S))
      return __errorhandler();
    if (S) Stmts.push_back(S);
  }
  return false;
}
bool M2Parser::parseAssignmentStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  Expression *E = nullptr;
  if (M2Parser::parseVariableDesignator(__FollowSet))
    return __errorhandler();
  if (consume(tok::colonequal))
    return __errorhandler();
  if (M2Parser::parseExpression(__FollowSet, E))
    return __errorhandler();
  return false;
}
bool M2Parser::parseProcedureCall(const __TokenBitSet &__FollowSetCallers, Statement *&S) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  if (M2Parser::parseProcedureDesignator(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::l_paren)) {
    if (M2Parser::parseActualParameters(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseProcedureDesignator(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[32] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[32]);
  };
  if (M2Parser::parseValueDesignator(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseReturnStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  Expression *E = nullptr;
  if (consume(tok::kw_RETURN))
    return __errorhandler();
  if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, }).contains(Tok.getKind())) {
    if (M2Parser::parseExpression(__FollowSet, E))
      return __errorhandler();
  }
  S = Actions.actOnReturnStmt(E);
  return false;
}
bool M2Parser::parseRetryStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  if (expect(tok::kw_RETRY))
    return __errorhandler();
  SMLoc Loc = Tok.getLocation();
                                 S = Actions.actOnRetryStmt(Loc);
  advance();
  return false;
}
bool M2Parser::parseWithStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  StatementList Stmts;
  if (consume(tok::kw_WITH))
    return __errorhandler();
  if (M2Parser::parseRecordDesignator(__FollowSet))
    return __errorhandler();
  if (consume(tok::kw_DO))
    return __errorhandler();
  if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
    return __errorhandler();
  if (consume(tok::kw_END))
    return __errorhandler();
  return false;
}
bool M2Parser::parseRecordDesignator(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[33] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[33]);
  };
  if (Tok.is(tok::identifier)) {
    if (M2Parser::parseVariableDesignator(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseValueDesignator(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseIfStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  if (M2Parser::parseGuardedStatements(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::kw_ELSE)) {
    if (M2Parser::parseIfElsePart(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::kw_END))
    return __errorhandler();
  return false;
}
bool M2Parser::parseGuardedStatements(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[34] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[34]);
  };
  StatementList Stmts; /* ERROR */
  if (consume(tok::kw_IF))
    return __errorhandler();
  if (M2Parser::parseBooleanExpression(__FollowSet))
    return __errorhandler();
  if (consume(tok::kw_THEN))
    return __errorhandler();
  if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
    return __errorhandler();
  while (Tok.is(tok::kw_ELSIF)) {
    advance();
    if (M2Parser::parseBooleanExpression(__FollowSet))
      return __errorhandler();
    if (consume(tok::kw_THEN))
      return __errorhandler();
    if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseIfElsePart(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[11] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[11]);
  };
  StatementList Stmts; /* ERROR */
  if (consume(tok::kw_ELSE))
    return __errorhandler();
  if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
    return __errorhandler();
  return false;
}
bool M2Parser::parseBooleanExpression(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[35] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[35]);
  };
  Expression *E = nullptr;
  if (M2Parser::parseExpression(__FollowSet, E))
    return __errorhandler();
  return false;
}
bool M2Parser::parseCaseStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  if (consume(tok::kw_CASE))
    return __errorhandler();
  if (M2Parser::parseCaseSelector(__FollowSet))
    return __errorhandler();
  if (consume(tok::kw_OF))
    return __errorhandler();
  if (M2Parser::parseCaseList(__FollowSet))
    return __errorhandler();
  if (consume(tok::kw_END))
    return __errorhandler();
  return false;
}
bool M2Parser::parseCaseSelector(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[26] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[26]);
  };
  if (M2Parser::parseOrdinalExpression(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseCaseList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[11] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[11]);
  };
  if (M2Parser::parseCaseAlternative(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::pipe)) {
    advance();
    if (M2Parser::parseCaseAlternative(__FollowSet))
      return __errorhandler();
  }
  if (Tok.is(tok::kw_ELSE)) {
    if (M2Parser::parseCaseElsePart(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseCaseElsePart(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[11] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[11]);
  };
  StatementList Stmts; /* ERROR */
  if (consume(tok::kw_ELSE))
    return __errorhandler();
  if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
    return __errorhandler();
  return false;
}
bool M2Parser::parseCaseAlternative(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[25] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[25]);
  };
  StatementList Stmts; /* ERROR */
  if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, }).contains(Tok.getKind())) {
    if (M2Parser::parseCaseLabelList(__FollowSet))
      return __errorhandler();
    if (consume(tok::colon))
      return __errorhandler();
    if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseCaseLabelList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[15] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[15]);
  };
  if (M2Parser::parseCaseLabel(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (M2Parser::parseCaseLabel(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseCaseLabel(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[16] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[16]);
  };
  if (M2Parser::parseConstantExpression(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::ellipsis)) {
    advance();
    if (M2Parser::parseConstantExpression(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseWhileStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  if (expect(tok::kw_WHILE))
    return __errorhandler();
  SMLoc Loc = Tok.getLocation();
                                 Expression *Cond = nullptr;
  advance();
  if (M2Parser::parseExpression(__FollowSet, Cond))
    return __errorhandler();
  if (expect(tok::kw_DO))
    return __errorhandler();
  StatementList Stmts;
  advance();
  if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
    return __errorhandler();
  if (expect(tok::kw_END))
    return __errorhandler();
  S = Actions.actOnWhileStmt(Cond, Stmts, Loc);
  advance();
  return false;
}
bool M2Parser::parseRepeatStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  if (expect(tok::kw_REPEAT))
    return __errorhandler();
  SMLoc Loc = Tok.getLocation();
                                 StatementList Stmts;
  advance();
  if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
    return __errorhandler();
  if (expect(tok::kw_UNTIL))
    return __errorhandler();
  Expression *Cond = nullptr;
  advance();
  if (M2Parser::parseExpression(__FollowSet, Cond))
    return __errorhandler();
  S = Actions.actOnRepeatStmt(Cond, Stmts, Loc);
  return false;
}
bool M2Parser::parseLoopStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  if (expect(tok::kw_LOOP))
    return __errorhandler();
  SMLoc Loc = Tok.getLocation();
                                 StatementList Stmts;
  advance();
  if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
    return __errorhandler();
  if (expect(tok::kw_END))
    return __errorhandler();
  S = Actions.actOnLoopStmt(Stmts, Loc);
  advance();
  return false;
}
bool M2Parser::parseExitStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  if (expect(tok::kw_EXIT))
    return __errorhandler();
  SMLoc Loc = Tok.getLocation();
                                 S = Actions.actOnExitStmt(Loc);
  advance();
  return false;
}
bool M2Parser::parseForStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  StatementList Stmts; /* ERROR */
  if (consume(tok::kw_FOR))
    return __errorhandler();
  if (M2Parser::parseControlVariableIdentifier(__FollowSet))
    return __errorhandler();
  if (consume(tok::colonequal))
    return __errorhandler();
  if (M2Parser::parseInitialValue(__FollowSet))
    return __errorhandler();
  if (consume(tok::kw_TO))
    return __errorhandler();
  if (M2Parser::parseFinalValue(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::kw_BY)) {
    advance();
    if (M2Parser::parseStepSize(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::kw_DO))
    return __errorhandler();
  if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
    return __errorhandler();
  if (consume(tok::kw_END))
    return __errorhandler();
  return false;
}
bool M2Parser::parseControlVariableIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[36] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[36]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool M2Parser::parseInitialValue(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[37] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[37]);
  };
  if (M2Parser::parseOrdinalExpression(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseFinalValue(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[38] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[38]);
  };
  if (M2Parser::parseOrdinalExpression(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseStepSize(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[33] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[33]);
  };
  if (M2Parser::parseConstantExpression(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseVariableDesignator(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[39] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[39]);
  };
  if (Tok.is(tok::identifier)) {
    if (M2Parser::parseEntireDesignator(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseIndexedDesignator(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseSelectedDesignator(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseDereferencedDesignator(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (!(getLangOpts().ISOObjects)) {
      error();
      return __errorhandler();
    }
    if (M2Parser::parseObjectSelectedDesignator(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseEntireDesignator(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[39] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[39]);
  };
  if (M2Parser::parseQualifiedIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseIndexedDesignator(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[39] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[39]);
  };
  if (M2Parser::parseArrayVariableDesignator(__FollowSet))
    return __errorhandler();
  if (consume(tok::l_square))
    return __errorhandler();
  if (M2Parser::parseIndexExpression(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (M2Parser::parseIndexExpression(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::r_square))
    return __errorhandler();
  return false;
}
bool M2Parser::parseArrayVariableDesignator(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[22] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[22]);
  };
  if (M2Parser::parseVariableDesignator(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseIndexExpression(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[40] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[40]);
  };
  if (M2Parser::parseOrdinalExpression(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseSelectedDesignator(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[39] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[39]);
  };
  if (M2Parser::parseRecordVariableDesignator(__FollowSet))
    return __errorhandler();
  if (consume(tok::period))
    return __errorhandler();
  if (M2Parser::parseFieldIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseRecordVariableDesignator(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[41] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[41]);
  };
  if (M2Parser::parseVariableDesignator(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseFieldIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[42] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[42]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool M2Parser::parseDereferencedDesignator(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[39] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[39]);
  };
  if (M2Parser::parsePointerVariableDesignator(__FollowSet))
    return __errorhandler();
  if (consume(tok::caret))
    return __errorhandler();
  return false;
}
bool M2Parser::parsePointerVariableDesignator(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[43] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[43]);
  };
  if (M2Parser::parseVariableDesignator(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseExpression(const __TokenBitSet &__FollowSetCallers, Expression *&E) {
  const __TokenBitSet __FollowSet = __FollowSets[44] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[44]);
  };
  if (M2Parser::parseSimpleExpression(__FollowSet, E))
    return __errorhandler();
  if ((__TokenBitSet{tok::equal, tok::hash, tok::less, tok::greater, tok::lessequal, tok::greaterequal, tok::kw_IN, }).contains(Tok.getKind())) {
    OperatorInfo Op;
    if (M2Parser::parseRelationalOperator(__FollowSet, Op))
      return __errorhandler();
    Expression *Right = nullptr;
    if (M2Parser::parseSimpleExpression(__FollowSet, Right))
      return __errorhandler();
    E = Actions.actOnExpression(E, Right, Op);
  }
  return false;
}
bool M2Parser::parseSimpleExpression(const __TokenBitSet &__FollowSetCallers, Expression *&E) {
  const __TokenBitSet __FollowSet = __FollowSets[45] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[45]);
  };
  if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::kw_NOT, }).contains(Tok.getKind())) {
    OperatorInfo Op;
    if (Tok.is(tok::plus)) {
      Op = OperatorInfo(Tok.getLocation(), Tok.getKind());
      advance();
    }
    if (M2Parser::parseTerm(__FollowSet, E))
      return __errorhandler();
    if (Op.getKind() != tok::unknown)
                                   E = Actions.actOnFactor(E, Op);
    while ((__TokenBitSet{tok::plus, tok::minus, tok::kw_OR, }).contains(Tok.getKind())) {
      OperatorInfo Op;
      if (M2Parser::parseTermOperator(__FollowSet, Op))
        return __errorhandler();
      Expression *Right = nullptr;
      if (M2Parser::parseTerm(__FollowSet, Right))
        return __errorhandler();
      E = Actions.actOnSimpleExpression(E, Right, Op);
    }
  }
  else if (Tok.is(tok::minus)) {
    OperatorInfo Op(Tok.getLocation(), Tok.getKind());
    advance();
    if (M2Parser::parseFactor(__FollowSet, E))
      return __errorhandler();
    E = Actions.actOnFactor(E, Op);
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseTerm(const __TokenBitSet &__FollowSetCallers, Expression *&E) {
  const __TokenBitSet __FollowSet = __FollowSets[46] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[46]);
  };
  if (M2Parser::parseFactor(__FollowSet, E))
    return __errorhandler();
  while ((__TokenBitSet{tok::star, tok::slash, tok::kw_REM, tok::kw_DIV, tok::kw_MOD, tok::kw_AND, }).contains(Tok.getKind())) {
    OperatorInfo Op;
    if (M2Parser::parseFactorOperator(__FollowSet, Op))
      return __errorhandler();
    Expression *Right = nullptr;
    if (M2Parser::parseFactor(__FollowSet, Right))
      return __errorhandler();
    E = Actions.actOnTerm(E, Right, Op);
  }
  return false;
}
bool M2Parser::parseFactor(const __TokenBitSet &__FollowSetCallers, Expression *&E) {
  const __TokenBitSet __FollowSet = __FollowSets[47] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[47]);
  };
  if (Tok.is(tok::l_paren)) {
    advance();
    if (M2Parser::parseExpression(__FollowSet, E))
      return __errorhandler();
    if (consume(tok::r_paren))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_NOT)) {
    OperatorInfo Op(Tok.getLocation(), Tok.getKind());
    advance();
    if (M2Parser::parseFactor(__FollowSet, E))
      return __errorhandler();
    E = Actions.actOnFactor(E, Op);
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseValueDesignator(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseFunctionCall(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseValueConstructor(__FollowSet))
      return __errorhandler();
  }
  else if ((__TokenBitSet{tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, }).contains(Tok.getKind())) {
    if (M2Parser::parseConstantLiteral(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseOrdinalExpression(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[48] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[48]);
  };
  Expression *E = nullptr;
  if (M2Parser::parseExpression(__FollowSet, E))
    return __errorhandler();
  return false;
}
bool M2Parser::parseRelationalOperator(const __TokenBitSet &__FollowSetCallers, OperatorInfo &Op) {
  const __TokenBitSet __FollowSet = __FollowSets[49] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[49]);
  };
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
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseTermOperator(const __TokenBitSet &__FollowSetCallers, OperatorInfo &Op) {
  const __TokenBitSet __FollowSet = __FollowSets[50] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[50]);
  };
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
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseFactorOperator(const __TokenBitSet &__FollowSetCallers, OperatorInfo &Op) {
  const __TokenBitSet __FollowSet = __FollowSets[50] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[50]);
  };
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
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseValueDesignator(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[51] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[51]);
  };
  if (Tok.is(tok::identifier)) {
    if (M2Parser::parseEntireValue(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseIndexedValue(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseSelectedValue(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseDereferencedValue(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (!(getLangOpts().ISOObjects)) {
      error();
      return __errorhandler();
    }
    if (M2Parser::parseObjectSelectedValue(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseEntireValue(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[51] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[51]);
  };
  if (M2Parser::parseQualifiedIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseIndexedValue(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[51] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[51]);
  };
  if (M2Parser::parseArrayValue(__FollowSet))
    return __errorhandler();
  if (consume(tok::l_square))
    return __errorhandler();
  if (M2Parser::parseIndexExpression(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (M2Parser::parseIndexExpression(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::r_square))
    return __errorhandler();
  return false;
}
bool M2Parser::parseArrayValue(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[22] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[22]);
  };
  if (M2Parser::parseValueDesignator(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseSelectedValue(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[51] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[51]);
  };
  if (M2Parser::parseRecordValue(__FollowSet))
    return __errorhandler();
  if (consume(tok::period))
    return __errorhandler();
  if (M2Parser::parseFieldIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseRecordValue(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[41] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[41]);
  };
  if (M2Parser::parseValueDesignator(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseDereferencedValue(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[51] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[51]);
  };
  if (M2Parser::parsePointerValue(__FollowSet))
    return __errorhandler();
  if (consume(tok::caret))
    return __errorhandler();
  return false;
}
bool M2Parser::parsePointerValue(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[43] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[43]);
  };
  if (M2Parser::parseValueDesignator(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseFunctionCall(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[47] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[47]);
  };
  if (M2Parser::parseFunctionDesignator(__FollowSet))
    return __errorhandler();
  if (M2Parser::parseActualParameters(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseFunctionDesignator(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[52] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[52]);
  };
  if (M2Parser::parseValueDesignator(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseValueConstructor(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[47] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[47]);
  };
  if (Tok.is(tok::identifier)) {
    if (M2Parser::parseArrayConstructor(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseRecordConstructor(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseSetConstructor(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseArrayConstructor(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[47] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[47]);
  };
  if (M2Parser::parseArrayTypeIdentifier(__FollowSet))
    return __errorhandler();
  if (M2Parser::parseArrayConstructedValue(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseArrayTypeIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[53] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[53]);
  };
  if (M2Parser::parseTypeIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseArrayConstructedValue(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[47] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[47]);
  };
  if (consume(tok::l_brace))
    return __errorhandler();
  if (M2Parser::parseRepeatedStructureComponent(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (M2Parser::parseRepeatedStructureComponent(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::r_brace))
    return __errorhandler();
  return false;
}
bool M2Parser::parseRepeatedStructureComponent(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[54] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[54]);
  };
  if (M2Parser::parseStructureComponent(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::kw_BY)) {
    advance();
    if (M2Parser::parseRepetitionFactor(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseRepetitionFactor(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[54] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[54]);
  };
  if (M2Parser::parseConstantExpression(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseStructureComponent(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[55] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[55]);
  };
  if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, }).contains(Tok.getKind())) {
    Expression *E = nullptr;
    if (M2Parser::parseExpression(__FollowSet, E))
      return __errorhandler();
  }
  else if (Tok.is(tok::l_brace)) {
    if (M2Parser::parseArrayConstructedValue(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::l_brace)) {
    if (M2Parser::parseRecordConstructedValue(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::l_brace)) {
    if (M2Parser::parseSetConstructedValue(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseRecordConstructor(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[47] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[47]);
  };
  if (M2Parser::parseRecordTypeIdentifier(__FollowSet))
    return __errorhandler();
  if (M2Parser::parseRecordConstructedValue(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseRecordTypeIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[53] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[53]);
  };
  if (M2Parser::parseTypeIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseRecordConstructedValue(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[47] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[47]);
  };
  if (consume(tok::l_brace))
    return __errorhandler();
  if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::l_brace, }).contains(Tok.getKind())) {
    if (M2Parser::parseStructureComponent(__FollowSet))
      return __errorhandler();
    while (Tok.is(tok::comma)) {
      advance();
      if (M2Parser::parseStructureComponent(__FollowSet))
        return __errorhandler();
    }
  }
  if (consume(tok::r_brace))
    return __errorhandler();
  return false;
}
bool M2Parser::parseSetConstructor(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[47] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[47]);
  };
  if (M2Parser::parseSetTypeIdentifier(__FollowSet))
    return __errorhandler();
  if (M2Parser::parseSetConstructedValue(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseSetTypeIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[53] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[53]);
  };
  if (M2Parser::parseTypeIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseSetConstructedValue(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[47] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[47]);
  };
  if (consume(tok::l_brace))
    return __errorhandler();
  if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, }).contains(Tok.getKind())) {
    if (M2Parser::parseMember(__FollowSet))
      return __errorhandler();
    while (Tok.is(tok::comma)) {
      advance();
      if (M2Parser::parseMember(__FollowSet))
        return __errorhandler();
    }
  }
  if (consume(tok::r_brace))
    return __errorhandler();
  return false;
}
bool M2Parser::parseMember(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[54] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[54]);
  };
  if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, }).contains(Tok.getKind())) {
    if (M2Parser::parseInterval(__FollowSet))
      return __errorhandler();
  }
  else if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, }).contains(Tok.getKind())) {
    if (M2Parser::parseSingleton(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseInterval(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[54] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[54]);
  };
  if (M2Parser::parseOrdinalExpression(__FollowSet))
    return __errorhandler();
  if (consume(tok::ellipsis))
    return __errorhandler();
  if (M2Parser::parseOrdinalExpression(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseSingleton(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[54] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[54]);
  };
  if (M2Parser::parseOrdinalExpression(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseConstantLiteral(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[47] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[47]);
  };
  if (Tok.is(tok::integer_literal)) {
    advance();
  }
  else if (Tok.is(tok::real_literal)) {
    advance();
  }
  else if ((__TokenBitSet{tok::char_literal, tok::string_literal, }).contains(Tok.getKind())) {
    if (M2Parser::parseStringLiteral(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseStringLiteral(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[47] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[47]);
  };
  if (Tok.is(tok::string_literal)) {
    advance();
  }
  else if (Tok.is(tok::char_literal)) {
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseConstantExpression(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[56] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[56]);
  };
  Expression *E = nullptr;
  if (M2Parser::parseExpression(__FollowSet, E))
    return __errorhandler();
  return false;
}
bool M2Parser::parseActualParameters(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[47] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[47]);
  };
  if (consume(tok::l_paren))
    return __errorhandler();
  if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, }).contains(Tok.getKind())) {
    if (M2Parser::parseActualParameterList(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::r_paren))
    return __errorhandler();
  return false;
}
bool M2Parser::parseActualParameterList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[8] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[8]);
  };
  if (M2Parser::parseActualParameter(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (M2Parser::parseActualParameter(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseActualParameter(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[10] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[10]);
  };
  Expression *E = nullptr;
  if (Tok.is(tok::identifier)) {
    if (M2Parser::parseVariableDesignator(__FollowSet))
      return __errorhandler();
  }
  else if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, }).contains(Tok.getKind())) {
    if (M2Parser::parseExpression(__FollowSet, E))
      return __errorhandler();
  }
  else if (Tok.is(tok::identifier)) {
    if (M2Parser::parseTypeParameter(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseTypeParameter(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[10] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[10]);
  };
  if (M2Parser::parseTypeIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseClassDefinition(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (Tok.is(tok::kw_TRACED)) {
    if (M2Parser::parseTracedClassDefinition(__FollowSet))
      return __errorhandler();
  }
  else if ((__TokenBitSet{tok::kw_CLASS, tok::kw_ABSTRACT, }).contains(Tok.getKind())) {
    if (M2Parser::parseUntracedClassDefinition(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseUntracedClassDefinition(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (Tok.is(tok::kw_CLASS)) {
    if (M2Parser::parseNormalClassDefinition(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_ABSTRACT)) {
    if (M2Parser::parseAbstractClassDefinition(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseTracedClassDefinition(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::kw_TRACED))
    return __errorhandler();
  if (Tok.is(tok::kw_CLASS)) {
    if (M2Parser::parseNormalClassDefinition(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_ABSTRACT)) {
    if (M2Parser::parseAbstractClassDefinition(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseNormalClassDefinition(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (M2Parser::parseNormalClassHeader(__FollowSet))
    return __errorhandler();
  if ((__TokenBitSet{tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_OVERRIDE, tok::kw_REVEAL, tok::kw_INHERIT, }).contains(Tok.getKind())) {
    if (M2Parser::parseNormalClassDefinitionBody(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_FORWARD)) {
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseNormalClassHeader(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[57] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[57]);
  };
  if (consume(tok::kw_CLASS))
    return __errorhandler();
  if (M2Parser::parseClassIdentifier(__FollowSet))
    return __errorhandler();
  if (consume(tok::semi))
    return __errorhandler();
  return false;
}
bool M2Parser::parseNormalClassDefinitionBody(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (Tok.is(tok::kw_INHERIT)) {
    if (M2Parser::parseInheritClause(__FollowSet))
      return __errorhandler();
  }
  if (Tok.is(tok::kw_REVEAL)) {
    if (M2Parser::parseRevealList(__FollowSet))
      return __errorhandler();
  }
  if (M2Parser::parseNormalClassComponentDefinitions(__FollowSet))
    return __errorhandler();
  if (consume(tok::kw_END))
    return __errorhandler();
  if (M2Parser::parseClassIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseAbstractClassDefinition(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (M2Parser::parseAbstractClassHeader(__FollowSet))
    return __errorhandler();
  if ((__TokenBitSet{tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_ABSTRACT, tok::kw_OVERRIDE, tok::kw_REVEAL, tok::kw_INHERIT, }).contains(Tok.getKind())) {
    if (M2Parser::parseAbstractClassDefinitionBody(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_FORWARD)) {
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseAbstractClassHeader(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[58] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[58]);
  };
  if (consume(tok::kw_ABSTRACT))
    return __errorhandler();
  if (consume(tok::kw_CLASS))
    return __errorhandler();
  if (M2Parser::parseClassIdentifier(__FollowSet))
    return __errorhandler();
  if (consume(tok::semi))
    return __errorhandler();
  return false;
}
bool M2Parser::parseAbstractClassDefinitionBody(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (Tok.is(tok::kw_INHERIT)) {
    if (M2Parser::parseInheritClause(__FollowSet))
      return __errorhandler();
  }
  if (Tok.is(tok::kw_REVEAL)) {
    if (M2Parser::parseRevealList(__FollowSet))
      return __errorhandler();
  }
  if (M2Parser::parseAbstractClassComponentDefinitions(__FollowSet))
    return __errorhandler();
  if (consume(tok::kw_END))
    return __errorhandler();
  if (M2Parser::parseClassIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseClassIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[59] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[59]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool M2Parser::parseNormalClassComponentDefinitions(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[11] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[11]);
  };
  while ((__TokenBitSet{tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_OVERRIDE, }).contains(Tok.getKind())) {
    if (M2Parser::parseNormalComponentDefinition(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseNormalComponentDefinition(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[60] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[60]);
  };
  DeclarationList Decls;
  if (Tok.is(tok::kw_CONST)) {
    advance();
    while (Tok.is(tok::identifier)) {
      if (M2Parser::parseConstantDeclaration(__FollowSet, Decls))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
  }
  else if (Tok.is(tok::kw_TYPE)) {
    advance();
    while (Tok.is(tok::identifier)) {
      if (M2Parser::parseTypeDefinition(__FollowSet, Decls))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
  }
  else if (Tok.is(tok::kw_VAR)) {
    advance();
    if (Tok.is(tok::identifier)) {
      if (M2Parser::parseClassVariableDeclaration(__FollowSet))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
  }
  else if ((__TokenBitSet{tok::kw_PROCEDURE, tok::kw_OVERRIDE, }).contains(Tok.getKind())) {
    if (Tok.is(tok::kw_PROCEDURE)) {
      if (M2Parser::parseNormalMethodDefinition(__FollowSet))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_OVERRIDE)) {
      if (M2Parser::parseOverridingMethodDefinition(__FollowSet))
        return __errorhandler();
    }
    else {
      error();
      return __errorhandler();
    }
    if (consume(tok::semi))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseAbstractClassComponentDefinitions(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[11] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[11]);
  };
  while ((__TokenBitSet{tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_ABSTRACT, tok::kw_OVERRIDE, }).contains(Tok.getKind())) {
    if (M2Parser::parseAbstractComponentDefinition(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseAbstractComponentDefinition(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[61] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[61]);
  };
  DeclarationList Decls;
  if (Tok.is(tok::kw_CONST)) {
    advance();
    while (Tok.is(tok::identifier)) {
      if (M2Parser::parseConstantDeclaration(__FollowSet, Decls))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
  }
  else if (Tok.is(tok::kw_TYPE)) {
    advance();
    while (Tok.is(tok::identifier)) {
      if (M2Parser::parseTypeDefinition(__FollowSet, Decls))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
  }
  else if (Tok.is(tok::kw_VAR)) {
    advance();
    while (Tok.is(tok::identifier)) {
      if (M2Parser::parseClassVariableDeclaration(__FollowSet))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
  }
  else if ((__TokenBitSet{tok::kw_PROCEDURE, tok::kw_ABSTRACT, tok::kw_OVERRIDE, }).contains(Tok.getKind())) {
    if (Tok.is(tok::kw_PROCEDURE)) {
      if (M2Parser::parseNormalMethodDefinition(__FollowSet))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_ABSTRACT)) {
      if (M2Parser::parseAbstractMethodDefinition(__FollowSet))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_OVERRIDE)) {
      if (M2Parser::parseOverridingMethodDefinition(__FollowSet))
        return __errorhandler();
    }
    else {
      error();
      return __errorhandler();
    }
    if (consume(tok::semi))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseClassVariableDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (M2Parser::parseIdentifierList(__FollowSet))
    return __errorhandler();
  if (consume(tok::colon))
    return __errorhandler();
  if (M2Parser::parseTypeDenoter(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseNormalMethodDefinition(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (M2Parser::parseProcedureHeading(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseOverridingMethodDefinition(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::kw_OVERRIDE))
    return __errorhandler();
  if (M2Parser::parseProcedureHeading(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseAbstractMethodDefinition(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::kw_ABSTRACT))
    return __errorhandler();
  if (M2Parser::parseProcedureHeading(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseClassDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (Tok.is(tok::kw_TRACED)) {
    if (M2Parser::parseTracedClassDeclaration(__FollowSet))
      return __errorhandler();
  }
  else if ((__TokenBitSet{tok::kw_CLASS, tok::kw_ABSTRACT, }).contains(Tok.getKind())) {
    if (M2Parser::parseUntracedClassDeclaration(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseUntracedClassDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (Tok.is(tok::kw_CLASS)) {
    if (M2Parser::parseNormalClassDeclaration(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_ABSTRACT)) {
    if (M2Parser::parseAbstractClassDeclaration(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseNormalClassDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (M2Parser::parseNormalClassHeader(__FollowSet))
    return __errorhandler();
  if ((__TokenBitSet{tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_OVERRIDE, tok::kw_REVEAL, tok::kw_INHERIT, }).contains(Tok.getKind())) {
    if (M2Parser::parseNormalClassDeclarationBody(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_FORWARD)) {
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseNormalClassDeclarationBody(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (Tok.is(tok::kw_INHERIT)) {
    if (M2Parser::parseInheritClause(__FollowSet))
      return __errorhandler();
  }
  if (Tok.is(tok::kw_REVEAL)) {
    if (M2Parser::parseRevealList(__FollowSet))
      return __errorhandler();
  }
  if (M2Parser::parseNormalClassComponentDeclarations(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::kw_BEGIN)) {
    if (M2Parser::parseClassBody(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::kw_END))
    return __errorhandler();
  if (M2Parser::parseClassIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseAbstractClassDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (M2Parser::parseAbstractClassHeader(__FollowSet))
    return __errorhandler();
  if ((__TokenBitSet{tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_ABSTRACT, tok::kw_OVERRIDE, tok::kw_REVEAL, tok::kw_INHERIT, }).contains(Tok.getKind())) {
    if (M2Parser::parseAbstractClassDeclarationBody(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_FORWARD)) {
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseAbstractClassDeclarationBody(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (Tok.is(tok::kw_INHERIT)) {
    if (M2Parser::parseInheritClause(__FollowSet))
      return __errorhandler();
  }
  if (Tok.is(tok::kw_REVEAL)) {
    if (M2Parser::parseRevealList(__FollowSet))
      return __errorhandler();
  }
  if (M2Parser::parseAbstractClassComponentDeclarations(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::kw_BEGIN)) {
    if (M2Parser::parseClassBody(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::kw_END))
    return __errorhandler();
  if (M2Parser::parseClassIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseClassBody(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[11] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[11]);
  };
  Block InitBlk, FinalBlk;
  if (M2Parser::parseModuleBody(__FollowSet, InitBlk, FinalBlk))
    return __errorhandler();
  return false;
}
bool M2Parser::parseNormalClassComponentDeclarations(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[14] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[14]);
  };
  while ((__TokenBitSet{tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_OVERRIDE, }).contains(Tok.getKind())) {
    if (M2Parser::parseNormalComponentDeclaration(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseNormalComponentDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[62] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[62]);
  };
  DeclarationList Decls;
  if (Tok.is(tok::kw_CONST)) {
    advance();
    while (Tok.is(tok::identifier)) {
      if (M2Parser::parseConstantDeclaration(__FollowSet, Decls))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
  }
  else if (Tok.is(tok::kw_TYPE)) {
    advance();
    while (Tok.is(tok::identifier)) {
      if (M2Parser::parseTypeDeclaration(__FollowSet, Decls))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
  }
  else if (Tok.is(tok::kw_VAR)) {
    advance();
    while (Tok.is(tok::identifier)) {
      if (M2Parser::parseClassVariableDeclaration(__FollowSet))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
  }
  else if ((__TokenBitSet{tok::kw_PROCEDURE, tok::kw_OVERRIDE, }).contains(Tok.getKind())) {
    if (M2Parser::parseNormalMethodDeclarations(__FollowSet))
      return __errorhandler();
    if (consume(tok::semi))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseAbstractClassComponentDeclarations(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[14] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[14]);
  };
  while ((__TokenBitSet{tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_ABSTRACT, tok::kw_OVERRIDE, }).contains(Tok.getKind())) {
    if (M2Parser::parseAbstractComponentDeclaration(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseAbstractComponentDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[63] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[63]);
  };
  DeclarationList Decls;
  if (Tok.is(tok::kw_CONST)) {
    advance();
    while (Tok.is(tok::identifier)) {
      if (M2Parser::parseConstantDeclaration(__FollowSet, Decls))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
  }
  else if (Tok.is(tok::kw_TYPE)) {
    advance();
    while (Tok.is(tok::identifier)) {
      if (M2Parser::parseTypeDeclaration(__FollowSet, Decls))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
  }
  else if (Tok.is(tok::kw_VAR)) {
    advance();
    while (Tok.is(tok::identifier)) {
      if (M2Parser::parseClassVariableDeclaration(__FollowSet))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
  }
  else if ((__TokenBitSet{tok::kw_PROCEDURE, tok::kw_ABSTRACT, tok::kw_OVERRIDE, }).contains(Tok.getKind())) {
    if (M2Parser::parseAbstractMethodDeclarations(__FollowSet))
      return __errorhandler();
    if (consume(tok::semi))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseNormalMethodDeclarations(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (Tok.is(tok::kw_PROCEDURE)) {
    if (M2Parser::parseNormalMethodDeclaration(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_OVERRIDE)) {
    if (M2Parser::parseOverridingMethodDeclaration(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseNormalMethodDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (M2Parser::parseProcedureDeclaration(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseOverridingMethodDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::kw_OVERRIDE))
    return __errorhandler();
  if (M2Parser::parseProcedureDeclaration(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseAbstractMethodDeclarations(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (Tok.is(tok::kw_PROCEDURE)) {
    if (M2Parser::parseNormalMethodDeclaration(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_ABSTRACT)) {
    if (M2Parser::parseAbstractMethodDefinition(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_OVERRIDE)) {
    if (M2Parser::parseOverridingMethodDeclaration(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseTracedClassDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::kw_TRACED))
    return __errorhandler();
  if (Tok.is(tok::kw_CLASS)) {
    if (M2Parser::parseNormalTracedClassDeclaration(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_ABSTRACT)) {
    if (M2Parser::parseAbstractTracedClassDeclaration(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseNormalTracedClassDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (M2Parser::parseNormalTracedClassHeader(__FollowSet))
    return __errorhandler();
  if ((__TokenBitSet{tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_OVERRIDE, tok::kw_REVEAL, tok::kw_INHERIT, }).contains(Tok.getKind())) {
    if (M2Parser::parseNormalTracedClassDeclarationBody(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_FORWARD)) {
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseNormalTracedClassHeader(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[57] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[57]);
  };
  if (consume(tok::kw_CLASS))
    return __errorhandler();
  if (M2Parser::parseClassIdentifier(__FollowSet))
    return __errorhandler();
  if (consume(tok::semi))
    return __errorhandler();
  return false;
}
bool M2Parser::parseNormalTracedClassDeclarationBody(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (Tok.is(tok::kw_INHERIT)) {
    if (M2Parser::parseInheritClause(__FollowSet))
      return __errorhandler();
  }
  if (Tok.is(tok::kw_REVEAL)) {
    if (M2Parser::parseRevealList(__FollowSet))
      return __errorhandler();
  }
  if (M2Parser::parseNormalClassComponentDeclarations(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::kw_BEGIN)) {
    if (M2Parser::parseTracedClassBody(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::kw_END))
    return __errorhandler();
  if (M2Parser::parseClassIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseAbstractTracedClassDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (M2Parser::parseAbstractTracedClassHeader(__FollowSet))
    return __errorhandler();
  if ((__TokenBitSet{tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_ABSTRACT, tok::kw_OVERRIDE, tok::kw_REVEAL, tok::kw_INHERIT, }).contains(Tok.getKind())) {
    if (M2Parser::parseAbstractTracedClassDeclarationBody(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_FORWARD)) {
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseAbstractTracedClassHeader(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[58] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[58]);
  };
  if (consume(tok::kw_ABSTRACT))
    return __errorhandler();
  if (consume(tok::kw_CLASS))
    return __errorhandler();
  if (M2Parser::parseClassIdentifier(__FollowSet))
    return __errorhandler();
  if (consume(tok::semi))
    return __errorhandler();
  return false;
}
bool M2Parser::parseAbstractTracedClassDeclarationBody(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (Tok.is(tok::kw_INHERIT)) {
    if (M2Parser::parseInheritClause(__FollowSet))
      return __errorhandler();
  }
  if (Tok.is(tok::kw_REVEAL)) {
    if (M2Parser::parseRevealList(__FollowSet))
      return __errorhandler();
  }
  if (M2Parser::parseAbstractClassComponentDeclarations(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::kw_BEGIN)) {
    if (M2Parser::parseTracedClassBody(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::kw_END))
    return __errorhandler();
  if (M2Parser::parseClassIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseTracedClassBody(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[11] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[11]);
  };
  Block Body;
  if (consume(tok::kw_BEGIN))
    return __errorhandler();
  if (M2Parser::parseBlockBody(__FollowSet, Body))
    return __errorhandler();
  return false;
}
bool M2Parser::parseRevealList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[63] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[63]);
  };
  if (consume(tok::kw_REVEAL))
    return __errorhandler();
  if (M2Parser::parseRevealedComponentList(__FollowSet))
    return __errorhandler();
  if (consume(tok::semi))
    return __errorhandler();
  return false;
}
bool M2Parser::parseRevealedComponentList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (M2Parser::parseRevealedComponent(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (M2Parser::parseRevealedComponent(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseRevealedComponent(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[64] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[64]);
  };
  if (Tok.is(tok::identifier)) {
    advance();
  }
  else if (Tok.is(tok::kw_READONLY)) {
    advance();
    if (M2Parser::parseClassVariableIdentifier(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseClassVariableIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[65] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[65]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool M2Parser::parseInheritClause(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[66] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[66]);
  };
  if (consume(tok::kw_INHERIT))
    return __errorhandler();
  if (M2Parser::parseClassTypeIdentifier(__FollowSet))
    return __errorhandler();
  if (consume(tok::semi))
    return __errorhandler();
  return false;
}
bool M2Parser::parseClassTypeIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[67] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[67]);
  };
  if (M2Parser::parseTypeIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseObjectSelectedDesignator(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[39] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[39]);
  };
  if (M2Parser::parseObjectVariableDesignator(__FollowSet))
    return __errorhandler();
  if (consume(tok::period))
    return __errorhandler();
  if (Tok.is(tok::identifier)) {
    if (M2Parser::parseClassIdentifier(__FollowSet))
      return __errorhandler();
    if (consume(tok::period))
      return __errorhandler();
  }
  if (M2Parser::parseClassVariableIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseObjectVariableDesignator(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[41] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[41]);
  };
  if (M2Parser::parseVariableDesignator(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseObjectSelectedValue(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[51] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[51]);
  };
  if (M2Parser::parseObjectValueDesignator(__FollowSet))
    return __errorhandler();
  if (consume(tok::period))
    return __errorhandler();
  if (Tok.is(tok::identifier)) {
    if (M2Parser::parseClassIdentifier(__FollowSet))
      return __errorhandler();
    if (consume(tok::period))
      return __errorhandler();
  }
  if (M2Parser::parseEntityIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseObjectValueDesignator(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[41] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[41]);
  };
  if (M2Parser::parseValueDesignator(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseEntityIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[51] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[51]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool M2Parser::parseGuardStatement(const __TokenBitSet &__FollowSetCallers, Statement *&S) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  StatementList Stmts; /* ERROR */
  if (consume(tok::kw_GUARD))
    return __errorhandler();
  if (M2Parser::parseGuardSelector(__FollowSet))
    return __errorhandler();
  if (consume(tok::kw_AS))
    return __errorhandler();
  if (M2Parser::parseGuardedList(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::kw_ELSE)) {
    advance();
    if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
      return __errorhandler();
  }
  if (consume(tok::kw_END))
    return __errorhandler();
  return false;
}
bool M2Parser::parseGuardSelector(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[68] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[68]);
  };
  Expression *E = nullptr;
  if (M2Parser::parseExpression(__FollowSet, E))
    return __errorhandler();
  return false;
}
bool M2Parser::parseGuardedList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[34] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[34]);
  };
  if (M2Parser::parseGuardedStatementSequence(__FollowSet))
    return __errorhandler();
  if (Tok.is(tok::pipe)) {
    advance();
    if (M2Parser::parseGuardedStatementSequence(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseGuardedStatementSequence(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[25] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[25]);
  };
  StatementList Stmts; /* ERROR */
  if ((__TokenBitSet{tok::identifier, tok::colon, }).contains(Tok.getKind())) {
    if (Tok.is(tok::identifier)) {
      if (M2Parser::parseObjectDenoter(__FollowSet))
        return __errorhandler();
    }
    if (consume(tok::colon))
      return __errorhandler();
    if (M2Parser::parseGuardedClassType(__FollowSet))
      return __errorhandler();
    if (consume(tok::kw_DO))
      return __errorhandler();
    if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseGuardedClassType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[33] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[33]);
  };
  if (M2Parser::parseClassTypeIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseObjectDenoter(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[15] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[15]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
#endif
