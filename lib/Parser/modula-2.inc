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
bool parseProgramModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool IsUnsafeGuarded);
bool parseModuleIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseProtection(const __TokenBitSet &__FollowSetCallers, Expression *&Expr);
bool parseDefinitionModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool IsUnsafeGuarded);
bool parseRefiningDefinitionModuleTail(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool IsUnsafeGuarded, Identifier ModuleName);
bool parseDefinitionModuleTail(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool IsUnsafeGuarded, Identifier ModuleName);
bool parseImplementationModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool IsUnsafeGuarded);
bool parseRefiningImplementationModuleTail(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool IsUnsafeGuarded, Identifier ModuleName);
bool parseImplementationModuleTail(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool IsUnsafeGuarded, Identifier ModuleName);
bool parseImportLists(const __TokenBitSet &__FollowSetCallers, ImportItemList &Imports);
bool parseImportList(const __TokenBitSet &__FollowSetCallers, ImportItemList &Imports);
bool parseSimpleImport(const __TokenBitSet &__FollowSetCallers, ImportItemList &Imports);
bool parseUnqualifiedImport(const __TokenBitSet &__FollowSetCallers, ImportItemList &Imports);
bool parseExportList(const __TokenBitSet &__FollowSetCallers, LocalModule *LM);
bool parseQualifiedIdentifier(const __TokenBitSet &__FollowSetCallers, Declaration *&Decl);
bool parseGenericDefinitionModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM);
bool parseGenericImplementationModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM);
bool parseGenericSeparateModuleIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseFormalModuleParameters(const __TokenBitSet &__FollowSetCallers);
bool parseFormalModuleParameterList(const __TokenBitSet &__FollowSetCallers);
bool parseFormalModuleParameter(const __TokenBitSet &__FollowSetCallers);
bool parseActualModuleParameters(const __TokenBitSet &__FollowSetCallers, ActualParameterList &Params);
bool parseActualModuleParameterList(const __TokenBitSet &__FollowSetCallers, ActualParameterList &Params);
bool parseActualModuleParameter(const __TokenBitSet &__FollowSetCallers, ActualParameterList &Params);
bool parseDefinitions(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseProcedureHeading(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseTypeDefinition(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseOpaqueTypeDefinition(const __TokenBitSet &__FollowSetCallers);
bool parseFormalParameters(const __TokenBitSet &__FollowSetCallers, FormalParameterList &Params);
bool parseFormalParameterList(const __TokenBitSet &__FollowSetCallers, FormalParameterList &Params);
bool parseFunctionResultType(const __TokenBitSet &__FollowSetCallers, Type *&Ty);
bool parseFormalParameter(const __TokenBitSet &__FollowSetCallers, FormalParameterList &Params);
bool parseDeclarations(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseConstantDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseTypeDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseVariableDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseVariableIdentifierList(const __TokenBitSet &__FollowSetCallers, VariableIdentifierList &VarIdList);
bool parseMachineAddress(const __TokenBitSet &__FollowSetCallers, Expression *&Addr);
bool parseProcedureDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseLocalModuleDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseTypeDenoter(const __TokenBitSet &__FollowSetCallers, TypeDenoter *&TyDen);
bool parseIndexTypeList(const __TokenBitSet &__FollowSetCallers, TypeDenoterList &TyDens);
bool parseTypeIdentifier(const __TokenBitSet &__FollowSetCallers, Type *&Ty);
bool parseOrdinalTypeIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseOrdinalTypeDenoter(const __TokenBitSet &__FollowSetCallers, TypeDenoter *&TyDen);
bool parseSubrangeTypeTail(const __TokenBitSet &__FollowSetCallers, TypeDenoter *&TyDen, Declaration *Decl);
bool parseEnumerationType(const __TokenBitSet &__FollowSetCallers, TypeDenoter *&TyDen);
bool parseIdentifierList(const __TokenBitSet &__FollowSetCallers, IdentifierList &IdentList);
bool parseProcedureType(const __TokenBitSet &__FollowSetCallers, TypeDenoter *&TyDen);
bool parseFormalParameterTypeList(const __TokenBitSet &__FollowSetCallers, FormalParameterTypeList &FPTList);
bool parseFormalParameterType(const __TokenBitSet &__FollowSetCallers, FormalParameterTypeList &FPTList);
bool parseFormalType(const __TokenBitSet &__FollowSetCallers, TypeDenoter *&FT);
bool parseFieldList(const __TokenBitSet &__FollowSetCallers, RecordFieldList &Fields);
bool parseFields(const __TokenBitSet &__FollowSetCallers, RecordFieldList &Fields);
bool parseFixedFields(const __TokenBitSet &__FollowSetCallers, RecordFieldList &Fields);
bool parseVariantFields(const __TokenBitSet &__FollowSetCallers);
bool parseTagIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseTagType(const __TokenBitSet &__FollowSetCallers);
bool parseVariantList(const __TokenBitSet &__FollowSetCallers);
bool parseVariantElsePart(const __TokenBitSet &__FollowSetCallers);
bool parseVariant(const __TokenBitSet &__FollowSetCallers);
bool parseVariantLabelList(const __TokenBitSet &__FollowSetCallers);
bool parseVariantLabel(const __TokenBitSet &__FollowSetCallers);
bool parseProperProcedureBlock(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls, Block &Body, bool IsFunction);
bool parseModuleBlock(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls, Block &InitBlk, Block &FinalBlk);
bool parseModuleBody(const __TokenBitSet &__FollowSetCallers, Block &InitBlk, Block &FinalBlk);
bool parseInitializationBody(const __TokenBitSet &__FollowSetCallers, Block &InitBlk);
bool parseFinalizationBody(const __TokenBitSet &__FollowSetCallers, Block &FinalBlk);
bool parseBlockBody(const __TokenBitSet &__FollowSetCallers, Block &Blk);
bool parseNormalPart(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseExceptionalPart(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseStatementSequence(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseAssignmentOrProcedireCall(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseReturnStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseRetryStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseWithStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseIfStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseGuardedStatements(const __TokenBitSet &__FollowSetCallers, GuardedStatementList &GuardedStmts);
bool parseIfElsePart(const __TokenBitSet &__FollowSetCallers, StatementList &ElseStmts);
bool parseCaseStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseCaseSelector(const __TokenBitSet &__FollowSetCallers);
bool parseCaseList(const __TokenBitSet &__FollowSetCallers);
bool parseCaseElsePart(const __TokenBitSet &__FollowSetCallers);
bool parseCaseAlternative(const __TokenBitSet &__FollowSetCallers);
bool parseCaseLabelList(const __TokenBitSet &__FollowSetCallers);
bool parseCaseLabel(const __TokenBitSet &__FollowSetCallers);
bool parseWhileStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseRepeatStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseLoopStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseExitStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseForStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseFieldIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseExpression(const __TokenBitSet &__FollowSetCallers, Expression *&E);
bool parseSimpleExpression(const __TokenBitSet &__FollowSetCallers, Expression *&E);
bool parseTerm(const __TokenBitSet &__FollowSetCallers, Expression *&E);
bool parseFactor(const __TokenBitSet &__FollowSetCallers, Expression *&E);
bool parseOrdinalExpression(const __TokenBitSet &__FollowSetCallers, Expression *&E);
bool parseRelationalOperator(const __TokenBitSet &__FollowSetCallers, OperatorInfo &Op);
bool parseTermOperator(const __TokenBitSet &__FollowSetCallers, OperatorInfo &Op);
bool parseFactorOperator(const __TokenBitSet &__FollowSetCallers, OperatorInfo &Op);
bool parseDesignator(const __TokenBitSet &__FollowSetCallers, Designator *&Desig);
bool parseDesignatorTail(const __TokenBitSet &__FollowSetCallers, SelectorList &Selectors);
bool parseValueConstructorTail(const __TokenBitSet &__FollowSetCallers);
bool parseRepeatedStructureComponent(const __TokenBitSet &__FollowSetCallers);
bool parseConstantLiteral(const __TokenBitSet &__FollowSetCallers, Expression *&Expr);
bool parseConstantExpression(const __TokenBitSet &__FollowSetCallers);
bool parseActualParameters(const __TokenBitSet &__FollowSetCallers, ActualParameterList &Params);
bool parseActualParameterList(const __TokenBitSet &__FollowSetCallers, ActualParameterList &Params);
bool parseActualParameter(const __TokenBitSet &__FollowSetCallers, ActualParameterList &Params);
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
bool parseNormalMethodDefinition(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseOverridingMethodDefinition(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseAbstractMethodDefinition(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseClassDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
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
bool parseNormalMethodDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
bool parseOverridingMethodDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls);
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
bool parseEntityIdentifier(const __TokenBitSet &__FollowSetCallers);
bool parseGuardStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts);
bool parseGuardSelector(const __TokenBitSet &__FollowSetCallers);
bool parseGuardedList(const __TokenBitSet &__FollowSetCallers);
bool parseGuardedStatementSequence(const __TokenBitSet &__FollowSetCallers);
bool parseGuardedClassType(const __TokenBitSet &__FollowSetCallers);
bool parseObjectDenoter(const __TokenBitSet &__FollowSetCallers);
#endif
#ifdef M2PARSER_DEFINITION
const M2Parser::__TokenBitSet M2Parser::__FollowSets[] = {
  { tok::eof },
  { tok::semi, tok::period, tok::l_square, tok::kw_IMPORT, tok::l_paren },
  { tok::semi, tok::l_paren },
  { tok::kw_MODULE, tok::kw_END, tok::kw_EXPORT, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT },
  { tok::kw_MODULE, tok::kw_END, tok::kw_IMPORT, tok::kw_FROM, tok::kw_EXPORT, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT },
  { tok::kw_MODULE, tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT },
  { tok::semi, tok::period, tok::l_square, tok::r_square, tok::equal, tok::kw_END, tok::l_paren, tok::r_paren, tok::colon, tok::comma, tok::kw_OF, tok::kw_TO, tok::ellipsis, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::colonequal, tok::kw_DO, tok::kw_THEN, tok::kw_ELSIF, tok::kw_UNTIL, tok::kw_BY, tok::plus, tok::minus, tok::hash, tok::less, tok::greater, tok::lessequal, tok::greaterequal, tok::kw_IN, tok::kw_OR, tok::star, tok::slash, tok::kw_REM, tok::kw_DIV, tok::kw_MOD, tok::kw_AND, tok::caret, tok::l_brace, tok::r_brace, tok::kw_AS },
  { tok::semi },
  { tok::r_paren },
  { tok::semi, tok::r_paren },
  { tok::r_paren, tok::comma },
  { tok::kw_END },
  { tok::semi, tok::colon },
  { tok::kw_END, tok::kw_BEGIN },
  { tok::colon },
  { tok::colon, tok::comma },
  { tok::semi, tok::kw_END, tok::pipe, tok::kw_ELSE },
  { tok::kw_OF },
  { tok::semi, tok::kw_END, tok::r_paren, tok::comma, tok::pipe, tok::kw_ELSE, tok::kw_DO },
  { tok::comma, tok::kw_OF },
  { tok::semi, tok::kw_END, tok::comma, tok::kw_OF, tok::pipe, tok::kw_ELSE },
  { tok::semi, tok::r_paren, tok::colon },
  { tok::semi, tok::r_paren, tok::comma },
  { tok::kw_END, tok::pipe, tok::kw_ELSE },
  { tok::identifier },
  { tok::kw_END, tok::kw_FINALLY },
  { tok::kw_END, tok::kw_FINALLY, tok::kw_EXCEPT },
  { tok::semi, tok::kw_END, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::kw_ELSIF, tok::kw_UNTIL },
  { tok::kw_END, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::kw_ELSIF, tok::kw_UNTIL },
  { tok::kw_END, tok::kw_ELSE },
  { tok::semi, tok::r_square, tok::equal, tok::kw_END, tok::l_paren, tok::r_paren, tok::colon, tok::comma, tok::kw_OF, tok::kw_TO, tok::ellipsis, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::colonequal, tok::kw_DO, tok::kw_THEN, tok::kw_ELSIF, tok::kw_UNTIL, tok::kw_BY, tok::plus, tok::minus, tok::hash, tok::less, tok::greater, tok::lessequal, tok::greaterequal, tok::kw_IN, tok::kw_OR, tok::star, tok::slash, tok::kw_REM, tok::kw_DIV, tok::kw_MOD, tok::kw_AND, tok::r_brace, tok::kw_AS },
  { tok::semi, tok::r_square, tok::kw_END, tok::r_paren, tok::colon, tok::comma, tok::kw_OF, tok::kw_TO, tok::ellipsis, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::kw_DO, tok::kw_THEN, tok::kw_ELSIF, tok::kw_UNTIL, tok::kw_BY, tok::r_brace, tok::kw_AS },
  { tok::semi, tok::r_square, tok::equal, tok::kw_END, tok::r_paren, tok::colon, tok::comma, tok::kw_OF, tok::kw_TO, tok::ellipsis, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::kw_DO, tok::kw_THEN, tok::kw_ELSIF, tok::kw_UNTIL, tok::kw_BY, tok::hash, tok::less, tok::greater, tok::lessequal, tok::greaterequal, tok::kw_IN, tok::r_brace, tok::kw_AS },
  { tok::semi, tok::r_square, tok::equal, tok::kw_END, tok::r_paren, tok::colon, tok::comma, tok::kw_OF, tok::kw_TO, tok::ellipsis, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::kw_DO, tok::kw_THEN, tok::kw_ELSIF, tok::kw_UNTIL, tok::kw_BY, tok::plus, tok::minus, tok::hash, tok::less, tok::greater, tok::lessequal, tok::greaterequal, tok::kw_IN, tok::kw_OR, tok::r_brace, tok::kw_AS },
  { tok::semi, tok::r_square, tok::equal, tok::kw_END, tok::r_paren, tok::colon, tok::comma, tok::kw_OF, tok::kw_TO, tok::ellipsis, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::kw_DO, tok::kw_THEN, tok::kw_ELSIF, tok::kw_UNTIL, tok::kw_BY, tok::plus, tok::minus, tok::hash, tok::less, tok::greater, tok::lessequal, tok::greaterequal, tok::kw_IN, tok::kw_OR, tok::star, tok::slash, tok::kw_REM, tok::kw_DIV, tok::kw_MOD, tok::kw_AND, tok::r_brace, tok::kw_AS },
  { tok::r_square, tok::comma, tok::kw_OF, tok::kw_BY, tok::r_brace },
  { tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT },
  { tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::kw_NOT },
  { tok::semi, tok::kw_END, tok::l_paren, tok::pipe, tok::kw_ELSE, tok::kw_FINALLY, tok::kw_EXCEPT, tok::colonequal, tok::kw_DO, tok::kw_ELSIF, tok::kw_UNTIL },
  { tok::comma, tok::r_brace },
  { tok::colon, tok::comma, tok::ellipsis, tok::r_brace },
  { tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_FORWARD, tok::kw_BEGIN, tok::kw_OVERRIDE, tok::kw_REVEAL, tok::kw_INHERIT },
  { tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_FORWARD, tok::kw_BEGIN, tok::kw_ABSTRACT, tok::kw_OVERRIDE, tok::kw_REVEAL, tok::kw_INHERIT },
  { tok::semi, tok::period },
  { tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_OVERRIDE },
  { tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_ABSTRACT, tok::kw_OVERRIDE },
  { tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_OVERRIDE },
  { tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_ABSTRACT, tok::kw_OVERRIDE },
  { tok::semi, tok::comma },
  { tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_ABSTRACT, tok::kw_OVERRIDE, tok::kw_REVEAL },
  { tok::semi, tok::kw_DO },
  { tok::kw_AS },
  { tok::kw_DO },
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
bool M2Parser::parseProgramModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool IsUnsafeGuarded) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  if (consume(tok::kw_MODULE))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  ImplementationModule *M = Actions.actOnCompilationModule<ImplementationModule>(tokenAs<Identifier>(Tok), IsUnsafeGuarded);
  EnterDeclScope S(Actions, M);
  Expression *Protection = nullptr;
  ImportItemList Imports;
  advance();
  if (Tok.is(tok::l_square)) {
    if (M2Parser::parseProtection(__FollowSet, Protection))
      return __errorhandler();
  }
  if (consume(tok::semi))
    return __errorhandler();
  if (M2Parser::parseImportLists(__FollowSet, Imports))
    return __errorhandler();
  DeclarationList Decls; Block InitBlk, FinalBlk;
  if (M2Parser::parseModuleBlock(__FollowSet, Decls, InitBlk, FinalBlk))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  Actions.actOnImplementationModule(M, tokenAs<Identifier>(Tok), Protection, Decls, InitBlk, FinalBlk, true);
  advance();
  if (expect(tok::period))
    return __errorhandler();
  CM = M;
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
bool M2Parser::parseDefinitionModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool IsUnsafeGuarded) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  if (consume(tok::kw_DEFINITION))
    return __errorhandler();
  if (consume(tok::kw_MODULE))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  Identifier ModuleName = tokenAs<Identifier>(Tok);
  advance();
  if (Tok.is(tok::equal)) {
    if (M2Parser::parseRefiningDefinitionModuleTail(__FollowSet, CM, IsUnsafeGuarded, ModuleName))
      return __errorhandler();
  }
  else if ((__TokenBitSet{tok::kw_END, tok::kw_IMPORT, tok::kw_FROM, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT, }).contains(Tok.getKind())) {
    if (M2Parser::parseDefinitionModuleTail(__FollowSet, CM, IsUnsafeGuarded, ModuleName))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseRefiningDefinitionModuleTail(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool IsUnsafeGuarded, Identifier ModuleName) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  if (!(getLangOpts().ISOGenerics)) {
    error();
    return __errorhandler();
  }
  RefiningDefinitionModule *M = Actions.actOnCompilationModule<RefiningDefinitionModule>(ModuleName, IsUnsafeGuarded);
  EnterDeclScope S(Actions, M);
  if (consume(tok::equal))
    return __errorhandler();
  if (M2Parser::parseGenericSeparateModuleIdentifier(__FollowSet))
    return __errorhandler();
  ActualParameterList ActualModulParams;
  if (Tok.is(tok::l_paren)) {
    if (M2Parser::parseActualModuleParameters(__FollowSet, ActualModulParams))
      return __errorhandler();
  }
  if (consume(tok::semi))
    return __errorhandler();
  if (consume(tok::kw_END))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  Actions.actOnRefiningDefinitionModule(M, tokenAs<Identifier>(Tok), ActualModulParams);
  advance();
  if (expect(tok::period))
    return __errorhandler();
  CM = M;
  advance();
  return false;
}
bool M2Parser::parseDefinitionModuleTail(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool IsUnsafeGuarded, Identifier ModuleName) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  DefinitionModule *M = Actions.actOnCompilationModule<DefinitionModule>(ModuleName, IsUnsafeGuarded);
  EnterDeclScope S(Actions, M);
  DeclarationList Decls;
  ImportItemList Imports;
  if (M2Parser::parseImportLists(__FollowSet, Imports))
    return __errorhandler();
  if (M2Parser::parseDefinitions(__FollowSet, Decls))
    return __errorhandler();
  if (consume(tok::kw_END))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  Actions.actOnDefinitionModule(M, tokenAs<Identifier>(Tok), Decls);
  advance();
  if (expect(tok::period))
    return __errorhandler();
  CM = M;
  advance();
  return false;
}
bool M2Parser::parseImplementationModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool IsUnsafeGuarded) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  if (consume(tok::kw_IMPLEMENTATION))
    return __errorhandler();
  if (consume(tok::kw_MODULE))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  Identifier ModuleName = tokenAs<Identifier>(Tok);
  advance();
  if (Tok.is(tok::equal)) {
    if (M2Parser::parseRefiningImplementationModuleTail(__FollowSet, CM, IsUnsafeGuarded, ModuleName))
      return __errorhandler();
  }
  else if ((__TokenBitSet{tok::semi, tok::l_square, }).contains(Tok.getKind())) {
    if (M2Parser::parseImplementationModuleTail(__FollowSet, CM, IsUnsafeGuarded, ModuleName))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseRefiningImplementationModuleTail(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool IsUnsafeGuarded, Identifier ModuleName) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  if (!(getLangOpts().ISOGenerics)) {
    error();
    return __errorhandler();
  }
  RefiningImplementationModule *M = Actions.actOnCompilationModule<RefiningImplementationModule>(ModuleName, IsUnsafeGuarded);
  EnterDeclScope S(Actions, M);
  if (consume(tok::equal))
    return __errorhandler();
  if (M2Parser::parseGenericSeparateModuleIdentifier(__FollowSet))
    return __errorhandler();
  ActualParameterList ActualModulParams;
  if (Tok.is(tok::l_paren)) {
    if (M2Parser::parseActualModuleParameters(__FollowSet, ActualModulParams))
      return __errorhandler();
  }
  if (consume(tok::semi))
    return __errorhandler();
  if (consume(tok::kw_END))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  Actions.actOnRefiningImplementationModule(M, tokenAs<Identifier>(Tok), ActualModulParams);
  advance();
  if (expect(tok::period))
    return __errorhandler();
  CM = M;
  advance();
  return false;
}
bool M2Parser::parseImplementationModuleTail(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM, bool IsUnsafeGuarded, Identifier ModuleName) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  ImplementationModule *M = Actions.actOnCompilationModule<ImplementationModule>(ModuleName, IsUnsafeGuarded);
  EnterDeclScope S(Actions, M);
  Expression *Protection = nullptr;
  ImportItemList Imports;
  if (Tok.is(tok::l_square)) {
    if (M2Parser::parseProtection(__FollowSet, Protection))
      return __errorhandler();
  }
  if (consume(tok::semi))
    return __errorhandler();
  if (M2Parser::parseImportLists(__FollowSet, Imports))
    return __errorhandler();
  DeclarationList Decls; Block InitBlk, FinalBlk;
  if (M2Parser::parseModuleBlock(__FollowSet, Decls, InitBlk, FinalBlk))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  Actions.actOnImplementationModule(M, tokenAs<Identifier>(Tok), Protection, Decls, InitBlk, FinalBlk, true);
  advance();
  if (expect(tok::period))
    return __errorhandler();
  CM = M;
  advance();
  return false;
}
bool M2Parser::parseImportLists(const __TokenBitSet &__FollowSetCallers, ImportItemList &Imports) {
  const __TokenBitSet __FollowSet = __FollowSets[3] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[3]);
  };
  while ((__TokenBitSet{tok::kw_IMPORT, tok::kw_FROM, }).contains(Tok.getKind())) {
    if (M2Parser::parseImportList(__FollowSet, Imports))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseImportList(const __TokenBitSet &__FollowSetCallers, ImportItemList &Imports) {
  const __TokenBitSet __FollowSet = __FollowSets[4] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[4]);
  };
  if (Tok.is(tok::kw_IMPORT)) {
    if (M2Parser::parseSimpleImport(__FollowSet, Imports))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_FROM)) {
    if (M2Parser::parseUnqualifiedImport(__FollowSet, Imports))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseSimpleImport(const __TokenBitSet &__FollowSetCallers, ImportItemList &Imports) {
  const __TokenBitSet __FollowSet = __FollowSets[4] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[4]);
  };
  IdentifierList IdentList;
  if (consume(tok::kw_IMPORT))
    return __errorhandler();
  if (M2Parser::parseIdentifierList(__FollowSet, IdentList))
    return __errorhandler();
  Actions.actOnSimpleImport(Imports, IdentList);
  if (consume(tok::semi))
    return __errorhandler();
  return false;
}
bool M2Parser::parseUnqualifiedImport(const __TokenBitSet &__FollowSetCallers, ImportItemList &Imports) {
  const __TokenBitSet __FollowSet = __FollowSets[4] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[4]);
  };
  IdentifierList IdentList;
  if (consume(tok::kw_FROM))
    return __errorhandler();
  if (M2Parser::parseModuleIdentifier(__FollowSet))
    return __errorhandler();
  if (consume(tok::kw_IMPORT))
    return __errorhandler();
  if (M2Parser::parseIdentifierList(__FollowSet, IdentList))
    return __errorhandler();
  if (consume(tok::semi))
    return __errorhandler();
  return false;
}
bool M2Parser::parseExportList(const __TokenBitSet &__FollowSetCallers, LocalModule *LM) {
  const __TokenBitSet __FollowSet = __FollowSets[5] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[5]);
  };
  IdentifierList IdentList; bool Qualified = false;
  if (consume(tok::kw_EXPORT))
    return __errorhandler();
  if (Tok.is(tok::kw_QUALIFIED)) {
    advance();
  }
  if (M2Parser::parseIdentifierList(__FollowSet, IdentList))
    return __errorhandler();
  if (expect(tok::semi))
    return __errorhandler();
  Actions.actOnExportList(LM, IdentList, Qualified);
  advance();
  return false;
}
bool M2Parser::parseQualifiedIdentifier(const __TokenBitSet &__FollowSetCallers, Declaration *&Decl) {
  const __TokenBitSet __FollowSet = __FollowSets[6] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[6]);
  };
  while (Tok.is(tok::identifier) && (Actions.isModule(Tok.getIdentifier()))) {
    Decl = Actions.actOnQualifiedIdentifier(Decl, tokenAs<Identifier>(Tok));
    advance();
    if (consume(tok::period))
      return __errorhandler();
  }
  if (Tok.is(tok::identifier) && (getLangOpts().ISOObjects && Actions.isClass(Tok.getIdentifier()))) {
    Decl = Actions.actOnQualifiedIdentifier(Decl, tokenAs<Identifier>(Tok));
    advance();
    if (consume(tok::period))
      return __errorhandler();
  }
  if (expect(tok::identifier))
    return __errorhandler();
  Decl = Actions.actOnQualifiedIdentifier(Decl, tokenAs<Identifier>(Tok));
  advance();
  return false;
}
bool M2Parser::parseGenericDefinitionModule(const __TokenBitSet &__FollowSetCallers, CompilationModule *&CM) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  DeclarationList Decls;
  ImportItemList Imports;
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
  if (M2Parser::parseImportLists(__FollowSet, Imports))
    return __errorhandler();
  if (M2Parser::parseDefinitions(__FollowSet, Decls))
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
  ImportItemList Imports;
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
  if (M2Parser::parseImportLists(__FollowSet, Imports))
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
  IdentifierList IdentList;
  TypeDenoter *FT = nullptr;
  if (M2Parser::parseIdentifierList(__FollowSet, IdentList))
    return __errorhandler();
  if (consume(tok::colon))
    return __errorhandler();
  if ((__TokenBitSet{tok::identifier, tok::kw_ARRAY, }).contains(Tok.getKind())) {
    if (M2Parser::parseFormalType(__FollowSet, FT))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_TYPE)) {
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseActualModuleParameters(const __TokenBitSet &__FollowSetCallers, ActualParameterList &Params) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::l_paren))
    return __errorhandler();
  if (M2Parser::parseActualModuleParameterList(__FollowSet, Params))
    return __errorhandler();
  if (consume(tok::r_paren))
    return __errorhandler();
  return false;
}
bool M2Parser::parseActualModuleParameterList(const __TokenBitSet &__FollowSetCallers, ActualParameterList &Params) {
  const __TokenBitSet __FollowSet = __FollowSets[8] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[8]);
  };
  if (M2Parser::parseActualModuleParameter(__FollowSet, Params))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (M2Parser::parseActualModuleParameter(__FollowSet, Params))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseActualModuleParameter(const __TokenBitSet &__FollowSetCallers, ActualParameterList &Params) {
  const __TokenBitSet __FollowSet = __FollowSets[10] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[10]);
  };
  Expression *E = nullptr;
  if (M2Parser::parseExpression(__FollowSet, E))
    return __errorhandler();
  Actions.actOnActualParameter(Params, E);
  return false;
}
bool M2Parser::parseDefinitions(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[11] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[11]);
  };
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
      if (M2Parser::parseProcedureHeading(__FollowSet, Decls))
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
bool M2Parser::parseProcedureHeading(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::kw_PROCEDURE))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  Procedure *P = Actions.actOnProcedure(tokenAs<Identifier>(Tok));
  EnterDeclScope S(Actions, P);
  FormalParameterList Params;
  Type *ResultType = nullptr;
  advance();
  if (Tok.is(tok::l_paren)) {
    if (M2Parser::parseFormalParameters(__FollowSet, Params))
      return __errorhandler();
    if (Tok.is(tok::colon)) {
      advance();
      if (M2Parser::parseFunctionResultType(__FollowSet, ResultType))
        return __errorhandler();
    }
  }
  Actions.actOnProcedureHeading(Decls, P, Params, ResultType);
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
bool M2Parser::parseFormalParameters(const __TokenBitSet &__FollowSetCallers, FormalParameterList &Params) {
  const __TokenBitSet __FollowSet = __FollowSets[12] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[12]);
  };
  if (consume(tok::l_paren))
    return __errorhandler();
  if ((__TokenBitSet{tok::identifier, tok::kw_VAR, }).contains(Tok.getKind())) {
    if (M2Parser::parseFormalParameterList(__FollowSet, Params))
      return __errorhandler();
  }
  if (consume(tok::r_paren))
    return __errorhandler();
  return false;
}
bool M2Parser::parseFormalParameterList(const __TokenBitSet &__FollowSetCallers, FormalParameterList &Params) {
  const __TokenBitSet __FollowSet = __FollowSets[8] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[8]);
  };
  if (M2Parser::parseFormalParameter(__FollowSet, Params))
    return __errorhandler();
  while (Tok.is(tok::semi)) {
    advance();
    if (M2Parser::parseFormalParameter(__FollowSet, Params))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseFunctionResultType(const __TokenBitSet &__FollowSetCallers, Type *&Ty) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (M2Parser::parseTypeIdentifier(__FollowSet, Ty))
    return __errorhandler();
  return false;
}
bool M2Parser::parseFormalParameter(const __TokenBitSet &__FollowSetCallers, FormalParameterList &Params) {
  const __TokenBitSet __FollowSet = __FollowSets[9] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[9]);
  };
  bool IsCallByReference = false;
  IdentifierList IdentList;
  TypeDenoter *FT = nullptr;
  if (Tok.is(tok::kw_VAR)) {
    IsCallByReference = true;
    advance();
  }
  if (M2Parser::parseIdentifierList(__FollowSet, IdentList))
    return __errorhandler();
  if (consume(tok::colon))
    return __errorhandler();
  if (M2Parser::parseFormalType(__FollowSet, FT))
    return __errorhandler();
  Actions.actOnFormalParameter(Params, IdentList, IsCallByReference, FT);
  return false;
}
bool M2Parser::parseDeclarations(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
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
      if (M2Parser::parseProcedureDeclaration(__FollowSet, Decls))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
    else if ((__TokenBitSet{tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT, }).contains(Tok.getKind()) && (getLangOpts().ISOObjects)) {
      if (M2Parser::parseClassDeclaration(__FollowSet, Decls))
        return __errorhandler();
      if (consume(tok::semi))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_MODULE)) {
      if (M2Parser::parseLocalModuleDeclaration(__FollowSet, Decls))
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
  if (expect(tok::identifier))
    return __errorhandler();
  Identifier ConstName = tokenAs<Identifier>(Tok);
  advance();
  if (expect(tok::equal))
    return __errorhandler();
  Expression *E = nullptr;
  advance();
  if (M2Parser::parseExpression(__FollowSet, E))
    return __errorhandler();
  Actions.actOnConstant(Decls, ConstName, E);
  return false;
}
bool M2Parser::parseTypeDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (expect(tok::identifier))
    return __errorhandler();
  Identifier TypeName = tokenAs<Identifier>(Tok);
  advance();
  if (expect(tok::equal))
    return __errorhandler();
  TypeDenoter *TyDen = nullptr;
  advance();
  if (M2Parser::parseTypeDenoter(__FollowSet, TyDen))
    return __errorhandler();
  Actions.actOnType(Decls, TypeName, TyDen);
  return false;
}
bool M2Parser::parseVariableDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  VariableIdentifierList VarIdList;
  TypeDenoter *TyDen = nullptr;
  if (M2Parser::parseVariableIdentifierList(__FollowSet, VarIdList))
    return __errorhandler();
  if (consume(tok::colon))
    return __errorhandler();
  if (M2Parser::parseTypeDenoter(__FollowSet, TyDen))
    return __errorhandler();
  Actions.actOnVariable(Decls, VarIdList, TyDen);
  return false;
}
bool M2Parser::parseVariableIdentifierList(const __TokenBitSet &__FollowSetCallers, VariableIdentifierList &VarIdList) {
  const __TokenBitSet __FollowSet = __FollowSets[14] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[14]);
  };
  if (expect(tok::identifier))
    return __errorhandler();
  Identifier Id = tokenAs<Identifier>(Tok);
  Expression *Addr = nullptr;
  advance();
  if (Tok.is(tok::l_square)) {
    if (M2Parser::parseMachineAddress(__FollowSet, Addr))
      return __errorhandler();
  }
  VarIdList.push_back(std::pair<Identifier, Expression *>(Id, Addr));
  while (Tok.is(tok::comma)) {
    advance();
    if (expect(tok::identifier))
      return __errorhandler();
    Identifier Id = tokenAs<Identifier>(Tok);
    Expression *Addr = nullptr;
    advance();
    if (Tok.is(tok::l_square)) {
      if (M2Parser::parseMachineAddress(__FollowSet, Addr))
        return __errorhandler();
    }
    VarIdList.push_back(std::pair<Identifier, Expression *>(Id, Addr));
  }
  return false;
}
bool M2Parser::parseMachineAddress(const __TokenBitSet &__FollowSetCallers, Expression *&Addr) {
  const __TokenBitSet __FollowSet = __FollowSets[15] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[15]);
  };
  if (consume(tok::l_square))
    return __errorhandler();
  if (M2Parser::parseExpression(__FollowSet, Addr))
    return __errorhandler();
  if (consume(tok::r_square))
    return __errorhandler();
  return false;
}
bool M2Parser::parseProcedureDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::kw_PROCEDURE))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  Procedure *P = Actions.actOnProcedure(tokenAs<Identifier>(Tok));
  EnterDeclScope S(Actions, P);
  bool IsFunction = false;
  FormalParameterList Params;
  Type *ResultType = nullptr;
  advance();
  if (Tok.is(tok::l_paren)) {
    advance();
    if ((__TokenBitSet{tok::identifier, tok::kw_VAR, }).contains(Tok.getKind())) {
      if (M2Parser::parseFormalParameterList(__FollowSet, Params))
        return __errorhandler();
    }
    if (consume(tok::r_paren))
      return __errorhandler();
    if (Tok.is(tok::colon)) {
      IsFunction=true;
      advance();
      if (M2Parser::parseTypeIdentifier(__FollowSet, ResultType))
        return __errorhandler();
    }
  }
  if (expect(tok::semi))
    return __errorhandler();
  Actions.actOnProcedureHeading(Decls, P, Params, ResultType);
  advance();
  if ((__TokenBitSet{tok::kw_MODULE, tok::kw_END, tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_BEGIN, tok::kw_TRACED, tok::kw_CLASS, tok::kw_ABSTRACT, }).contains(Tok.getKind())) {
    DeclarationList ProcDecls; Block Body;
    if (M2Parser::parseProperProcedureBlock(__FollowSet, ProcDecls, Body, IsFunction))
      return __errorhandler();
    if (expect(tok::identifier))
      return __errorhandler();
    Actions.actOnProcedure(P, tokenAs<Identifier>(Tok), ProcDecls, Body, IsFunction);
    advance();
  }
  else if (Tok.is(tok::kw_FORWARD)) {
    Actions.actOnForwardProcedure(Decls, P);
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseLocalModuleDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::kw_MODULE))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  LocalModule *LM = Actions.actOnLocalModule(tokenAs<Identifier>(Tok));
  EnterDeclScope S(Actions, LM);
  DeclarationList ModDecls; Block InitBlk, FinalBlk;
  Expression *Protection = nullptr;
  ImportItemList Imports;
  advance();
  if (Tok.is(tok::equal) && (getLangOpts().ISOGenerics)) {
    if (!(getLangOpts().ISOGenerics)) {
      error();
      return __errorhandler();
    }
    advance();
    if (M2Parser::parseGenericSeparateModuleIdentifier(__FollowSet))
      return __errorhandler();
    ActualParameterList ActualModulParams;
    if (Tok.is(tok::l_paren)) {
      if (M2Parser::parseActualModuleParameters(__FollowSet, ActualModulParams))
        return __errorhandler();
    }
    if (consume(tok::semi))
      return __errorhandler();
    if (Tok.is(tok::kw_EXPORT)) {
      if (M2Parser::parseExportList(__FollowSet, LM))
        return __errorhandler();
    }
    if (consume(tok::kw_END))
      return __errorhandler();
  }
  else if ((__TokenBitSet{tok::semi, tok::l_square, }).contains(Tok.getKind())) {
    if (Tok.is(tok::l_square)) {
      if (M2Parser::parseProtection(__FollowSet, Protection))
        return __errorhandler();
    }
    if (consume(tok::semi))
      return __errorhandler();
    if (M2Parser::parseImportLists(__FollowSet, Imports))
      return __errorhandler();
    if (Tok.is(tok::kw_EXPORT)) {
      if (M2Parser::parseExportList(__FollowSet, LM))
        return __errorhandler();
    }
    if (M2Parser::parseModuleBlock(__FollowSet, ModDecls, InitBlk, FinalBlk))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  if (expect(tok::identifier))
    return __errorhandler();
  Actions.actOnLocalModule(LM, tokenAs<Identifier>(Tok), Protection, ModDecls, InitBlk, FinalBlk);
                                Decls.push_back(LM);
  advance();
  return false;
}
bool M2Parser::parseTypeDenoter(const __TokenBitSet &__FollowSetCallers, TypeDenoter *&TyDen) {
  const __TokenBitSet __FollowSet = __FollowSets[16] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[16]);
  };
  if (Tok.is(tok::identifier)) {
    Declaration *Decl = nullptr;
    if (M2Parser::parseQualifiedIdentifier(__FollowSet, Decl))
      return __errorhandler();
    if (Tok.is(tok::l_square)) {
      if (M2Parser::parseSubrangeTypeTail(__FollowSet, TyDen, Decl))
        return __errorhandler();
    }
    else {
      TyDen = Actions.actOnTypeIdentifier(SMLoc(), Decl);
    }
  }
  else if (Tok.is(tok::l_square)) {
    if (M2Parser::parseSubrangeTypeTail(__FollowSet, TyDen, nullptr))
      return __errorhandler();
  }
  else if (Tok.is(tok::l_paren)) {
    if (M2Parser::parseEnumerationType(__FollowSet, TyDen))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_SET)) {
    advance();
    if (consume(tok::kw_OF))
      return __errorhandler();
    if (M2Parser::parseTypeDenoter(__FollowSet, TyDen))
      return __errorhandler();
    TyDen = Actions.actOnSetType(TyDen, false);
  }
  else if (Tok.is(tok::kw_PACKEDSET)) {
    advance();
    if (consume(tok::kw_OF))
      return __errorhandler();
    if (M2Parser::parseTypeDenoter(__FollowSet, TyDen))
      return __errorhandler();
    TyDen = Actions.actOnSetType(TyDen, true);
  }
  else if (Tok.is(tok::kw_POINTER)) {
    advance();
    if (consume(tok::kw_TO))
      return __errorhandler();
    if (Tok.is(tok::identifier) && (Actions.isUndeclared(Tok.getIdentifier()))) {
      if (!(Actions.isUndeclared(Tok.getIdentifier()))) {
        error();
        return __errorhandler();
      }
      TyDen = Actions.actOnPointerType(tokenAs<Identifier>(Tok));
      advance();
    }
    else if ((__TokenBitSet{tok::identifier, tok::l_square, tok::l_paren, tok::kw_PROCEDURE, tok::kw_SET, tok::kw_PACKEDSET, tok::kw_POINTER, tok::kw_ARRAY, tok::kw_RECORD, }).contains(Tok.getKind())) {
      if (M2Parser::parseTypeDenoter(__FollowSet, TyDen))
        return __errorhandler();
      TyDen = Actions.actOnPointerType(TyDen);
    }
    else {
      error();
      return __errorhandler();
    }
  }
  else if (Tok.is(tok::kw_PROCEDURE)) {
    if (M2Parser::parseProcedureType(__FollowSet, TyDen))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_ARRAY)) {
    TypeDenoterList IndexTypeList;
    advance();
    if (M2Parser::parseIndexTypeList(__FollowSet, IndexTypeList))
      return __errorhandler();
    if (consume(tok::kw_OF))
      return __errorhandler();
    if (M2Parser::parseTypeDenoter(__FollowSet, TyDen))
      return __errorhandler();
    TyDen = Actions.actOnArrayType(TyDen, IndexTypeList);
  }
  else if (Tok.is(tok::kw_RECORD)) {
    RecordFieldList Fields;
    advance();
    if (M2Parser::parseFieldList(__FollowSet, Fields))
      return __errorhandler();
    if (expect(tok::kw_END))
      return __errorhandler();
    TyDen = Actions.actOnRecordType(Fields);
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseIndexTypeList(const __TokenBitSet &__FollowSetCallers, TypeDenoterList &TyDens) {
  const __TokenBitSet __FollowSet = __FollowSets[17] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[17]);
  };
  TypeDenoter *TyDen = nullptr;
  if (M2Parser::parseOrdinalTypeDenoter(__FollowSet, TyDen))
    return __errorhandler();
  TyDens.push_back(TyDen);
  while (Tok.is(tok::comma)) {
    TyDen = nullptr;
    advance();
    if (M2Parser::parseOrdinalTypeDenoter(__FollowSet, TyDen))
      return __errorhandler();
    TyDens.push_back(TyDen);
  }
  return false;
}
bool M2Parser::parseTypeIdentifier(const __TokenBitSet &__FollowSetCallers, Type *&Ty) {
  const __TokenBitSet __FollowSet = __FollowSets[18] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[18]);
  };
  Declaration *Decl = nullptr;
  if (M2Parser::parseQualifiedIdentifier(__FollowSet, Decl))
    return __errorhandler();
  Ty = Actions.actOnTypeIdentifier(Decl);
  return false;
}
bool M2Parser::parseOrdinalTypeIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[17] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[17]);
  };
  Declaration *Decl = nullptr;
  if (M2Parser::parseQualifiedIdentifier(__FollowSet, Decl))
    return __errorhandler();
  Actions.actOnOrdinalTypeIdentifier(Decl);
  return false;
}
bool M2Parser::parseOrdinalTypeDenoter(const __TokenBitSet &__FollowSetCallers, TypeDenoter *&TyDen) {
  const __TokenBitSet __FollowSet = __FollowSets[19] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[19]);
  };
  if (Tok.is(tok::identifier)) {
    Declaration *Decl = nullptr;
    if (M2Parser::parseQualifiedIdentifier(__FollowSet, Decl))
      return __errorhandler();
    if (Tok.is(tok::l_square)) {
      if (M2Parser::parseSubrangeTypeTail(__FollowSet, TyDen, Decl))
        return __errorhandler();
    }
    else {
      TyDen = Actions.actOnOrdinalTypeIdentifier(Decl);
    }
  }
  else if (Tok.is(tok::l_square)) {
    if (M2Parser::parseSubrangeTypeTail(__FollowSet, TyDen, nullptr))
      return __errorhandler();
  }
  else if (Tok.is(tok::l_paren)) {
    if (M2Parser::parseEnumerationType(__FollowSet, TyDen))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseSubrangeTypeTail(const __TokenBitSet &__FollowSetCallers, TypeDenoter *&TyDen, Declaration *Decl) {
  const __TokenBitSet __FollowSet = __FollowSets[20] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[20]);
  };
  if (expect(tok::l_square))
    return __errorhandler();
  Expression *From = nullptr, *To = nullptr;
  advance();
  if (M2Parser::parseExpression(__FollowSet, From))
    return __errorhandler();
  if (consume(tok::ellipsis))
    return __errorhandler();
  if (M2Parser::parseExpression(__FollowSet, To))
    return __errorhandler();
  if (expect(tok::r_square))
    return __errorhandler();
  TyDen = Actions.actOnSubrangeType(Decl, From, To);
  advance();
  return false;
}
bool M2Parser::parseEnumerationType(const __TokenBitSet &__FollowSetCallers, TypeDenoter *&TyDen) {
  const __TokenBitSet __FollowSet = __FollowSets[20] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[20]);
  };
  if (expect(tok::l_paren))
    return __errorhandler();
  IdentifierList IdentList;
  advance();
  if (M2Parser::parseIdentifierList(__FollowSet, IdentList))
    return __errorhandler();
  TyDen = Actions.actOnEnumerationType(IdentList);
  if (consume(tok::r_paren))
    return __errorhandler();
  return false;
}
bool M2Parser::parseIdentifierList(const __TokenBitSet &__FollowSetCallers, IdentifierList &IdentList) {
  const __TokenBitSet __FollowSet = __FollowSets[21] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[21]);
  };
  if (expect(tok::identifier))
    return __errorhandler();
  IdentList.push_back(tokenAs<Identifier>(Tok));
  advance();
  while (Tok.is(tok::comma)) {
    advance();
    if (expect(tok::identifier))
      return __errorhandler();
    IdentList.push_back(tokenAs<Identifier>(Tok));
    advance();
  }
  return false;
}
bool M2Parser::parseProcedureType(const __TokenBitSet &__FollowSetCallers, TypeDenoter *&TyDen) {
  const __TokenBitSet __FollowSet = __FollowSets[16] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[16]);
  };
  if (expect(tok::kw_PROCEDURE))
    return __errorhandler();
  Type *ResultType = nullptr;
  FormalParameterTypeList FPTList;
  advance();
  if (Tok.is(tok::l_paren)) {
    advance();
    if ((__TokenBitSet{tok::identifier, tok::kw_VAR, tok::kw_ARRAY, }).contains(Tok.getKind())) {
      if (M2Parser::parseFormalParameterTypeList(__FollowSet, FPTList))
        return __errorhandler();
    }
    if (consume(tok::r_paren))
      return __errorhandler();
    if (Tok.is(tok::colon)) {
      advance();
      if (M2Parser::parseTypeIdentifier(__FollowSet, ResultType))
        return __errorhandler();
    }
  }
  TyDen = Actions.actOnProcedureType(ResultType, FPTList);
  return false;
}
bool M2Parser::parseFormalParameterTypeList(const __TokenBitSet &__FollowSetCallers, FormalParameterTypeList &FPTList) {
  const __TokenBitSet __FollowSet = __FollowSets[8] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[8]);
  };
  if (M2Parser::parseFormalParameterType(__FollowSet, FPTList))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (M2Parser::parseFormalParameterType(__FollowSet, FPTList))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseFormalParameterType(const __TokenBitSet &__FollowSetCallers, FormalParameterTypeList &FPTList) {
  const __TokenBitSet __FollowSet = __FollowSets[10] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[10]);
  };
  SMLoc Loc = Tok.getLocation();
  bool IsVar = false;
  if (Tok.is(tok::kw_VAR)) {
    IsVar = true;
    advance();
  }
  TypeDenoter *FT = nullptr;
  if (M2Parser::parseFormalType(__FollowSet, FT))
    return __errorhandler();
  Actions.actOnFormalParameterType(FPTList, Loc, IsVar, FT);
  return false;
}
bool M2Parser::parseFormalType(const __TokenBitSet &__FollowSetCallers, TypeDenoter *&FT) {
  const __TokenBitSet __FollowSet = __FollowSets[22] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[22]);
  };
  Type *Ty = nullptr;
  unsigned OpenArrayLevel = 0;
  while (Tok.is(tok::kw_ARRAY)) {
    advance();
    if (expect(tok::kw_OF))
      return __errorhandler();
    ++OpenArrayLevel;
    advance();
  }
  if (M2Parser::parseTypeIdentifier(__FollowSet, Ty))
    return __errorhandler();
  FT = Actions.actOnFormalType(Ty, OpenArrayLevel);
  return false;
}
bool M2Parser::parseFieldList(const __TokenBitSet &__FollowSetCallers, RecordFieldList &Fields) {
  const __TokenBitSet __FollowSet = __FollowSets[23] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[23]);
  };
  if (M2Parser::parseFields(__FollowSet, Fields))
    return __errorhandler();
  while (Tok.is(tok::semi)) {
    advance();
    if (M2Parser::parseFields(__FollowSet, Fields))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseFields(const __TokenBitSet &__FollowSetCallers, RecordFieldList &Fields) {
  const __TokenBitSet __FollowSet = __FollowSets[16] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[16]);
  };
  if ((__TokenBitSet{tok::identifier, tok::kw_CASE, }).contains(Tok.getKind())) {
    if (Tok.is(tok::identifier)) {
      if (M2Parser::parseFixedFields(__FollowSet, Fields))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_CASE)) {
      if (M2Parser::parseVariantFields(__FollowSet))
        return __errorhandler();
    }
  }
  return false;
}
bool M2Parser::parseFixedFields(const __TokenBitSet &__FollowSetCallers, RecordFieldList &Fields) {
  const __TokenBitSet __FollowSet = __FollowSets[16] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[16]);
  };
  IdentifierList IdentList;
  if (M2Parser::parseIdentifierList(__FollowSet, IdentList))
    return __errorhandler();
  if (expect(tok::colon))
    return __errorhandler();
  TypeDenoter *TyDen = nullptr;
  advance();
  if (M2Parser::parseTypeDenoter(__FollowSet, TyDen))
    return __errorhandler();
  Actions.actOnFixedFields(Fields, IdentList, TyDen);
  return false;
}
bool M2Parser::parseVariantFields(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[16] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[16]);
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
  const __TokenBitSet __FollowSet = __FollowSets[14] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[14]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool M2Parser::parseTagType(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[17] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[17]);
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
  RecordFieldList Fields;
  if (consume(tok::kw_ELSE))
    return __errorhandler();
  if (M2Parser::parseFieldList(__FollowSet, Fields))
    return __errorhandler();
  return false;
}
bool M2Parser::parseVariant(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[23] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[23]);
  };
  RecordFieldList Fields;
  if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, }).contains(Tok.getKind())) {
    if (M2Parser::parseVariantLabelList(__FollowSet))
      return __errorhandler();
    if (consume(tok::colon))
      return __errorhandler();
    if (M2Parser::parseFieldList(__FollowSet, Fields))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseVariantLabelList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[14] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[14]);
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
  const __TokenBitSet __FollowSet = __FollowSets[15] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[15]);
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
bool M2Parser::parseProperProcedureBlock(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls, Block &Body, bool IsFunction) {
  const __TokenBitSet __FollowSet = __FollowSets[24] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[24]);
  };
  if (M2Parser::parseDeclarations(__FollowSet, Decls))
    return __errorhandler();
  if (Tok.is(tok::kw_BEGIN)) {
    Actions.actOnBlockBegin();
    advance();
    if (M2Parser::parseBlockBody(__FollowSet, Body))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_END) && (IsFunction)) {
  }
  if (consume(tok::kw_END))
    return __errorhandler();
  return false;
}
bool M2Parser::parseModuleBlock(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls, Block &InitBlk, Block &FinalBlk) {
  const __TokenBitSet __FollowSet = __FollowSets[24] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[24]);
  };
  if (M2Parser::parseDeclarations(__FollowSet, Decls))
    return __errorhandler();
  if (Tok.is(tok::kw_BEGIN)) {
    if (M2Parser::parseModuleBody(__FollowSet, InitBlk, FinalBlk))
      return __errorhandler();
  }
  if (expect(tok::kw_END))
    return __errorhandler();
  Actions.actOnModuleBlockEnd();
  advance();
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
  const __TokenBitSet __FollowSet = __FollowSets[25] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[25]);
  };
  if (expect(tok::kw_BEGIN))
    return __errorhandler();
  Actions.actOnBlockBegin();
  advance();
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
  const __TokenBitSet __FollowSet = __FollowSets[25] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[25]);
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
  const __TokenBitSet __FollowSet = __FollowSets[26] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[26]);
  };
  if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
    return __errorhandler();
  return false;
}
bool M2Parser::parseExceptionalPart(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[25] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[25]);
  };
  if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
    return __errorhandler();
  return false;
}
bool M2Parser::parseStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[27] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[27]);
  };
  if ((__TokenBitSet{tok::identifier, tok::kw_CASE, tok::kw_RETURN, tok::kw_RETRY, tok::kw_WITH, tok::kw_IF, tok::kw_WHILE, tok::kw_REPEAT, tok::kw_LOOP, tok::kw_EXIT, tok::kw_FOR, tok::kw_GUARD, }).contains(Tok.getKind())) {
    if (Tok.is(tok::identifier)) {
      if (M2Parser::parseAssignmentOrProcedireCall(__FollowSet, Stmts))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_RETURN)) {
      if (M2Parser::parseReturnStatement(__FollowSet, Stmts))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_RETRY)) {
      if (M2Parser::parseRetryStatement(__FollowSet, Stmts))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_WITH)) {
      if (M2Parser::parseWithStatement(__FollowSet, Stmts))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_IF)) {
      if (M2Parser::parseIfStatement(__FollowSet, Stmts))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_CASE)) {
      if (M2Parser::parseCaseStatement(__FollowSet, Stmts))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_WHILE)) {
      if (M2Parser::parseWhileStatement(__FollowSet, Stmts))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_REPEAT)) {
      if (M2Parser::parseRepeatStatement(__FollowSet, Stmts))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_LOOP)) {
      if (M2Parser::parseLoopStatement(__FollowSet, Stmts))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_EXIT)) {
      if (M2Parser::parseExitStatement(__FollowSet, Stmts))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_FOR)) {
      if (M2Parser::parseForStatement(__FollowSet, Stmts))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_GUARD) && (getLangOpts().ISOObjects)) {
      if (M2Parser::parseGuardStatement(__FollowSet, Stmts))
        return __errorhandler();
    }
  }
  return false;
}
bool M2Parser::parseStatementSequence(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[28] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[28]);
  };
  if (M2Parser::parseStatement(__FollowSet, Stmts))
    return __errorhandler();
  while (Tok.is(tok::semi)) {
    advance();
    if (M2Parser::parseStatement(__FollowSet, Stmts))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseAssignmentOrProcedireCall(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[27] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[27]);
  };
  Designator *Desig = nullptr;
  if (M2Parser::parseDesignator(__FollowSet, Desig))
    return __errorhandler();
  if (Tok.is(tok::colonequal)) {
    SMLoc Loc = Tok.getLocation();
    Expression *E = nullptr;
    advance();
    if (M2Parser::parseExpression(__FollowSet, E))
      return __errorhandler();
    Actions.actOnAssignmentStmt(Stmts, Loc, Desig, E);
  }
  else {
    if (Tok.is(tok::l_paren)) {
      SMLoc Loc = Tok.getLocation();
      ActualParameterList ActualParameters;
      if (M2Parser::parseActualParameters(__FollowSet, ActualParameters))
        return __errorhandler();
      Actions.actOnProcedureCallStmt(Stmts, Loc, Desig, ActualParameters);
    }
  }
  return false;
}
bool M2Parser::parseReturnStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[27] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[27]);
  };
  if (expect(tok::kw_RETURN))
    return __errorhandler();
  SMLoc Loc = Tok.getLocation();
  Expression *E = nullptr;
  advance();
  if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, }).contains(Tok.getKind())) {
    if (M2Parser::parseExpression(__FollowSet, E))
      return __errorhandler();
  }
  Actions.actOnReturnStmt(Stmts, Loc, E);
  return false;
}
bool M2Parser::parseRetryStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[27] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[27]);
  };
  if (expect(tok::kw_RETRY))
    return __errorhandler();
  SMLoc Loc = Tok.getLocation();
  Actions.actOnRetryStmt(Stmts, Loc);
  advance();
  return false;
}
bool M2Parser::parseWithStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[27] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[27]);
  };
  if (expect(tok::kw_WITH))
    return __errorhandler();
  SMLoc Loc = Tok.getLocation();
  Designator *Desig = nullptr;
  advance();
  if (M2Parser::parseDesignator(__FollowSet, Desig))
    return __errorhandler();
  StatementList WithStmts;
  if (consume(tok::kw_DO))
    return __errorhandler();
  if (M2Parser::parseStatementSequence(__FollowSet, Stmts))
    return __errorhandler();
  Actions.actOnWithStmt(Stmts, Loc, Desig, WithStmts);
  if (consume(tok::kw_END))
    return __errorhandler();
  return false;
}
bool M2Parser::parseIfStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[27] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[27]);
  };
  GuardedStatementList GuardedStmts;
  StatementList ElseStmts;
  if (M2Parser::parseGuardedStatements(__FollowSet, GuardedStmts))
    return __errorhandler();
  if (Tok.is(tok::kw_ELSE)) {
    if (M2Parser::parseIfElsePart(__FollowSet, ElseStmts))
      return __errorhandler();
  }
  Actions.actOnIfStmt(Stmts, GuardedStmts, ElseStmts);
  if (consume(tok::kw_END))
    return __errorhandler();
  return false;
}
bool M2Parser::parseGuardedStatements(const __TokenBitSet &__FollowSetCallers, GuardedStatementList &GuardedStmts) {
  const __TokenBitSet __FollowSet = __FollowSets[29] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[29]);
  };
  if (expect(tok::kw_IF))
    return __errorhandler();
  SMLoc Loc = Tok.getLocation();
  Expression *Cond = nullptr;
  advance();
  if (M2Parser::parseExpression(__FollowSet, Cond))
    return __errorhandler();
  if (expect(tok::kw_THEN))
    return __errorhandler();
  StatementList IfStmts;
  advance();
  if (M2Parser::parseStatementSequence(__FollowSet, IfStmts))
    return __errorhandler();
  Actions.actOnGuardedStmt(GuardedStmts, Loc, Cond, IfStmts);
  while (Tok.is(tok::kw_ELSIF)) {
    SMLoc Loc = Tok.getLocation();
    Expression *Cond = nullptr;
    advance();
    if (M2Parser::parseExpression(__FollowSet, Cond))
      return __errorhandler();
    if (expect(tok::kw_THEN))
      return __errorhandler();
    StatementList ElsIfStmts;
    advance();
    if (M2Parser::parseStatementSequence(__FollowSet, ElsIfStmts))
      return __errorhandler();
    Actions.actOnGuardedStmt(GuardedStmts, Loc, Cond, ElsIfStmts);
  }
  return false;
}
bool M2Parser::parseIfElsePart(const __TokenBitSet &__FollowSetCallers, StatementList &ElseStmts) {
  const __TokenBitSet __FollowSet = __FollowSets[11] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[11]);
  };
  if (consume(tok::kw_ELSE))
    return __errorhandler();
  if (M2Parser::parseStatementSequence(__FollowSet, ElseStmts))
    return __errorhandler();
  return false;
}
bool M2Parser::parseCaseStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[27] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[27]);
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
  const __TokenBitSet __FollowSet = __FollowSets[17] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[17]);
  };
  Expression *E = nullptr;
  if (M2Parser::parseOrdinalExpression(__FollowSet, E))
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
  const __TokenBitSet __FollowSet = __FollowSets[23] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[23]);
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
  const __TokenBitSet __FollowSet = __FollowSets[14] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[14]);
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
  const __TokenBitSet __FollowSet = __FollowSets[15] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[15]);
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
bool M2Parser::parseWhileStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[27] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[27]);
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
  StatementList WhileStmts;
  advance();
  if (M2Parser::parseStatementSequence(__FollowSet, WhileStmts))
    return __errorhandler();
  if (expect(tok::kw_END))
    return __errorhandler();
  Actions.actOnWhileStmt(Stmts, Loc, Cond, WhileStmts);
  advance();
  return false;
}
bool M2Parser::parseRepeatStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[27] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[27]);
  };
  if (expect(tok::kw_REPEAT))
    return __errorhandler();
  SMLoc Loc = Tok.getLocation();
  StatementList RepeatStmts;
  advance();
  if (M2Parser::parseStatementSequence(__FollowSet, RepeatStmts))
    return __errorhandler();
  if (expect(tok::kw_UNTIL))
    return __errorhandler();
  Expression *Cond = nullptr;
  advance();
  if (M2Parser::parseExpression(__FollowSet, Cond))
    return __errorhandler();
  Actions.actOnRepeatStmt(Stmts, Loc, Cond, RepeatStmts);
  return false;
}
bool M2Parser::parseLoopStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[27] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[27]);
  };
  if (expect(tok::kw_LOOP))
    return __errorhandler();
  SMLoc Loc = Tok.getLocation();
  StatementList LoopStmts;
  advance();
  if (M2Parser::parseStatementSequence(__FollowSet, LoopStmts))
    return __errorhandler();
  if (expect(tok::kw_END))
    return __errorhandler();
  Actions.actOnLoopStmt(Stmts, Loc, LoopStmts);
  advance();
  return false;
}
bool M2Parser::parseExitStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[27] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[27]);
  };
  if (expect(tok::kw_EXIT))
    return __errorhandler();
  Actions.actOnExitStmt(Stmts, Tok.getLocation());
  advance();
  return false;
}
bool M2Parser::parseForStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[27] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[27]);
  };
  if (expect(tok::kw_FOR))
    return __errorhandler();
  SMLoc Loc = Tok.getLocation();
  advance();
  if (expect(tok::identifier))
    return __errorhandler();
  Identifier ControlVariable = tokenAs<Identifier>(Tok);
  advance();
  if (expect(tok::colonequal))
    return __errorhandler();
  Expression *InitialValue = nullptr;
  advance();
  if (M2Parser::parseExpression(__FollowSet, InitialValue))
    return __errorhandler();
  Expression *FinalValue = nullptr;
  if (consume(tok::kw_TO))
    return __errorhandler();
  if (M2Parser::parseExpression(__FollowSet, FinalValue))
    return __errorhandler();
  Expression *StepSize = nullptr;
  if (Tok.is(tok::kw_BY)) {
    advance();
    if (M2Parser::parseExpression(__FollowSet, StepSize))
      return __errorhandler();
  }
  StatementList ForStmts;
  if (consume(tok::kw_DO))
    return __errorhandler();
  if (M2Parser::parseStatementSequence(__FollowSet, ForStmts))
    return __errorhandler();
  if (expect(tok::kw_END))
    return __errorhandler();
  Actions.actOnForStmt(Stmts, Loc, ControlVariable, InitialValue, FinalValue, StepSize, ForStmts);
  advance();
  return false;
}
bool M2Parser::parseFieldIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool M2Parser::parseExpression(const __TokenBitSet &__FollowSetCallers, Expression *&E) {
  const __TokenBitSet __FollowSet = __FollowSets[31] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[31]);
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
  const __TokenBitSet __FollowSet = __FollowSets[32] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[32]);
  };
  OperatorInfo PrefixOp;
  if ((__TokenBitSet{tok::plus, tok::minus, }).contains(Tok.getKind())) {
    if (Tok.is(tok::plus)) {
      PrefixOp = tokenAs<OperatorInfo>(Tok);
      advance();
    }
    else if (Tok.is(tok::minus)) {
      PrefixOp = tokenAs<OperatorInfo>(Tok);
      advance();
    }
  }
  if (M2Parser::parseTerm(__FollowSet, E))
    return __errorhandler();
  while ((__TokenBitSet{tok::plus, tok::minus, tok::kw_OR, }).contains(Tok.getKind())) {
    OperatorInfo Op;
    if (M2Parser::parseTermOperator(__FollowSet, Op))
      return __errorhandler();
    Expression *Right = nullptr;
    if (M2Parser::parseTerm(__FollowSet, Right))
      return __errorhandler();
    E = Actions.actOnSimpleExpression(E, Right, Op);
  }
  if (!PrefixOp.isUnspecified()) E = Actions.actOnPrefixOperator(E, PrefixOp);
  return false;
}
bool M2Parser::parseTerm(const __TokenBitSet &__FollowSetCallers, Expression *&E) {
  const __TokenBitSet __FollowSet = __FollowSets[33] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[33]);
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
  const __TokenBitSet __FollowSet = __FollowSets[34] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[34]);
  };
  if (Tok.is(tok::l_paren)) {
    advance();
    if (M2Parser::parseExpression(__FollowSet, E))
      return __errorhandler();
    if (consume(tok::r_paren))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_NOT)) {
    OperatorInfo Op(tokenAs<OperatorInfo>(Tok));
    advance();
    if (M2Parser::parseFactor(__FollowSet, E))
      return __errorhandler();
    E = Actions.actOnNot(E, Op);
  }
  else if (Tok.is(tok::identifier)) {
    Declaration *Decl = nullptr;
    if (M2Parser::parseQualifiedIdentifier(__FollowSet, Decl))
      return __errorhandler();
    if (Tok.is(tok::l_brace)) {
      if (M2Parser::parseValueConstructorTail(__FollowSet))
        return __errorhandler();
    }
    else {
      SelectorList Selectors;
      if (M2Parser::parseDesignatorTail(__FollowSet, Selectors))
        return __errorhandler();
      E = Actions.actOnDesignator(Decl, Selectors);
      if (Tok.is(tok::l_paren)) {
        ActualParameterList ActualParameters;
        if (M2Parser::parseActualParameters(__FollowSet, ActualParameters))
          return __errorhandler();
        E = Actions.actOnFunctionCall(E, ActualParameters);
      }
    }
  }
  else if ((__TokenBitSet{tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, }).contains(Tok.getKind())) {
    if (M2Parser::parseConstantLiteral(__FollowSet, E))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseOrdinalExpression(const __TokenBitSet &__FollowSetCallers, Expression *&E) {
  const __TokenBitSet __FollowSet = __FollowSets[35] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[35]);
  };
  SMLoc Loc = Tok.getLocation();
  if (M2Parser::parseExpression(__FollowSet, E))
    return __errorhandler();
  E = Actions.actOnOrdinalExpression(Loc, E);
  return false;
}
bool M2Parser::parseRelationalOperator(const __TokenBitSet &__FollowSetCallers, OperatorInfo &Op) {
  const __TokenBitSet __FollowSet = __FollowSets[36] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[36]);
  };
  if (Tok.is(tok::equal)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else if (Tok.is(tok::hash)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else if (Tok.is(tok::less)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else if (Tok.is(tok::greater)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else if (Tok.is(tok::lessequal)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else if (Tok.is(tok::greaterequal)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else if (Tok.is(tok::kw_IN)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseTermOperator(const __TokenBitSet &__FollowSetCallers, OperatorInfo &Op) {
  const __TokenBitSet __FollowSet = __FollowSets[37] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[37]);
  };
  if (Tok.is(tok::plus)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else if (Tok.is(tok::minus)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else if (Tok.is(tok::kw_OR)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseFactorOperator(const __TokenBitSet &__FollowSetCallers, OperatorInfo &Op) {
  const __TokenBitSet __FollowSet = __FollowSets[37] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[37]);
  };
  if (Tok.is(tok::star)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else if (Tok.is(tok::slash)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else if (Tok.is(tok::kw_REM)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else if (Tok.is(tok::kw_DIV)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else if (Tok.is(tok::kw_MOD)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else if (Tok.is(tok::kw_AND)) {
    Op = tokenAs<OperatorInfo>(Tok);
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseDesignator(const __TokenBitSet &__FollowSetCallers, Designator *&Desig) {
  const __TokenBitSet __FollowSet = __FollowSets[38] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[38]);
  };
  Declaration *Decl = nullptr;
  SelectorList Selectors;
  if (M2Parser::parseQualifiedIdentifier(__FollowSet, Decl))
    return __errorhandler();
  if (M2Parser::parseDesignatorTail(__FollowSet, Selectors))
    return __errorhandler();
  Desig = Actions.actOnDesignator(Decl, Selectors);
  return false;
}
bool M2Parser::parseDesignatorTail(const __TokenBitSet &__FollowSetCallers, SelectorList &Selectors) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  while ((__TokenBitSet{tok::period, tok::l_square, tok::caret, }).contains(Tok.getKind())) {
    if (Tok.is(tok::l_square)) {
      Expression *E = nullptr;
      advance();
      if (M2Parser::parseOrdinalExpression(__FollowSet, E))
        return __errorhandler();
      Actions.actOnIndexSelector(Selectors, E);
      while (Tok.is(tok::comma)) {
        E = nullptr;
        advance();
        if (M2Parser::parseOrdinalExpression(__FollowSet, E))
          return __errorhandler();
        Actions.actOnIndexSelector(Selectors, E);
      }
      if (consume(tok::r_square))
        return __errorhandler();
    }
    else if (Tok.is(tok::period)) {
      advance();
      if (Tok.is(tok::identifier)) {
        if (M2Parser::parseFieldIdentifier(__FollowSet))
          return __errorhandler();
      }
      else if (Tok.is(tok::period) && (getLangOpts().ISOObjects)) {
        if (!(getLangOpts().ISOObjects)) {
          error();
          return __errorhandler();
        }
        advance();
        if (Tok.is(tok::identifier)) {
          if (M2Parser::parseClassIdentifier(__FollowSet))
            return __errorhandler();
          if (consume(tok::period))
            return __errorhandler();
        }
        if (M2Parser::parseEntityIdentifier(__FollowSet))
          return __errorhandler();
      }
      else {
        error();
        return __errorhandler();
      }
    }
    else if (Tok.is(tok::caret)) {
      Actions.actOnDereferenceSelector(Selectors);
      advance();
    }
  }
  return false;
}
bool M2Parser::parseValueConstructorTail(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[34] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[34]);
  };
  if (consume(tok::l_brace))
    return __errorhandler();
  if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, tok::l_brace, }).contains(Tok.getKind())) {
    if (M2Parser::parseRepeatedStructureComponent(__FollowSet))
      return __errorhandler();
    while (Tok.is(tok::comma)) {
      advance();
      if (M2Parser::parseRepeatedStructureComponent(__FollowSet))
        return __errorhandler();
    }
  }
  if (consume(tok::r_brace))
    return __errorhandler();
  return false;
}
bool M2Parser::parseRepeatedStructureComponent(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[39] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[39]);
  };
  Expression *E = nullptr;
  if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, }).contains(Tok.getKind())) {
    if (M2Parser::parseExpression(__FollowSet, E))
      return __errorhandler();
    if (Tok.is(tok::ellipsis)) {
      advance();
      if (M2Parser::parseOrdinalExpression(__FollowSet, E))
        return __errorhandler();
    }
  }
  else if (Tok.is(tok::l_brace)) {
    if (M2Parser::parseValueConstructorTail(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  if (Tok.is(tok::kw_BY)) {
    advance();
    if (M2Parser::parseConstantExpression(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseConstantLiteral(const __TokenBitSet &__FollowSetCallers, Expression *&Expr) {
  const __TokenBitSet __FollowSet = __FollowSets[34] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[34]);
  };
  if (Tok.is(tok::integer_literal)) {
    Expr = Actions.actOnIntegerLiteral(Tok.getLocation(), Tok.getLiteralData());
    advance();
  }
  else if (Tok.is(tok::real_literal)) {
    Expr = Actions.actOnRealLiteral(Tok.getLocation(), Tok.getLiteralData());
    advance();
  }
  else if (Tok.is(tok::string_literal)) {
    Expr = Actions.actOnStringLiteral(Tok.getLocation(), Tok.getLiteralData());
    advance();
  }
  else if (Tok.is(tok::char_literal)) {
    Expr = Actions.actOnCharLiteral(Tok.getLocation(), Tok.getLiteralData());
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseConstantExpression(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[40] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[40]);
  };
  Expression *E = nullptr;
  if (M2Parser::parseExpression(__FollowSet, E))
    return __errorhandler();
  return false;
}
bool M2Parser::parseActualParameters(const __TokenBitSet &__FollowSetCallers, ActualParameterList &Params) {
  const __TokenBitSet __FollowSet = __FollowSets[34] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[34]);
  };
  if (consume(tok::l_paren))
    return __errorhandler();
  if ((__TokenBitSet{tok::identifier, tok::integer_literal, tok::char_literal, tok::real_literal, tok::string_literal, tok::l_paren, tok::plus, tok::minus, tok::kw_NOT, }).contains(Tok.getKind())) {
    if (M2Parser::parseActualParameterList(__FollowSet, Params))
      return __errorhandler();
  }
  if (consume(tok::r_paren))
    return __errorhandler();
  return false;
}
bool M2Parser::parseActualParameterList(const __TokenBitSet &__FollowSetCallers, ActualParameterList &Params) {
  const __TokenBitSet __FollowSet = __FollowSets[8] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[8]);
  };
  if (M2Parser::parseActualParameter(__FollowSet, Params))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (M2Parser::parseActualParameter(__FollowSet, Params))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseActualParameter(const __TokenBitSet &__FollowSetCallers, ActualParameterList &Params) {
  const __TokenBitSet __FollowSet = __FollowSets[10] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[10]);
  };
  Expression *E = nullptr;
  if (M2Parser::parseExpression(__FollowSet, E))
    return __errorhandler();
  Actions.actOnActualParameter(Params, E);
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
  const __TokenBitSet __FollowSet = __FollowSets[41] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[41]);
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
  const __TokenBitSet __FollowSet = __FollowSets[42] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[42]);
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
  const __TokenBitSet __FollowSet = __FollowSets[43] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[43]);
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
  const __TokenBitSet __FollowSet = __FollowSets[44] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[44]);
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
      if (M2Parser::parseNormalMethodDefinition(__FollowSet, Decls))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_OVERRIDE)) {
      if (M2Parser::parseOverridingMethodDefinition(__FollowSet, Decls))
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
  const __TokenBitSet __FollowSet = __FollowSets[45] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[45]);
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
      if (M2Parser::parseNormalMethodDefinition(__FollowSet, Decls))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_ABSTRACT)) {
      if (M2Parser::parseAbstractMethodDefinition(__FollowSet, Decls))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_OVERRIDE)) {
      if (M2Parser::parseOverridingMethodDefinition(__FollowSet, Decls))
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
  TypeDenoter *TyDen = nullptr;
  IdentifierList IdentList;
  if (M2Parser::parseIdentifierList(__FollowSet, IdentList))
    return __errorhandler();
  if (consume(tok::colon))
    return __errorhandler();
  if (M2Parser::parseTypeDenoter(__FollowSet, TyDen))
    return __errorhandler();
  return false;
}
bool M2Parser::parseNormalMethodDefinition(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (M2Parser::parseProcedureHeading(__FollowSet, Decls))
    return __errorhandler();
  return false;
}
bool M2Parser::parseOverridingMethodDefinition(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::kw_OVERRIDE))
    return __errorhandler();
  if (M2Parser::parseProcedureHeading(__FollowSet, Decls))
    return __errorhandler();
  return false;
}
bool M2Parser::parseAbstractMethodDefinition(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::kw_ABSTRACT))
    return __errorhandler();
  if (M2Parser::parseProcedureHeading(__FollowSet, Decls))
    return __errorhandler();
  return false;
}
bool M2Parser::parseClassDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
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
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  while ((__TokenBitSet{tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_OVERRIDE, }).contains(Tok.getKind())) {
    if (M2Parser::parseNormalComponentDeclaration(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseNormalComponentDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[46] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[46]);
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
  const __TokenBitSet __FollowSet = __FollowSets[13] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[13]);
  };
  while ((__TokenBitSet{tok::kw_TYPE, tok::kw_CONST, tok::kw_VAR, tok::kw_PROCEDURE, tok::kw_ABSTRACT, tok::kw_OVERRIDE, }).contains(Tok.getKind())) {
    if (M2Parser::parseAbstractComponentDeclaration(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool M2Parser::parseAbstractComponentDeclaration(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[47] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[47]);
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
  DeclarationList Decls;
  if (Tok.is(tok::kw_PROCEDURE)) {
    if (M2Parser::parseNormalMethodDeclaration(__FollowSet, Decls))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_OVERRIDE)) {
    if (M2Parser::parseOverridingMethodDeclaration(__FollowSet, Decls))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool M2Parser::parseNormalMethodDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (M2Parser::parseProcedureDeclaration(__FollowSet, Decls))
    return __errorhandler();
  return false;
}
bool M2Parser::parseOverridingMethodDeclaration(const __TokenBitSet &__FollowSetCallers, DeclarationList &Decls) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::kw_OVERRIDE))
    return __errorhandler();
  if (M2Parser::parseProcedureDeclaration(__FollowSet, Decls))
    return __errorhandler();
  return false;
}
bool M2Parser::parseAbstractMethodDeclarations(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  DeclarationList Decls;
  if (Tok.is(tok::kw_PROCEDURE)) {
    if (M2Parser::parseNormalMethodDeclaration(__FollowSet, Decls))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_ABSTRACT)) {
    if (M2Parser::parseAbstractMethodDefinition(__FollowSet, Decls))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_OVERRIDE)) {
    if (M2Parser::parseOverridingMethodDeclaration(__FollowSet, Decls))
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
  const __TokenBitSet __FollowSet = __FollowSets[41] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[41]);
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
  const __TokenBitSet __FollowSet = __FollowSets[42] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[42]);
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
  const __TokenBitSet __FollowSet = __FollowSets[47] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[47]);
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
  const __TokenBitSet __FollowSet = __FollowSets[48] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[48]);
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
  const __TokenBitSet __FollowSet = __FollowSets[48] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[48]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool M2Parser::parseInheritClause(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[49] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[49]);
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
  const __TokenBitSet __FollowSet = __FollowSets[50] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[50]);
  };
  Type *Ty = nullptr;
  if (M2Parser::parseTypeIdentifier(__FollowSet, Ty))
    return __errorhandler();
  return false;
}
bool M2Parser::parseEntityIdentifier(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[30] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[30]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool M2Parser::parseGuardStatement(const __TokenBitSet &__FollowSetCallers, StatementList &Stmts) {
  const __TokenBitSet __FollowSet = __FollowSets[27] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[27]);
  };
  StatementList ElseStmts; /* ERROR */
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
    if (M2Parser::parseStatementSequence(__FollowSet, ElseStmts))
      return __errorhandler();
  }
  if (consume(tok::kw_END))
    return __errorhandler();
  return false;
}
bool M2Parser::parseGuardSelector(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[51] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[51]);
  };
  Expression *E = nullptr;
  if (M2Parser::parseExpression(__FollowSet, E))
    return __errorhandler();
  return false;
}
bool M2Parser::parseGuardedList(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[29] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[29]);
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
  const __TokenBitSet __FollowSet = __FollowSets[23] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[23]);
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
  const __TokenBitSet __FollowSet = __FollowSets[52] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[52]);
  };
  if (M2Parser::parseClassTypeIdentifier(__FollowSet))
    return __errorhandler();
  return false;
}
bool M2Parser::parseObjectDenoter(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[14] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[14]);
  };
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
#endif
