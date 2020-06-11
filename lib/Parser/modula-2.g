/*
 * ISO/IEC 10514-1 Modula-2 grammar.
 * Includes ISO/IEC 10514-2 (generics) and 10514-3 (OO layer).
 * See https://www.arjay.bc.ca/Modula-2/Text/Appendices/Ap3.html
 *
 * The following expressions are used in predicates:
 * - getLangOpts().ISOGenerics is true iff language level is ISO/IEC 10514-2
 * - getLangOpts().ISOObjects is true iff language level is ISO/IEC 10514-3
 *
 * Assumption is that the lexer classifies identifiers as keywords according
 * to the supported language. E.g. "GENERIC" is only a keyword if the language
 * level is ISO/IEC 10514-2 and otherwise it's an identifier.
 *
 * The following changes were made:
 * - For symbols with alternative representations, it is expected that the lexer
 *   only returns the main representation. This is the list of tokens:
 *   AND: "&"
 *   NOT: "~"
 *   "#": "<>"
 *   "[": "(!"
 *   "]": "!)"
 *   "{": "(:"
 *   "}": ":)"
 *   "|": "!"
 *
 * Resolved LL(1) conflicts:
 * - Various changes to compilationModule:
 *   - Moved "UNSAFEGUARDED" and "GENERIC" into this rule.
 *   - Passes flag if "UNSAFEGUARDED" has bin parsed.
 * - Integrate refiningDefinitionModule into definitionModule.
 * - Integrate refiningImplementationModule into implementationModule.
 * - Integrate refiningLocalModuleDeclaration into localModuleDeclaration.
 * - Between properProcedureType and functionProcedureType.
 *   Integrated into procedureType using a predicate.
 * - Moved "TRACED" from normalTracedClassDeclaration and
 *   abstractTracedClassDeclaration into tracedClassDeclaration.
 * - Moved singleReturnStatement and functionReturnStatement into rule
 *   returnStatement.
 *
 * To enable predicates:
 * - Moved symbol definition into single parent rule definitions.
 * - Moved symbol declaration into single parent rule declarations.
 */
%language "c++"
%define api.parser.class {M2Parser}
%token identifier, integer_literal, char_literal, real_literal, string_literal
%start compilationModule
%eoi eof
%%
compilationModule<CompilationModule *&CM>
  : "UNSAFEGUARDED"
      ( programModule<CM, true>
      | definitionModule<CM, true>
      | implementationModule<CM, true>
      )
    | "GENERIC"
      ( genericDefinitionModule<CM>
      | genericImplementationModule<CM>
      )
    | programModule<CM, false>
    | definitionModule<CM, false>
    | implementationModule<CM, false>
  ;
programModule<CompilationModule *&CM, bool HasUnsafeGuarded>
  : "MODULE"
    identifier                { ProgramModule *PM = Actions.actOnProgramModule(tokenAs<Identifier>(Tok)); }
                              { EnterDeclScope S(Actions, PM); }
                              { DeclarationList Decls; Block InitBlk, FinalBlk; }
                              { Expression *ProtectionExpr = nullptr; }
    ( protection<ProtectionExpr> )? ";"
    importLists
    moduleBlock<Decls, InitBlk, FinalBlk>
    identifier                { Actions.actOnProgramModule(PM, tokenAs<Identifier>(Tok), Decls, InitBlk, FinalBlk); }
    "."                       { CM = PM; }
  ;
moduleIdentifier :
   identifier ;
protection<Expression *&Expr> :
   "[" expression<Expr> "]" ;
definitionModule<CompilationModule *&CM, bool HasUnsafeGuarded> :
  "DEFINITION" "MODULE" moduleIdentifier
  ( %if {.!HasUnsafeGuarded && getLangOpts().ISOGenerics.} /* refiningDefinitionModule*/
    "=" genericSeparateModuleIdentifier (actualModuleParameters)? ";"
  |                           { DeclarationList Decls; }
    importLists definitions<Decls> /* definitionModule*/
  )
  "END" moduleIdentifier "." ;
implementationModule<CompilationModule *&CM, bool HasUnsafeGuarded>
 :                            { DeclarationList Decls; Block InitBlk, FinalBlk; }
                              { Expression *ProtectionExpr = nullptr; }
  "IMPLEMENTATION" "MODULE" moduleIdentifier
  ( %if {.!HasUnsafeGuarded && getLangOpts().ISOGenerics.} /* refiningImplementationModule */
    "=" genericSeparateModuleIdentifier (actualModuleParameters)? ";" "END"
  | (protection<ProtectionExpr>)? ";" importLists moduleBlock<Decls, InitBlk, FinalBlk> /* implementationModule */
  )
  moduleIdentifier "." ;
importLists :
   ( importList )* ;
importList :
   simpleImport | unqualifiedImport ;
simpleImport
  :                           { IdentifierList IdentList; }
   "IMPORT" identifierList<IdentList> ";" ;
unqualifiedImport
  :                           { IdentifierList IdentList; }
   "FROM" moduleIdentifier "IMPORT" identifierList<IdentList> ";" ;
exportList
  :                           { IdentifierList IdentList; }
   "EXPORT" ("QUALIFIED")? identifierList<IdentList> ";" ;
qualifiedIdentifier<Declaration *&Decl>
  : ( %if{Actions.isModule(Tok.getIdentifier())}
      identifier              { Decl = Actions.actOnModuleIdentifier(Decl, tokenAs<Identifier>(Tok)); }
      "." )*
    ( %if{getLangOpts().ISOObjects && Actions.isClass(Tok.getIdentifier())}
      identifier              { Decl = Actions.actOnClassIdentifier(Decl, tokenAs<Identifier>(Tok)); }
      "." )?
    identifier                { Decl = Actions.actOnQualifiedIdentifier(Decl, tokenAs<Identifier>(Tok)); }
  ;
/* Generics start */
genericDefinitionModule<CompilationModule *&CM>
  :                           {DeclarationList Decls;}
   /*"GENERIC"*/ "DEFINITION" "MODULE" moduleIdentifier (formalModuleParameters)?
   ";" importLists definitions<Decls> "END" moduleIdentifier "." ;
genericImplementationModule<CompilationModule *&CM>
  :                           { DeclarationList Decls; Block InitBlk, FinalBlk; }
                              { Expression *ProtectionExpr = nullptr; }
   /*"GENERIC"*/ "IMPLEMENTATION" "MODULE" moduleIdentifier (protection<ProtectionExpr>)?
   (formalModuleParameters)? ";" importLists moduleBlock<Decls, InitBlk, FinalBlk>
    moduleIdentifier "." ;
genericSeparateModuleIdentifier : identifier;
formalModuleParameters :
   "(" formalModuleParameterList ")" ;
formalModuleParameterList :
   formalModuleParameter (";" formalModuleParameter)*;
formalModuleParameter :
   constantValueParameterSpecification | typeParameterSpecification ;
constantValueParameterSpecification
  :                           { IdentifierList IdentList; }
   identifierList<IdentList> ":" formalType ;
typeParameterSpecification
  :                           { IdentifierList IdentList; }
   identifierList<IdentList> ":" "TYPE" ;
actualModuleParameters :
   "(" actualModuleParameterList ")" ;
actualModuleParameterList :
  actualModuleParameter ("," actualModuleParameter )* ;
actualModuleParameter :
  constantExpression | typeParameter ;
/* Generics end */
definitions<DeclarationList &Decls>
  : ( "CONST" (constantDeclaration<Decls> ";")*
    | "TYPE" (typeDefinition<Decls> ";")*
    | "VAR" (variableDeclaration<Decls> ";")*
    | procedureHeading ";"
    | %if {.getLangOpts().ISOObjects.} classDefinition ";"
    )*
  ;
procedureHeading
  :                           { FormalParameterList Params; }
   "PROCEDURE" procedureIdentifier (formalParameters<Params> ( ":" functionResultType )? )? ;
typeDefinition<DeclarationList &Decls>
  : typeDeclaration<Decls> | opaqueTypeDefinition ;
opaqueTypeDefinition :
   identifier ;
formalParameters<FormalParameterList &Params>
  : "(" ( formalParameterList<Params> )? ")" ;
formalParameterList<FormalParameterList &Params>
  : formalParameter<Params> (";" formalParameter<Params> )* ;
functionResultType :
   typeIdentifier ;
formalParameter<FormalParameterList &Params>
  :                           { bool IsVar = false; }
                              { IdentifierList IdentList; }
    ( "VAR" )? identifierList<IdentList> ":" formalType
                              { Actions.actOnFormalParameter(Params, IdentList, IsVar, nullptr); }
  ;
declarations<DeclarationList &Decls>
  : ( "CONST" (constantDeclaration<Decls> ";")*
    | "TYPE" (typeDeclaration<Decls> ";")*
    | "VAR" (variableDeclaration<Decls> ";")*
    | procedureDeclaration ";"
    | %if {.getLangOpts().ISOObjects.} classDeclaration ";"
    | localModuleDeclaration ";"
    )*
  ;
constantDeclaration<DeclarationList &Decls>
  : identifier                { Identifier ConstName = tokenAs<Identifier>(Tok); }
    "="                       { Expression *E = nullptr; }
    expression<E>             { Actions.actOnConstant(Decls, ConstName, E); }
  ;
typeDeclaration<DeclarationList &Decls>
  : identifier                { Identifier TypeName = tokenAs<Identifier>(Tok); }
    "="                       { TypeDenoter *TyDen = nullptr; }
    typeDenoter<TyDen>        { Actions.actOnType(Decls, TypeName, TyDen); }
  ;
variableDeclaration<DeclarationList &Decls>
  :                           { VariableIdentifierList VarIdList; }
                              { TypeDenoter *TyDen = nullptr; }
    variableIdentifierList<VarIdList> ":" typeDenoter<TyDen>
                              { Actions.actOnVariable(Decls, VarIdList, TyDen); }
  ;
variableIdentifierList<VariableIdentifierList &VarIdList>
  : identifier                { Identifier Id = tokenAs<Identifier>(Tok); }
                              { Expression *Addr = nullptr; }
    ( machineAddress<Addr> )? { VarIdList.push_back(std::pair<Identifier, Expression *>(Id, Addr)); }
    ( ","
      identifier              { Identifier Id = tokenAs<Identifier>(Tok); }
                              { Expression *Addr = nullptr; }
      ( machineAddress<Addr> )?
                              { VarIdList.push_back(std::pair<Identifier, Expression *>(Id, Addr)); }
    )*
  ;
machineAddress<Expression *&Addr>
  : "[" expression<Addr> "]" ;
procedureDeclaration
  : "PROCEDURE" identifier    { Procedure *P = Actions.actOnProcedure(tokenAs<Identifier>(Tok)); }
                              { EnterDeclScope S(Actions, P); }
                              { bool IsFunction = false; }
                              { FormalParameterList Params; }
                              { Declaration *ResultType = nullptr; }
    ( "(" (formalParameterList<Params>)? ")"
      ( ":"                   { IsFunction=true; }
        qualifiedIdentifier<ResultType> )?
    )?
    ";"
    (                         { DeclarationList Decls; Block Body; }
       properProcedureBlock<Decls, Body, IsFunction> identifier
                              { Actions.actOnProcedure(P, tokenAs<Identifier>(Tok), Params, ResultType, Decls, Body, IsFunction); }
    | "FORWARD"               { Actions.actOnForwardProcedure(P); }
    )
  ;
procedureIdentifier :
   identifier ;
localModuleDeclaration
  : "MODULE" identifier       { LocalModule *LM = Actions.actOnLocalModule(tokenAs<Identifier>(Tok)); }
                              { EnterDeclScope S(Actions, LM); }
                              { DeclarationList Decls; Block InitBlk, FinalBlk; }
                              { Expression *ProtectionExpr = nullptr; }
    ( %if {.getLangOpts().ISOGenerics.} /* refiningLocalModuleDeclaration*/
      "=" genericSeparateModuleIdentifier (actualModuleParameters)? ";"
      (exportList)? "END"
    | ( protection<ProtectionExpr> )? ";" importLists (exportList)? moduleBlock<Decls, InitBlk, FinalBlk>
    )
    moduleIdentifier
  ;
typeDenoter<TypeDenoter *&TyDen>
  :                           { Declaration *Decl = nullptr; }
    qualifiedIdentifier<Decl> { TyDen = Actions.actOnNamedType(SMLoc(), Decl); }
  | newType
  ;
ordinalTypeDenoter :
   ordinalTypeIdentifier | newOrdinalType ;
typeIdentifier
  :                           { Declaration *Decl = nullptr; }
   qualifiedIdentifier<Decl> ;
ordinalTypeIdentifier :
   typeIdentifier ;
newType :
   newOrdinalType | setType | packedsetType | pointerType |
   procedureType | arrayType | recordType ;
newOrdinalType :
   enumerationType | subrangeType ;
enumerationType
  :                           { IdentifierList IdentList; }
   "(" identifierList<IdentList> ")" ;
identifierList<IdentifierList &IdentList>
  : identifier                { IdentList.push_back(tokenAs<Identifier>(Tok)); }
    ( "," identifier          { IdentList.push_back(tokenAs<Identifier>(Tok)); }
    )*
  ;
subrangeType :
   (rangeType)? "[" constantExpression ".."
   constantExpression "]" ;
rangeType :
   ordinalTypeIdentifier ;
setType :
   "SET" "OF" baseType ;
baseType :
   ordinalTypeDenoter ;
packedsetType :
   "PACKEDSET" "OF" baseType ;
pointerType :
   "POINTER" "TO" boundType ;
boundType
  :                           { TypeDenoter *TyDen = nullptr; }
   typeDenoter<TyDen> ;
procedureType
  : "PROCEDURE" ( "(" ( formalParameterTypeList )? ")" ( ":" functionResultType )? )? ;
formalParameterTypeList :
   formalParameterType ("," formalParameterType)* ;
formalParameterType
  : ( "VAR" )? formalType ;
formalType
  :                           { TypeDenoter *TyDen = nullptr; }
   typeIdentifier | openArrayFormalType ;
openArrayFormalType :
   "ARRAY" "OF" ("ARRAY" "OF")* typeIdentifier ;
arrayType :
   "ARRAY" indexType ("," indexType)* "OF" componentType ;
indexType :
   ordinalTypeDenoter ;
componentType
  :                           { TypeDenoter *TyDen = nullptr; }
   typeDenoter<TyDen> ;
recordType :
   "RECORD" fieldList "END" ;
fieldList :
   fields (";" fields)* ;
fields :
   (fixedFields | variantFields)? ;
fixedFields
  :                           { IdentifierList IdentList; }
   identifierList<IdentList> ":" fieldType ;
fieldType
  :                           { TypeDenoter *TyDen = nullptr; }
   typeDenoter<TyDen> ;
variantFields :
   "CASE" (tagIdentifier)? ":" tagType "OF"
   variantList "END" ;
tagIdentifier :
   identifier ;
tagType :
   ordinalTypeIdentifier ;
variantList :
   variant ("|" variant)* (variantElsePart)? ;
variantElsePart :
   "ELSE" fieldList ;
variant :
   (variantLabelList ":" fieldList)? ;
variantLabelList :
   variantLabel ("," variantLabel)* ;
variantLabel :
   constantExpression (".." constantExpression)? ;
properProcedureBlock<DeclarationList &Decls, Block &Body, bool IsFunction>
  : declarations<Decls>
    ( "BEGIN" blockBody<Body>
    | %if {.IsFunction.} /* A function must have a body! */
    )
    "END"
  ;
moduleBlock<DeclarationList &Decls, Block &InitBlk, Block &FinalBlk>
  : declarations<Decls> ( moduleBody<InitBlk, FinalBlk> )? "END" ;
moduleBody<Block &InitBlk, Block &FinalBlk> :
   initializationBody<InitBlk> ( finalizationBody<FinalBlk> )? ;
initializationBody<Block &InitBlk>
  : "BEGIN" blockBody<InitBlk> ;
finalizationBody<Block &FinalBlk>
  : "FINALLY" blockBody<FinalBlk> ;
blockBody<Block &Blk>
  :                           { StatementList Stmts, ExceptStmts; }
   normalPart<Stmts>
   ( "EXCEPT" exceptionalPart<Stmts> )?
                              { Blk = Block(Stmts, ExceptStmts); }
   ;
normalPart<StatementList &Stmts>
  : statementSequence<Stmts> ;
exceptionalPart<StatementList &Stmts>
  : statementSequence<Stmts> ;
statement<StatementList &Stmts, Statement *&S>
  : ( assignmentOrProcedireCall<Stmts>
    | returnStatement<Stmts>
    | retryStatement<S>
    | withStatement<S>
    | ifStatement<S>
    | caseStatement<S>
    | whileStatement<Stmts>
    | repeatStatement<Stmts>
    | loopStatement<Stmts>
    | exitStatement<Stmts>
    | forStatement<S>
    | %if {.getLangOpts().ISOObjects.} guardStatement<S>
    )?
  ;
statementSequence<StatementList &Stmts>
  :                           {. Statement *S = nullptr; .}
   statement<Stmts, S>        {. if (S) Stmts.push_back(S); .}
   ( ";"                      {. S = nullptr; .}
     statement<Stmts, S>      {. if (S) Stmts.push_back(S); .}
   )*
  ;
assignmentOrProcedireCall<StatementList &Stmts>
  :                           { Designator *Desig = nullptr; }
    designator<Desig>
    ( ":="                    { Expression *E = nullptr; }  /* assignment */
      expression<E>           { Actions.actOnAssignmentStmt(Stmts, Desig, E); }
    | (                       { ExpressionList ActualParameters; }
        actualParameters                                    /* procedureCall */
                              { Actions.actOnProcedureCallStmt(Stmts, Desig, ActualParameters); }
      )?
    )
  ;
returnStatement<StatementList &Stmts>
  : "RETURN"                  { Expression *E = nullptr; }
    ( expression<E> )?        { Actions.actOnReturnStmt(Stmts, E); }
  ;
retryStatement<Statement *&S>
  : "RETRY"                   {. SMLoc Loc = Tok.getLocation();
                                 S = Actions.actOnRetryStmt(Loc); .}
  ;
withStatement<Statement *&S>
  :                           {. StatementList Stmts; .}
   "WITH" recordDesignator "DO" statementSequence<Stmts> "END" ;
recordDesignator
  :                           { Designator *Desig = nullptr; }
    designator<Desig>         /* Before refactor: variableDesignator | valueDesignator */
  ;
ifStatement<Statement *&S> :
   guardedStatements (ifElsePart)? "END" ;
guardedStatements
  :                           {. StatementList Stmts; /* ERROR */ .}
   "IF" booleanExpression "THEN" statementSequence<Stmts>
   ("ELSIF" booleanExpression "THEN" statementSequence<Stmts>)* ;
ifElsePart
  :                           {. StatementList Stmts; /* ERROR */ .}
   "ELSE" statementSequence<Stmts> ;
booleanExpression
  :                           {. Expression *E = nullptr; .}
   expression<E> ;
caseStatement<Statement *&S> :
   "CASE" caseSelector "OF" caseList "END" ;
caseSelector :
   ordinalExpression ;
caseList :
   caseAlternative ("|" caseAlternative)*
   (caseElsePart)? ;
caseElsePart
  :                           {. StatementList Stmts; /* ERROR */ .}
   "ELSE" statementSequence<Stmts> ;
caseAlternative
  :                           {. StatementList Stmts; /* ERROR */ .}
   (caseLabelList ":" statementSequence<Stmts>)? ;
caseLabelList :
   caseLabel ("," caseLabel)* ;
caseLabel :
   constantExpression (".." constantExpression)? ;
whileStatement<StatementList &Stmts>
  : "WHILE"                   { SMLoc Loc = Tok.getLocation(); }
                              { Expression *Cond = nullptr; }
    expression<Cond> "DO"     { StatementList WhileStmts; }
    statementSequence<WhileStmts>
    "END"                     { Actions.actOnWhileStmt(Stmts, Loc, Cond, WhileStmts); }
  ;
repeatStatement<StatementList &Stmts>
  : "REPEAT"                  { SMLoc Loc = Tok.getLocation(); }
                              { StatementList RepeatStmts; }
    statementSequence<RepeatStmts>
    "UNTIL"                   { Expression *Cond = nullptr; }
    expression<Cond>          { Actions.actOnRepeatStmt(Stmts, Loc, Cond, RepeatStmts); }
  ;
loopStatement<StatementList &Stmts>
  : "LOOP"                    { SMLoc Loc = Tok.getLocation(); }
                              { StatementList LoopStmts; }
    statementSequence<LoopStmts>
    "END"                     { Actions.actOnLoopStmt(Stmts, Loc, LoopStmts); }
  ;
exitStatement<StatementList &Stmts>
  : "EXIT"                    { Actions.actOnExitStmt(Stmts, Tok.getLocation()); }
  ;
forStatement<Statement *&S>
  :                           {. StatementList Stmts; /* ERROR */ .}
   "FOR" controlVariableIdentifier ":="
   initialValue "TO" finalValue ("BY" stepSize)? "DO"
   statementSequence<Stmts> "END" ;
controlVariableIdentifier :
   identifier ;
initialValue :
   ordinalExpression ;
finalValue :
   ordinalExpression ;
stepSize :
   constantExpression ;
variableDesignator
  :                           { Designator *Desig = nullptr; }
    designator<Desig> ;
indexExpression :
   ordinalExpression ;
fieldIdentifier :
   identifier ;
expression<Expression *&E>
  :
    simpleExpression<E>
    (                         {. OperatorInfo Op; .}
      relationalOperator<Op>
                              {. Expression *Right = nullptr; .}
      simpleExpression<Right> {. E = Actions.actOnExpression(E, Right, Op); .}
    )?
  ;
simpleExpression<Expression *&E>
  :                           { OperatorInfo PrefixOp; }
    ( "+"                     { PrefixOp = tokenAs<OperatorInfo>(Tok); }
    | "-"                     { PrefixOp = tokenAs<OperatorInfo>(Tok); }
    )?
    term<E>
    (                         { OperatorInfo Op; }
      termOperator<Op>
                              { Expression *Right = nullptr; }
      term<Right>             { E = Actions.actOnSimpleExpression(E, Right, Op); }
    )*
                              { if (!PrefixOp.isUnspecified()) E = Actions.actOnPrefixOperator(E, PrefixOp); }
  ;
term<Expression *&E>
  : factor<E>
    (                         {. OperatorInfo Op; .}
      factorOperator<Op>
                              {. Expression *Right = nullptr; .}
      factor<Right>           {. E = Actions.actOnTerm(E, Right, Op); .}
    )*
  ;
factor<Expression *&E>
  : "(" expression<E> ")"
  | "NOT"                     { OperatorInfo Op(tokenAs<OperatorInfo>(Tok)); }
    factor<E>                 { E = Actions.actOnFactor(E, Op); }
  |                           { Declaration *Decl = nullptr; }
                              { SelectorList Selectors; }
    /* Refactored: valueDesignator | functionCall | valueConstructor */
    qualifiedIdentifier<Decl>
    ( designatorTail<Selectors>                             /* valueDesignator */
                              { E = Actions.actOnDesignator(Decl, Selectors); }
      (                                                     /* functionCall */
                              { ExpressionList ActualParameters; }
        actualParameters      { E = Actions.actOnFunctionCall(E, ActualParameters); }
      )?
    | valueConstructorTail
    )?
  | constantLiteral<E>
  ;
ordinalExpression
  :                           {. Expression *E = nullptr; .}
    expression<E> ;
relationalOperator<OperatorInfo &Op>
  : "="                       { Op = tokenAs<OperatorInfo>(Tok); }
  | "#"                       { Op = tokenAs<OperatorInfo>(Tok); }
  | "<"                       { Op = tokenAs<OperatorInfo>(Tok); }
  | ">"                       { Op = tokenAs<OperatorInfo>(Tok); }
  | "<="                      { Op = tokenAs<OperatorInfo>(Tok); }
  | ">="                      { Op = tokenAs<OperatorInfo>(Tok); }
  | "IN"                      { Op = tokenAs<OperatorInfo>(Tok); }
  ;
termOperator<OperatorInfo &Op>
  : "+"                       { Op = tokenAs<OperatorInfo>(Tok); }
  | "-"                       { Op = tokenAs<OperatorInfo>(Tok); }
  | "OR"                      { Op = tokenAs<OperatorInfo>(Tok); }
  ;
factorOperator<OperatorInfo &Op>
  : "*"                       { Op = tokenAs<OperatorInfo>(Tok); }
  | "/"                       { Op = tokenAs<OperatorInfo>(Tok); }
  | "REM"                     { Op = tokenAs<OperatorInfo>(Tok); }
  | "DIV"                     { Op = tokenAs<OperatorInfo>(Tok); }
  | "MOD"                     { Op = tokenAs<OperatorInfo>(Tok); }
  | "AND"                     { Op = tokenAs<OperatorInfo>(Tok); }
  ;
/* Either a valueDesignator or a variableDesignator */
designator<Designator *&Desig>
  :                           { Declaration *Decl = nullptr; }
                              { SelectorList Selectors; }
    qualifiedIdentifier<Decl>                               /* entireValue */
    designatorTail<Selectors> { Desig = Actions.actOnDesignator(Decl, Selectors); }
  ;
designatorTail<SelectorList &Selectors>
  : ( "[" indexExpression ("," indexExpression)* "]"        /* indexedValue / indexedDesignator */
    | "." ( fieldIdentifier                                 /* selectedValue / selectedDesignator */
          | %if {getLangOpts().ISOObjects}                  /* objectSelectedValue / objectSelectedDesignator */
            "." ( classIdentifier "." )? entityIdentifier
          )
    | "^"                                                   /* dereferencedValue / dereferencedDesignator */
    )*
  ;
valueConstructorTail
  : "{"
    ( repeatedStructureComponent                          /* arrayConstructedValue */
      ("," repeatedStructureComponent)* "}"
    | (structureComponent ("," structureComponent)* )?    /* recordConstructedValue */
    | (member ("," member)* )? "}"                        /* setConstructedValue */
    )
    "}"
  ;
arrayConstructedValue
  : "{" repeatedStructureComponent ("," repeatedStructureComponent)* "}" ;
repeatedStructureComponent
  : structureComponent ("BY" repetitionFactor)? ;
repetitionFactor
  : constantExpression ;
structureComponent
  :                           {. Expression *E = nullptr; .}
   expression<E> | arrayConstructedValue |
   recordConstructedValue | setConstructedValue ;
recordConstructedValue :
   "{" (structureComponent ("," structureComponent)* )?
   "}" ;
setConstructedValue :
   "{" (member ("," member)* )? "}" ;
member :
   interval | singleton ;
interval :
   ordinalExpression ".." ordinalExpression ;
singleton :
   ordinalExpression ;
constantLiteral<Expression *&Expr>
  : integer_literal           { Expr = Actions.actOnIntegerLiteral(Tok.getLocation(), Tok.getLiteralData()); }
  | real_literal              { Expr = Actions.actOnRealLiteral(Tok.getLocation(), Tok.getLiteralData()); }
  | string_literal            { Expr = Actions.actOnStringLiteral(Tok.getLocation(), Tok.getLiteralData()); }
  | char_literal              { Expr = Actions.actOnCharLiteral(Tok.getLocation(), Tok.getLiteralData()); }
  ;
constantExpression
  :                           {. Expression *E = nullptr; .}
    expression<E> ;
actualParameters :
   "(" (actualParameterList)? ")" ;
actualParameterList :
   actualParameter ("," actualParameter)* ;
actualParameter
  :                           {. Expression *E = nullptr; .}
                              {. Designator *Desig = nullptr; .}
    (variableDesignator<Desig> | expression<E> | typeParameter) ;
typeParameter :
   typeIdentifier ;

/* Begin OO */
classDefinition :
   ( tracedClassDefinition | untracedClassDefinition );
untracedClassDefinition :
   ( normalClassDefinition | abstractClassDefinition ) ;
tracedClassDefinition :
   "TRACED" ( normalClassDefinition | abstractClassDefinition ) ;
normalClassDefinition :
   normalClassHeader ( normalClassDefinitionBody | "FORWARD" ) ;
normalClassHeader :
   "CLASS" classIdentifier ";" ;
normalClassDefinitionBody :
   ( inheritClause )? ( revealList )? normalClassComponentDefinitions
   "END" classIdentifier ;
abstractClassDefinition :
   abstractClassHeader ( abstractClassDefinitionBody | "FORWARD" ) ;
abstractClassHeader :
   "ABSTRACT" "CLASS" classIdentifier ";" ;
abstractClassDefinitionBody :
   ( inheritClause )? ( revealList )? abstractClassComponentDefinitions
   "END" classIdentifier ;
classIdentifier :
   identifier ;
normalClassComponentDefinitions :
  ( normalComponentDefinition )* ;
normalComponentDefinition
  :                           {. DeclarationList Decls; .}
    (
   "CONST" ( constantDeclaration<Decls> ";" )* |
   "TYPE" ( typeDefinition<Decls> ";" )* |
   "VAR" ( classVariableDeclaration ";" )? |
   (normalMethodDefinition | overridingMethodDefinition) ";"
    );
abstractClassComponentDefinitions :
   ( abstractComponentDefinition )* ;
abstractComponentDefinition
  :                           {. DeclarationList Decls; .}
    (
   "CONST" ( constantDeclaration<Decls> ";" )* |
   "TYPE" ( typeDefinition<Decls> ";" )* |
   "VAR" ( classVariableDeclaration ";" )* |
  (normalMethodDefinition | abstractMethodDefinition |
   overridingMethodDefinition) ";"
   );
classVariableDeclaration
  :                           { TypeDenoter *TyDen = nullptr; }
                              { IdentifierList IdentList; }
   identifierList<IdentList> ":" typeDenoter<TyDen> ;
normalMethodDefinition :
   procedureHeading;
overridingMethodDefinition :
   "OVERRIDE" procedureHeading;
abstractMethodDefinition :
   "ABSTRACT" procedureHeading;
classDeclaration :
   ( tracedClassDeclaration | untracedClassDeclaration ) ;
untracedClassDeclaration :
   ( normalClassDeclaration | abstractClassDeclaration ) ;
normalClassDeclaration :
   normalClassHeader ( normalClassDeclarationBody | "FORWARD" ) ;
normalClassDeclarationBody :
   ( inheritClause )? ( revealList )? normalClassComponentDeclarations
   ( classBody )? "END" classIdentifier ;
abstractClassDeclaration :
   abstractClassHeader ( abstractClassDeclarationBody | "FORWARD" ) ;
abstractClassDeclarationBody :
   ( inheritClause )? ( revealList )? abstractClassComponentDeclarations
   ( classBody )? "END" classIdentifier ;
classBody
  :                                     { Block InitBlk, FinalBlk; }
   moduleBody<InitBlk, FinalBlk>;
normalClassComponentDeclarations :
   ( normalComponentDeclaration )* ;
normalComponentDeclaration
  :                           {. DeclarationList Decls; .}
    ( "CONST" ( constantDeclaration<Decls> ";" )*
    | "TYPE" ( typeDeclaration<Decls> ";" )*
    | "VAR" ( classVariableDeclaration ";" )*
    | normalMethodDeclarations ";"
    )
  ;
abstractClassComponentDeclarations :
   ( abstractComponentDeclaration )* ;
abstractComponentDeclaration
  :                           {. DeclarationList Decls; .}
   ( "CONST" ( constantDeclaration<Decls> ";" )* |
   "TYPE" ( typeDeclaration<Decls> ";" )* |
   "VAR" ( classVariableDeclaration ";" )* |
   abstractMethodDeclarations ";"
   );
normalMethodDeclarations :
   normalMethodDeclaration | overridingMethodDeclaration;
normalMethodDeclaration :
   procedureDeclaration;
overridingMethodDeclaration :
   "OVERRIDE" procedureDeclaration;
abstractMethodDeclarations :
   normalMethodDeclaration | abstractMethodDefinition |
   overridingMethodDeclaration;
tracedClassDeclaration :
   "TRACED" ( normalTracedClassDeclaration | abstractTracedClassDeclaration ) ;
normalTracedClassDeclaration :
   normalTracedClassHeader ( normalTracedClassDeclarationBody | "FORWARD" ) ;
normalTracedClassHeader :
   "CLASS" classIdentifier ";" ;
normalTracedClassDeclarationBody :
   ( inheritClause )? ( revealList )? normalClassComponentDeclarations
   ( tracedClassBody )? "END" classIdentifier ;
abstractTracedClassDeclaration :
   abstractTracedClassHeader ( abstractTracedClassDeclarationBody | "FORWARD" ) ;
abstractTracedClassHeader :
   "ABSTRACT" "CLASS" classIdentifier ";" ;
abstractTracedClassDeclarationBody :
   ( inheritClause )? ( revealList )? abstractClassComponentDeclarations
   ( tracedClassBody )? "END" classIdentifier ;
tracedClassBody
  :                           { Block Body; }
   "BEGIN" blockBody<Body>;

revealList :
   "REVEAL" revealedComponentList ";" ;
revealedComponentList :
   revealedComponent ("," revealedComponent )* ;
revealedComponent :
   identifier | "READONLY" classVariableIdentifier ;
classVariableIdentifier :
   identifier ;

inheritClause :
   "INHERIT" classTypeIdentifier ";" ;
classTypeIdentifier :
   typeIdentifier ;

entityIdentifier :
   identifier ;

guardStatement<Statement *&S>
  :                           {. StatementList Stmts; /* ERROR */ .}
   "GUARD" guardSelector "AS" guardedList ("ELSE" statementSequence<Stmts>)? "END" ;
guardSelector
  :                           {. Expression *E = nullptr; .}
    expression<E> ;
guardedList :
   guardedStatementSequence ("|" guardedStatementSequence )? ;
guardedStatementSequence
  :                           {. StatementList Stmts; /* ERROR */ .}
   ((objectDenoter)? ":" guardedClassType "DO" statementSequence<Stmts>)? ;
guardedClassType :
   classTypeIdentifier ;
objectDenoter :
   identifier ;
/* End OO */