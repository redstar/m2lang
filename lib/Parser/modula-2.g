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
compilationModule :
  "UNSAFEGUARDED"
    ( programModule<true>
    | definitionModule<true>
    | implementationModule<true>
   )
  | "GENERIC"
    ( genericDefinitionModule
    | genericImplementationModule
    )
  | programModule<false>
  | definitionModule<false>
  | implementationModule<false>
   ;
programModule<bool HasUnsafeGuarded>
  : "MODULE"                  {. SourceLocation Loc = Tok.getLocation(); .}
    identifier                {. StringRef ModuleName = Tok.getIdentifier(); .}
    (protection)? ";"
    importLists
    moduleBlock
    identifier                {. if (ModuleName != Tok.getIdentifier()) {
                                   getDiagnostics().report(Tok.getLocation(), diag::err_module_identifier_not_equal)
                                     << ModuleName << Tok.getIdentifier();
                                 } .}
    "."            {. Actions.actOnProgramModule(Loc, ModuleName); .}
  ;
moduleIdentifier :
   identifier ;
protection :
   "[" protectionExpression "]" ;
protectionExpression :
   constantExpression ;
definitionModule<bool HasUnsafeGuarded> :
  "DEFINITION" "MODULE" moduleIdentifier
  ( %if {.!HasUnsafeGuarded && getLangOpts().ISOGenerics.} /* refiningDefinitionModule*/
    "=" genericSeparateModuleIdentifier (actualModuleParameters)? ";"
  | importLists definitions /* definitionModule*/
  )
  "END" moduleIdentifier "." ;
implementationModule<bool HasUnsafeGuarded> :
  "IMPLEMENTATION" "MODULE" moduleIdentifier
  ( %if {.!HasUnsafeGuarded && getLangOpts().ISOGenerics.} /* refiningImplementationModule */
    "=" genericSeparateModuleIdentifier (actualModuleParameters)? ";" "END"
  | (protection)? ";" importLists moduleBlock /* implementationModule */
  )
  moduleIdentifier "." ;
importLists :
   ( importList )* ;
importList :
   simpleImport | unqualifiedImport ;
simpleImport :
   "IMPORT" identifierList ";" ;
unqualifiedImport :
   "FROM" moduleIdentifier "IMPORT" identifierList ";" ;
exportList :
   "EXPORT" ("QUALIFIED")? identifierList ";" ;
qualifiedIdentifier :
   (moduleIdentifier ".")* (%if {.getLangOpts().ISOObjects.} classIdentifier)? identifier ;
/* Generics start */
genericDefinitionModule :
   /*"GENERIC"*/ "DEFINITION" "MODULE" moduleIdentifier (formalModuleParameters)?
   ";" importLists definitions "END" moduleIdentifier "." ;
genericImplementationModule :
   /*"GENERIC"*/ "IMPLEMENTATION" "MODULE" moduleIdentifier (protection)?
   (formalModuleParameters)? ";" importLists moduleBlock moduleIdentifier "." ;
genericSeparateModuleIdentifier : identifier;
formalModuleParameters :
   "(" formalModuleParameterList ")" ;
formalModuleParameterList :
   formalModuleParameter (";" formalModuleParameter)*;
formalModuleParameter :
   constantValueParameterSpecification | typeParameterSpecification ;
constantValueParameterSpecification :
   identifierList ":" formalType ;
typeParameterSpecification :
   identifierList ":" "TYPE" ;
actualModuleParameters :
   "(" actualModuleParameterList ")" ;
actualModuleParameterList :
  actualModuleParameter ("," actualModuleParameter )* ;
actualModuleParameter :
  constantExpression | typeParameter ;
/* Generics end */
definitions
  :                           {. DeclList Decls; .}
  ( "CONST" (constantDeclaration<Decls> ";")*
  | "TYPE" (typeDefinition ";")*
  | "VAR" (variableDeclaration ";")*
  | procedureHeading ";"
  | %if {.getLangOpts().ISOObjects.} classDefinition ";"
   )* ;
procedureHeading :
   "PROCEDURE" procedureIdentifier (formalParameters ( ":" functionResultType )? )? ;
typeDefinition :
   typeDeclaration | opaqueTypeDefinition ;
opaqueTypeDefinition :
   identifier ;
formalParameters :
   "(" (formalParameterList)? ")" ;
formalParameterList :
   formalParameter (";" formalParameter)* ;
functionResultType :
   typeIdentifier ;
formalParameter :
   valueParameterSpecification | variableParameterSpecification ;
valueParameterSpecification :
   identifierList ":" formalType ;
variableParameterSpecification :
   "VAR" identifierList ":" formalType ;
declarations
  :                           {. DeclList Decls; .}
   (
   "CONST" (constantDeclaration<Decls> ";")* |
   "TYPE" (typeDeclaration ";")* |
   "VAR" (variableDeclaration ";")* |
   procedureDeclaration ";" |
   %if {.getLangOpts().ISOObjects.} classDeclaration ";"  |
   localModuleDeclaration ";"
   )* ;
constantDeclaration<DeclList &Decls>
  :                           {. SourceLocation Loc; StringRef Name; .}
    identifier                {. Loc = Tok.getLocation(); Name = Tok.getIdentifier(); .}
    "="                       {. Expr *E = nullptr; .}
    expression<E>             {. Decl *D = Actions.actOnConstantDecl(Loc, Name, E);
                                 Decls.push_back(D); .}
  ;
typeDeclaration
  :                           {. SourceLocation Loc; StringRef Name; .}
    identifier                {. Loc = Tok.getLocation(); Name = Tok.getIdentifier(); .}
    "=" typeDenoter ;
variableDeclaration
  : variableIdentifierList ":" typeDenoter ;
variableIdentifierList
  : identifier ( machineAddress)? ("," identifier (machineAddress)? )* ;
machineAddress
  : "[" valueOfAddressType "]" ;
valueOfAddressType
  : constantExpression ;
procedureDeclaration
  :                           {. bool IsFunction = false; .}
    "PROCEDURE" procedureIdentifier
    ( "(" (formalParameterList)? ")" (":"{.IsFunction=true;.} functionResultType )? )?
    ";"
    (properProcedureBlock<IsFunction> procedureIdentifier
    | "FORWARD"
    )
  ;
procedureIdentifier :
   identifier ;
localModuleDeclaration
  : "MODULE" moduleIdentifier
    ( %if {.getLangOpts().ISOGenerics.} /* refiningLocalModuleDeclaration*/
      "=" genericSeparateModuleIdentifier (actualModuleParameters)? ";"
      (exportList)? "END"
    | (protection)? ";" importLists (exportList)? moduleBlock
    )
    moduleIdentifier
  ;
typeDenoter :
   typeIdentifier | newType ;
ordinalTypeDenoter :
   ordinalTypeIdentifier | newOrdinalType ;
typeIdentifier :
   qualifiedIdentifier ;
ordinalTypeIdentifier :
   typeIdentifier ;
newType :
   newOrdinalType | setType | packedsetType | pointerType |
   procedureType | arrayType | recordType ;
newOrdinalType :
   enumerationType | subrangeType ;
enumerationType :
   "(" identifierList ")" ;
identifierList :
   identifier ("," identifier)* ;
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
boundType :
   typeDenoter ;
procedureType
  : "PROCEDURE" ( "(" ( formalParameterTypeList )? ")" ( ":" functionResultType )? )? ;
formalParameterTypeList :
   formalParameterType ("," formalParameterType)* ;
formalParameterType :
   variableFormalType | valueFormalType ;
variableFormalType :
   "VAR" formalType ;
valueFormalType :
   formalType ;
formalType :
   typeIdentifier | openArrayFormalType ;
openArrayFormalType :
   "ARRAY" "OF" ("ARRAY" "OF")* typeIdentifier ;
arrayType :
   "ARRAY" indexType ("," indexType)* "OF" componentType ;
indexType :
   ordinalTypeDenoter ;
componentType :
   typeDenoter ;
recordType :
   "RECORD" fieldList "END" ;
fieldList :
   fields (";" fields)* ;
fields :
   (fixedFields | variantFields)? ;
fixedFields :
   identifierList ":" fieldType ;
fieldType :
   typeDenoter ;
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
properProcedureBlock<bool IsFunction>
  : declarations
    ( "BEGIN" blockBody
    | %if {.IsFunction.} /* A function must have a body! */
    )
    "END"
  ;
moduleBlock :
   declarations (moduleBody)? "END" ;
moduleBody :
   initializationBody (finalizationBody)? ;
initializationBody :
   "BEGIN" blockBody ;
finalizationBody :
   "FINALLY" blockBody ;
blockBody
  :                           {. StmtList Stmts; /*ERROR*/ .}
   normalPart<Stmts> ("EXCEPT" exceptionalPart<Stmts>)? ;
normalPart<StmtList &Stmts>
  : statementSequence<Stmts> ;
exceptionalPart<StmtList &Stmts>
  : statementSequence<Stmts> ;
statement<Stmt *&S>
  : ( assignmentStatement<S>
    | procedureCall<S>
    | returnStatement<S>
    | retryStatement<S>
    | withStatement<S>
    | ifStatement<S>
    | caseStatement<S>
    | whileStatement<S>
    | repeatStatement<S>
    | loopStatement<S>
    | exitStatement<S>
    | forStatement<S>
    | %if {.getLangOpts().ISOObjects.} guardStatement<S>
    )?
  ;
statementSequence<StmtList &Stmts>
  :                           {. Stmt *S = nullptr; .}
   statement<S>               {. if (S) Stmts.push_back(S); .}
   ( ";"                      {. S = nullptr; .}
     statement<S>             {. if (S) Stmts.push_back(S); .}
   )*
  ;
assignmentStatement<Stmt *&S>
  :                           {. Expr *E; .}
   variableDesignator ":=" expression<E> ;
procedureCall<Stmt *&S> :
   procedureDesignator (actualParameters)? ;
procedureDesignator :
   valueDesignator ;
returnStatement<Stmt *&S>
  :                           {. Expr *E = nullptr; .}
    "RETURN" ( expression<E> )?
                              {. S = Actions.actOnReturnStmt(E); .}
  ;
retryStatement<Stmt *&S>
  : "RETRY"                   {. SourceLocation Loc = Tok.getLocation();
                                 S = Actions.actOnRetryStmt(Loc); .}
  ;
withStatement<Stmt *&S>
  :                           {. StmtList Stmts; .}
   "WITH" recordDesignator "DO" statementSequence<Stmts> "END" ;
recordDesignator :
   variableDesignator | valueDesignator ;
ifStatement<Stmt *&S> :
   guardedStatements (ifElsePart)? "END" ;
guardedStatements
  :                           {. StmtList Stmts; /* ERROR */ .}
   "IF" booleanExpression "THEN" statementSequence<Stmts>
   ("ELSIF" booleanExpression "THEN" statementSequence<Stmts>)* ;
ifElsePart
  :                           {. StmtList Stmts; /* ERROR */ .}
   "ELSE" statementSequence<Stmts> ;
booleanExpression
  :                           {. Expr *E; .}
   expression<E> ;
caseStatement<Stmt *&S> :
   "CASE" caseSelector "OF" caseList "END" ;
caseSelector :
   ordinalExpression ;
caseList :
   caseAlternative ("|" caseAlternative)*
   (caseElsePart)? ;
caseElsePart
  :                           {. StmtList Stmts; /* ERROR */ .}
   "ELSE" statementSequence<Stmts> ;
caseAlternative
  :                           {. StmtList Stmts; /* ERROR */ .}
   (caseLabelList ":" statementSequence<Stmts>)? ;
caseLabelList :
   caseLabel ("," caseLabel)* ;
caseLabel :
   constantExpression (".." constantExpression)? ;
whileStatement<Stmt *&S>
  : "WHILE"                   {. SourceLocation Loc = Tok.getLocation();
                                 Expr *Cond = nullptr; .}
    expression<Cond> "DO"     {. StmtList Stmts; .}
    statementSequence<Stmts>
    "END"                     {. S = Actions.actOnWhileStmt(Cond, Stmts, Loc); .}
  ;
repeatStatement<Stmt *&S>
  : "REPEAT"                  {. SourceLocation Loc = Tok.getLocation();
                                 StmtList Stmts; .}
    statementSequence<Stmts>
    "UNTIL"                   {. Expr *Cond = nullptr; .}
    expression<Cond>          {. S = Actions.actOnRepeatStmt(Cond, Stmts, Loc); .}
  ;
loopStatement<Stmt *&S>
  : "LOOP"                    {. SourceLocation Loc = Tok.getLocation();
                                 StmtList Stmts; .}
    statementSequence<Stmts>
    "END"                     {. S = Actions.actOnLoopStmt(Stmts, Loc); .}
  ;
exitStatement<Stmt *&S>
  : "EXIT"                    {. SourceLocation Loc = Tok.getLocation();
                                 S = Actions.actOnExitStmt(Loc); .}
  ;
forStatement<Stmt *&S>
  :                           {. StmtList Stmts; /* ERROR */ .}
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
variableDesignator :
   entireDesignator | indexedDesignator |
   selectedDesignator | dereferencedDesignator |
   %if {.getLangOpts().ISOObjects.} objectSelectedDesignator  ;
entireDesignator :
   qualifiedIdentifier ;
indexedDesignator :
   arrayVariableDesignator "[" indexExpression
   ("," indexExpression)* "]" ;
arrayVariableDesignator :
   variableDesignator ;
indexExpression :
   ordinalExpression ;
selectedDesignator :
   recordVariableDesignator "." fieldIdentifier ;
recordVariableDesignator :
   variableDesignator ;
fieldIdentifier :
   identifier ;
dereferencedDesignator :
   pointerVariableDesignator "^" ;
pointerVariableDesignator :
   variableDesignator ;
expression<Expr *&E>
  :
    simpleExpression<E>
    (                         {. OperatorInfo Op; .}
      relationalOperator<Op>
                              {. Expr *Right; .}
      simpleExpression<Right> {. E = Actions.actOnExpression(E, Right, Op); .}
    )?
  ;
/* simpleExpression is changed according to B. Kowarsch.
 * Then negation is mathematically correct.
 */
simpleExpression<Expr *&E>
  :
    ( (                       {. OperatorInfo Op; .}
        ("+"                  {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
        )?
        term<E>               {. if (Op.getKind() != tok::unknown)
                                   E = Actions.actOnFactor(E, Op); .}
      )
      (                       {. OperatorInfo Op; .}
        termOperator<Op>
                              {. Expr *Right; .}
        term<Right>           {. E = Actions.actOnSimpleExpression(E, Right, Op); .}
      )*
    )
  | "-"                       {. OperatorInfo Op(Tok.getLocation(), Tok.getKind()); .}
    factor<E>                 {. E = Actions.actOnFactor(E, Op); .}
  ;
term<Expr *&E>
  : factor<E>
    (                         {. OperatorInfo Op; .}
      factorOperator<Op>
                              {. Expr *Right; .}
      factor<Right>           {. E = Actions.actOnTerm(E, Right, Op); .}
    )*
  ;
factor<Expr *&E>
  : "(" expression<E> ")"
  | "NOT"                     {. OperatorInfo Op(Tok.getLocation(), Tok.getKind()); .}
    factor<E>                 {. E = Actions.actOnFactor(E, Op); .}
  | valueDesignator | functionCall
  | valueConstructor | constantLiteral
  ;
ordinalExpression
  :                           {. Expr *E; .}
    expression<E> ;
relationalOperator<OperatorInfo &Op>
  : "="                       {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  | "#"                       {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  | "<"                       {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  | ">"                       {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  | "<="                      {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  | ">="                      {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  | "IN"                      {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  ;
termOperator<OperatorInfo &Op>
  : "+"                       {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  | "-"                       {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  | "OR"                      {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  ;
factorOperator<OperatorInfo &Op>
  : "*"                       {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  | "/"                       {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  | "REM"                     {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  | "DIV"                     {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  | "MOD"                     {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  | "AND"                     {. Op = OperatorInfo(Tok.getLocation(), Tok.getKind()); .}
  ;
valueDesignator :
  entireValue | indexedValue | selectedValue | dereferencedValue |
  %if {.getLangOpts().ISOObjects.} objectSelectedValue ;
entireValue :
   qualifiedIdentifier ;
indexedValue :
   arrayValue "[" indexExpression
   ("," indexExpression)* "]" ;
arrayValue :
   valueDesignator ;
selectedValue :
   recordValue "." fieldIdentifier ;
recordValue :
   valueDesignator ;
dereferencedValue :
   pointerValue "^" ;
pointerValue :
   valueDesignator ;
functionCall :
   functionDesignator actualParameters ;
functionDesignator :
   valueDesignator ;
valueConstructor :
   arrayConstructor | recordConstructor | setConstructor ;
arrayConstructor :
   arrayTypeIdentifier arrayConstructedValue ;
arrayTypeIdentifier :
   typeIdentifier ;
arrayConstructedValue :
   "{" repeatedStructureComponent
   ("," repeatedStructureComponent)* "}" ;
repeatedStructureComponent :
   structureComponent ("BY" repetitionFactor)? ;
repetitionFactor :
   constantExpression ;
structureComponent
  :                           {. Expr *E; .}
   expression<E> | arrayConstructedValue |
   recordConstructedValue | setConstructedValue ;
recordConstructor :
   recordTypeIdentifier recordConstructedValue ;
recordTypeIdentifier :
   typeIdentifier ;
recordConstructedValue :
   "{" (structureComponent ("," structureComponent)* )?
   "}" ;
setConstructor :
   setTypeIdentifier setConstructedValue ;
setTypeIdentifier :
   typeIdentifier ;
setConstructedValue :
   "{" (member ("," member)* )? "}" ;
member :
   interval | singleton ;
interval :
   ordinalExpression ".." ordinalExpression ;
singleton :
   ordinalExpression ;
constantLiteral :
   integer_literal | real_literal | stringLiteral;
stringLiteral :
   string_literal | char_literal;
constantExpression
  :                           {. Expr *E; .}
    expression<E> ;
actualParameters :
   "(" (actualParameterList)? ")" ;
actualParameterList :
   actualParameter ("," actualParameter)* ;
actualParameter
  :                           {. Expr *E; .}
    (variableDesignator | expression<E> | typeParameter) ;
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
  :                           {. DeclList Decls; .}
    (
   "CONST" ( constantDeclaration<Decls> ";" )* |
   "TYPE" ( typeDefinition ";" )* |
   "VAR" ( classVariableDeclaration ";" )? |
   (normalMethodDefinition | overridingMethodDefinition) ";"
    );
abstractClassComponentDefinitions :
   ( abstractComponentDefinition )* ;
abstractComponentDefinition
  :                           {. DeclList Decls; .}
    (
   "CONST" ( constantDeclaration<Decls> ";" )* |
   "TYPE" ( typeDefinition ";" )* |
   "VAR" ( classVariableDeclaration ";" )* |
  (normalMethodDefinition | abstractMethodDefinition |
   overridingMethodDefinition) ";"
   );
classVariableDeclaration :
   identifierList ":" typeDenoter ;
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
classBody :
   moduleBody;
normalClassComponentDeclarations :
   ( normalComponentDeclaration )* ;
normalComponentDeclaration
  :                           {. DeclList Decls; .}
   "CONST" ( constantDeclaration<Decls> ";" )* |
   "TYPE" ( typeDeclaration ";" )* |
   "VAR" ( classVariableDeclaration ";" )* |
   normalMethodDeclarations ";" ;
abstractClassComponentDeclarations :
   ( abstractComponentDeclaration )* ;
abstractComponentDeclaration
  :                           {. DeclList Decls; .}
   ( "CONST" ( constantDeclaration<Decls> ";" )* |
   "TYPE" ( typeDeclaration ";" )* |
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
tracedClassBody :
   "BEGIN" blockBody;

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

objectSelectedDesignator :
   objectVariableDesignator "." (classIdentifier "." )? classVariableIdentifier ;
objectVariableDesignator :
   variableDesignator ;
objectSelectedValue :
   objectValueDesignator "." ( classIdentifier "." )? entityIdentifier ;
objectValueDesignator :
   valueDesignator ;
entityIdentifier :
   identifier ;

guardStatement<Stmt *&S>
  :                           {. StmtList Stmts; /* ERROR */ .}
   "GUARD" guardSelector "AS" guardedList ("ELSE" statementSequence<Stmts>)? "END" ;
guardSelector
  :                           {. Expr *E; .}
    expression<E> ;
guardedList :
   guardedStatementSequence ("|" guardedStatementSequence )? ;
guardedStatementSequence
  :                           {. StmtList Stmts; /* ERROR */ .}
   ((objectDenoter)? ":" guardedClassType "DO" statementSequence<Stmts>)? ;
guardedClassType :
   classTypeIdentifier ;
objectDenoter :
   identifier ;
/* End OO */