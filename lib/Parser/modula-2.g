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
 * - Integrate refiningimplementationModule into implementationModule.
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
programModule<bool HasUnsafeGuarded> :
   "MODULE" moduleIdentifier (protection)? ";"
  importLists moduleBlock moduleIdentifier "."
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
   unqualifiedExport | qualifiedExport ;
unqualifiedExport :
   "EXPORT" identifierList ";" ;
qualifiedExport :
   "EXPORT" "QUALIFIED" identifierList ";" ;
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
definitions :
  ( "CONST" (constantDeclaration ";")*
  | "TYPE" (typeDefinition ";")*
  | "VAR" (variableDeclaration ";")*
  | procedureHeading ";"
  | %if {.getLangOpts().ISOObjects.} classDefinition ";"
   )* ;
procedureHeading :
   properProcedureHeading | functionProcedureHeading ;
typeDefinition :
   typeDeclaration | opaqueTypeDefinition ;
opaqueTypeDefinition :
   identifier ;
properProcedureHeading :
   "PROCEDURE" procedureIdentifier (formalParameters)? ;
formalParameters :
   "(" (formalParameterList)? ")" ;
formalParameterList :
   formalParameter (";" formalParameter)* ;
functionProcedureHeading :
   "PROCEDURE" procedureIdentifier formalParameters
   ":" functionResultType ;
functionResultType :
   typeIdentifier ;
formalParameter :
   valueParameterSpecification | variableParameterSpecification ;
valueParameterSpecification :
   identifierList ":" formalType ;
variableParameterSpecification :
   "VAR" identifierList ":" formalType ;
declarations :
   (
   "CONST" (constantDeclaration ";")* |
   "TYPE" (typeDeclaration ";")* |
   "VAR" (variableDeclaration ";")* |
   procedureDeclaration ";" |
   %if {.getLangOpts().ISOObjects.} classDeclaration ";"  |
   localModuleDeclaration ";"
   )* ;
constantDeclaration
  :                           {. SourceLocation Loc; StringRef Name; .}
    identifier                {. Loc = Tok.getLocation(); Name = Tok.getIdentifier(); .}
    "="                       {. Expr *E = nullptr; .}
    expression<E>
                              {. Actions.actOnConstantDecl(Loc, Name, E); .}
  ;
typeDeclaration
  : identifier "=" typeDenoter ;
variableDeclaration
  : variableIdentifierList ":" typeDenoter ;
variableIdentifierList
  : identifier ( machineAddress)? ("," identifier (machineAddress)? )* ;
machineAddress
  : "[" valueOfAddressType "]" ;
valueOfAddressType
  : constantExpression ;
procedureDeclaration
  :                           {. bool HasParen = false; .}
    "PROCEDURE" procedureIdentifier
    ( "(" {.HasParen=true;.} (formalParameterList)? ")" )?
    ";"
    (properProcedureBlock<HasParen> procedureIdentifier
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
procedureType :                         {. bool HasParen = false; .}
   "PROCEDURE" ( "(" {. HasParen = true; .} (formalParameterTypeList)? ")")?
   (%if {. HasParen .} ":" functionResultType)?  ;
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
blockBody :
   normalPart ("EXCEPT" exceptionalPart)? ;
normalPart :
   statementSequence ;
exceptionalPart :
   statementSequence ;
statement :
   emptyStatement | assignmentStatement | procedureCall |
    returnStatement |retryStatement | withStatement |
    ifStatement | caseStatement | whileStatement |
    repeatStatement | loopStatement | exitStatement | forStatement |
    %if {.getLangOpts().ISOObjects.} guardStatement ;
statementSequence :
   statement (";" statement)* ;
emptyStatement :
   ;
assignmentStatement
  :                           {. Expr *E; .}
   variableDesignator ":=" expression<E> ;
procedureCall :
   procedureDesignator (actualParameters)? ;
procedureDesignator :
   valueDesignator ;
returnStatement
  :                           {. Expr *E; .}
    "RETURN" ( expression<E> )?
  ;
retryStatement :
   "RETRY" ;
withStatement :
   "WITH" recordDesignator "DO" statementSequence "END" ;
recordDesignator :
   variableDesignator | valueDesignator ;
ifStatement :
   guardedStatements (ifElsePart)? "END" ;
guardedStatements :
   "IF" booleanExpression "THEN" statementSequence
   ("ELSIF" booleanExpression "THEN" statementSequence)* ;
ifElsePart :
   "ELSE" statementSequence ;
booleanExpression
  :                           {. Expr *E; .}
   expression<E> ;
caseStatement :
   "CASE" caseSelector "OF" caseList "END" ;
caseSelector :
   ordinalExpression ;
caseList :
   caseAlternative ("|" caseAlternative)*
   (caseElsePart)? ;
caseElsePart :
   "ELSE" statementSequence ;
caseAlternative :
   (caseLabelList ":" statementSequence)? ;
caseLabelList :
   caseLabel ("," caseLabel)* ;
caseLabel :
   constantExpression (".." constantExpression)? ;
whileStatement :
   "WHILE" booleanExpression "DO" statementSequence "END" ;
repeatStatement :
   "REPEAT" statementSequence "UNTIL" booleanExpression ;
loopStatement :
   "LOOP" statementSequence "END" ;
exitStatement :
   "EXIT" ;
forStatement :
   "FOR" controlVariableIdentifier ":="
   initialValue "TO" finalValue ("BY" stepSize)? "DO"
   statementSequence "END" ;
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
normalComponentDefinition :
   "CONST" ( constantDeclaration ";" )* |
   "TYPE" ( typeDefinition ";" )* |
   "VAR" ( classVariableDeclaration ";" )? |
   (normalMethodDefinition | overridingMethodDefinition) ";" ;
abstractClassComponentDefinitions :
   ( abstractComponentDefinition )* ;
abstractComponentDefinition :
   "CONST" ( constantDeclaration ";" )* |
   "TYPE" ( typeDefinition ";" )* |
   "VAR" ( classVariableDeclaration ";" )* |
  (normalMethodDefinition | abstractMethodDefinition |
   overridingMethodDefinition) ";" ;
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
normalComponentDeclaration :
   "CONST" ( constantDeclaration ";" )* |
   "TYPE" ( typeDeclaration ";" )* |
   "VAR" ( classVariableDeclaration ";" )* |
   normalMethodDeclarations ";" ;
abstractClassComponentDeclarations :
   ( abstractComponentDeclaration )* ;
abstractComponentDeclaration :
   "CONST" ( constantDeclaration ";" )* |
   "TYPE" ( typeDeclaration ";" )* |
   "VAR" ( classVariableDeclaration ";" )* |
   abstractMethodDeclarations ";" ;
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

guardStatement :
   "GUARD" guardSelector "AS" guardedList ("ELSE" statementSequence)? "END" ;
guardSelector
  :                           {. Expr *E; .}
    expression<E> ;
guardedList :
   guardedStatementSequence ("|" guardedStatementSequence )? ;
guardedStatementSequence :
   ((objectDenoter)? ":" guardedClassType "DO" statementSequence)? ;
guardedClassType :
   classTypeIdentifier ;
objectDenoter :
   identifier ;
/* End OO */