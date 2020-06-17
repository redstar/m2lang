//===--- DirectiveParers.g - Modula-2 Language Directive parser -----------===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the grammar for Modula-2 directives.
/// The grammar is based on the draft technical report "Interfacing Modula-2 to
/// C", Annex B: http://www.zi.biologie.uni-muenchen.de/~enger/SC22WG13/im2c-981130.html#TR-AXI-PRAGMAS
/// and is compatible to the Macintosh p1 compiler,
/// https://modula2.awiedemann.de/manual/comp4.html#L4_2
///
//===----------------------------------------------------------------------===//
%language "c++"
%define api.parser.class {DirectiveParser}
%token identifier, string_literal
%start directive
%eoi eof
%%
directive
  : "<*" singleDirective (";" singleDirective )* "*>"
  ;
singleDirective
  : ( assignment | environment | definition
    | save_restore | condition )?
  ;
assignment
  :                           { StringRef Val; }
    ( identifier              { StringRef Identifier = Tok.getIdentifier(); SMLoc Loc = Tok.getLocation(); }
      "(" value<Val> ")"      { actOnAssignment(Loc, Identifier, Val); }
    | "ASSIGN" "(" identifier { StringRef Identifier = Tok.getIdentifier(); SMLoc Loc = Tok.getLocation(); }
      "," value<Val> ")"      { actOnAssignment(Loc, Identifier, Val); }
    )
  ;
environment
  : "ENVIRON" "(" identifier  { StringRef Identifier = Tok.getIdentifier(); SMLoc Loc = Tok.getLocation(); }
    ","                       { StringRef Val; }
    value<Val> ")"            { actOnEnvironment(Loc, Identifier, Val); }
  ;
definition
  : "DEFINE" "(" identifier   { StringRef Identifier = Tok.getIdentifier(); SMLoc Loc = Tok.getLocation(); }
    ","                       { StringRef Val; }
    value<Val> ")"            { actOnDefinition(Loc, Identifier, Val); }
  ;
save_restore
  : "PUSH" | "POP" ;
condition
  : ifpart | elsifpart | elsepart | endifpart ;
ifpart
  : "IF"                      { SMLoc Loc = Tok.getLocation(); StringRef Val; }
    expr<Val> "THEN"          { actOnIf(Loc, Val); }
  ;
elsifpart
  : "ELSIF"                   { SMLoc Loc = Tok.getLocation(); StringRef Val; }
    expr<Val> "THEN"          { actOnElsIf(Loc, Val); }
  ;
elsepart
  : "ELSE"                    { actOnElse(Tok.getLocation()); }
  ;
endifpart
  : "END"                     { actOnEnd(Tok.getLocation()); }
  ;
expr<StringRef &Val>
  : condExpr<Val>
    (                         { tok::TokenKind Op = tok::unknown; }
      ( "="                   { Op = Tok.getKind(); }
      | "#"                   { Op = Tok.getKind(); }
      )                       { StringRef RightVal; }
      condExpr<RightVal>      { Val = actOnRelation(Op, Val, RightVal); }
    )?
  ;
condExpr<StringRef &Val>
  : condTerm<Val>
    ( "OR"                    { StringRef RightVal; }
      condTerm<Val>           { Val = actOnOr(Val, RightVal); }
    )*
  ;
condTerm<StringRef &Val>
  : condFactor<Val>
    ( "AND"                   { StringRef RightVal; }
      condFactor<RightVal>    { Val = actOnAnd(Val, RightVal); }
    )*
  ;
condFactor<StringRef &Val>
  : "NOT" condFactor<Val>     { actOnNot(Val); }
  | value<Val>
  | "(" expr<Val> ")"
  ;
value<StringRef &Val>
  : string_literal            { Val = Tok.getLiteralData().substr(1, Tok.getLiteralData().size()-2); }
  | identifier                { Val = actOnIdentifierValue(Tok.getIdentifier()); }
  ;
