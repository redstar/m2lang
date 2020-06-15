/* DirectiveParser.g - Parser for Modula-2 directives.
 */
%language "c++"
%define api.parser.class {DirectiveParser}
%token identifier
%start directive
%eoi eof
%%
directive
  : "<*"
    ( "IF"                    { bool Val = false; }
                              { SMLoc Loc = Tok.getLocation(); }
      expr<Val> "THEN" "*>"   { handleIf(Loc, Val); }
    | "ELSIF"                 { bool Val = false; }
                              { SMLoc Loc = Tok.getLocation(); }
      expr<Val> "THEN" "*>"   { handleElsif(Loc, Val); }
    | "ELSE"                  { SMLoc Loc = Tok.getLocation(); }
      "*>"                    { handleElse(Loc); }
    | "END"                   { SMLoc Loc = Tok.getLocation(); }
      "*>"                    { handleEnd(Loc); }
    /*
    | "DEFINE" "(" identifier "," identifier ")" "*>"
    | "ASSIGN" "(" identifier "," identifier ")" "*>"
    | "ASSERT"
    | "UNSED" "*>"
    | "OBSOLETE" "*>"
    */
    )
  ;
expr<bool &Val>
  : term<Val>
    ( "OR"                    { bool ValRight = false; }
      term<ValRight>          { Val |= ValRight; }
    )*
  ;
term<bool &Val>
  : factor<Val>
    ( "AND"                   { bool ValRight = true; }
      factor<ValRight>        { Val &= ValRight; }
    )*
  ;
factor<bool &Val>
  : identifier                { Val = lookup(Tok.getIdentifier()); }
  | "(" expr<Val> ")"
  | "NOT" factor<Val>         { Val = !Val; }
  ;
