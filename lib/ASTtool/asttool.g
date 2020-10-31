/* Grammar for ASTtool */
%language "c++"
%define api.parser.class {Parser}
%token identifier, code, string
%token "%typedef" = kw_typedef, "%node" = kw_node, "%base" = kw_base
%token "%language" = kw_language, "%plain" = kw_plain, "%list" = kw_list
%token "%enum" = kw_enum, "%in" = kw_in, "%out" = kw_out
%start asttool
%%
asttool
  : ( header )? ( typedecl )+ ;

header
  : ("%typedef" typedeflist
    | "%language" string               { /*Builder.language(Tok.getLoc(), Tok.getData());*/ }
    )*
    "%%"
  ;

typedeflist
  : typedef ("," typedef )*
  ;

typedef
  : identifier code
  ;

typedecl
  : ("%node" | "%base" | "%plain") identifier ( super )? "=" body ";"
  ;

super
  : "<:" identifier
  ;

body
  : ( decl ( "," decl )* )?
  ;

decl
  :  ( property )? identifier ":" ( "%list" )? identifier
  | "%enum" identifier code
  ;

property
  : "%in" | "%out"
  ;