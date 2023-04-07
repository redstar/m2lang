/* Grammar for LALRtool */
%language "c++"
%define api.parser.class {Parser}
%token identifier, qualidentifier, code, argument, string
%token "%token" = kw_token, "%start" = kw_start, "%eoi" = kw_eoi
%token "%language" = kw_language, "%define" = kw_define, "%if" = kw_if
%start lalrtool
%%
lalrtool
  : header_list "%%" rule_list;

header_list
  : header_list header
  | header
  ;

header
  : "%start" identifier
  | "%token" tokenlist
  | "%language" string
  | "%define" identifier value
  | "%define" qualidentifier value
  | "%eoi" identifier
  ;

value
  : code
  | identifier
  | qualidentifier
  | string
  ;

tokenlist
  : tokenlist "," tokendecl
  | tokendecl
  ;

tokendecl
  : identifier optdefault
  | string optdefault
  ;

optdefault
  : "=" identifier
  |
  ;

rule_list
  : rule_list rule
  |
  ;

rule
  : nonterminal ":" rhs ";"
  ;

nonterminal
  : identifier opt_argument opt_code
  ;

opt_argument
  : argument
  |
  ;

opt_code
  : code
  |
  ;

rhs
  : sequence_list<R>
  ;

sequence_list
  : sequence_list sequence
  |
  ;


sequence
  : sequence element
  |
  ;

element
  : identifier opt_argument
  | string
  | code
  | "%if" code
  ;
