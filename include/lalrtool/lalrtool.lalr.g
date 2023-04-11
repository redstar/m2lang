/* Grammar for LALRtool */
%language "c++"
%define api.parser.class {Parser}
%token identifier, qualidentifier, code, argument, string
%token "%token" = kw_token, "%start" = kw_start, "%eoi" = kw_eoi
%token "%language" = kw_language, "%define" = kw_define, "%if" = kw_if
%token "%left" = kw_left, "%right" = kw_right, "%empty" = kw_empty
%start lalrtool
%%
lalrtool
  : header_list "%%" rule_list
  ;

header_list
  : header_list header
  | %empty
  ;

header
  : "%start" identifier
  | "%token" tokenlist
  | "%left" tokenlist
  | "%right" tokenlist
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
  | %empty
  ;

rule_list
  : rule_list rule
  | %empty
  ;

rule
  : nonterminal ":" rhs_list ";"
  ;

nonterminal
  : identifier opt_argument opt_code
  ;

opt_argument
  : argument
  | %empty
  ;

opt_code
  : code
  | %empty
  ;

rhs_list
  : rhs_list "|" sequence_list_or_empty
  | sequence_list_or_empty
  ;

sequence_list_or_empty
  : sequence_list
  | "%empty"
  | %empty
  ;

sequence_list
  : sequence_list sequence
  | sequence
  ;

sequence
  : sequence element
  | element
  ;

element
  : identifier opt_argument
  | string
  | code
  | "%if" code
  ;
