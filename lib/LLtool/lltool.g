/* Grammar for LLtool */
%language "c++"
%define api.parser.class {Parser}
%token identifier, qualidentifier, code, argument, string
%token "%token" = kw_token, "%start" = kw_start, "%eoi" = kw_eoi
%token "%language" = kw_language, "%define" = kw_define, "%if" = kw_if
%start lltool
%%
lltool
  : ( header )? ( rule )+ ;

header
  : ("%start" identifier                { Builder.startSymbol(Tok.getLoc(), Tok.getData()); }
     | "%token" tokenlist
     | "%language" string               { Builder.language(Tok.getLoc(), Tok.getData()); }
     | "%define"                        { SMLoc Loc; StringRef ident, value; var::VarType type = var::Flag; }
       (                                { Loc = Tok.getLoc(); ident = Tok.getData(); }
         ( identifier | qualidentifier )
       )
       (                                { value = Tok.getData(); }
         ( code                         { type = var::Code; }
         | identifier                   { type = var::Identifier; }
         | qualidentifier               { type = var::Identifier; }
         | string                       { type = var::String; }
         )
       )?
                                        { Builder.define(Loc, ident, value, type); }
     | "%eoi" identifier                { Builder.eoiSymbol(Tok.getLoc(), Tok.getData()); }
    )*
    "%%"
  ;

tokenlist
  : tokendecl ("," tokendecl )*
  ;

tokendecl
  :                                     { SMLoc Loc; StringRef val, ext; }
    (identifier                         { Loc = Tok.getLoc(); val = Tok.getData(); }
     | string                           { Loc = Tok.getLoc(); val = Tok.getData(); }
    )
    ( "=" identifier                    { ext = Tok.getData(); }
    )?
                                        { Builder.terminal(Loc, val, ext); }
  ;

rule
  :                                     { Node *node = nullptr; }
    nonterminal<node> ":"
    rhs<.node->Link.>                   { node->Link->Back = node; }
    ";"
  ;

nonterminal<Node *&node>
  : identifier                          { node = Builder.nonterminal(Tok.getLoc(), Tok.getData()); }
    ( argument                          { Builder.argument(node, Tok.getData()); }
    )?
    ( code
    )?
  ;

rhs<Node *&node>
  : sequence<node>
    (                                   { node = Builder.alternative(node->Loc, node);
                                           Node *alt = node->Link; alt->Back = node; }
      ( "|" sequence<.alt->Link.>       { alt = alt->Link; alt->Back = node; }
      )+
    )?
  ;

sequence<Node *&node>
  :                                     { Node *last = nullptr; node = Builder.sequence(Tok.getLoc()); }
    (                                   { Node *n = nullptr; }
      ( group<n>
      | identifier                      { n = Builder.symbol(Tok.getLoc(), Tok.getData()); }
        ( argument                      { Builder.argument(n, Tok.getData()); }
        )?
      | string                          { n = Builder.symbol(Tok.getLoc(), Tok.getData(), true); }
      | code                            { n = Builder.code(Tok.getLoc(), Tok.getData()); }
      | "%if" code                      { n = Builder.code(Tok.getLoc(), Tok.getData());
                                          cast<Code>(n)->Type = Code::Condition; }
      )
                                        { if (!last) node->Inner = last = n;
                                           else last->Next = n, last = n; }
    )*
                                        { if (last) last->Back = node; }
  ;

group<Node *&node>
  : "("                                 { node = Builder.group(Tok.getLoc(), Group::One); }
    rhs<.node->Link.>                   { node->Link->Back = node; }
    ( ")"
      | ")?"                            { cast<Group>(node)->Cardinality = Group::ZeroOrOne; }
      | ")*"                            { cast<Group>(node)->Cardinality = Group::ZeroOrMore; }
      | ")+"                            { cast<Group>(node)->Cardinality = Group::OneOrMore; }
    )
  ;