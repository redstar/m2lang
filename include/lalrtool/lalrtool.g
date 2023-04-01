/* Grammar for LALRtool */
%language "c++"
%define api.parser.class {Parser}
%token identifier, qualidentifier, code, argument, string
%token "%token" = kw_token, "%start" = kw_start, "%eoi" = kw_eoi
%token "%language" = kw_language, "%define" = kw_define, "%if" = kw_if
%token "%left" = kw_left, "%right" = kw_right
%start lalrtool
%%
lalrtool
  : ( header )? ( rule )+ ;

header
  : ("%start" identifier                { Builder.actOnStartSymbol(Tok.getLoc(), Tok.getData()); }
     | "%token" tokenlist
     | "%language" string               { Builder.actOnLanguage(Tok.getLoc(), Tok.getData()); }
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
                                        { Builder.actOnDefine(Loc, ident, value, type); }
     | ("%left" | "%right")
       (identifier | string)+
     | "%eoi" identifier                { Builder.actOnEoiSymbol(Tok.getLoc(), Tok.getData()); }
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
                                        { Builder.actOnTerminal(Loc, val, ext); }
  ;

rule
  :                                     { Nonterminal *NT = nullptr; }
    nonterminal<NT>
    ":" rhs<NT>
    ";"
  ;

nonterminal<Nonterminal *&NT>
  : identifier                          { NT = Builder.actOnNonterminal(Tok.getLoc(), Tok.getData()); }
    ( argument
    )?
    ( code
    )?
  ;

rhs<Nonterminal *NT>
  :                                     { Rule *R = Builder.actOnRule(NT); }
    sequence<R>
    (
      "|"                               { R = Builder.actOnRule(NT, R); }
      sequence<R>
    )*
  ;

sequence<Rule *R>
  :
    (
        identifier                      { Builder.actOnSymbolRef(R, Tok.getLoc(), Tok.getData()); }
        ( argument )?
      | string                          { Builder.actOnSymbolRef(R, Tok.getLoc(), Tok.getData(), true); }
      | code                            { Builder.actOnAction(R, Tok.getLoc(), Tok.getData()); }
      | "%if" code                      { Builder.actOnPredicate(R, Tok.getLoc(), Tok.getData()); }
    )*
  ;
