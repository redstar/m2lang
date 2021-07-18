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
    | "%language" string      { Builder.actOnLanguage(tokenAs<Identifier>(Tok)); }
    )*
    "%%"                      { Builder.finalizeTypedefs(); }
  ;

typedeflist
  : typedef ("," typedef )*
  ;

typedef
  : identifier                { Identifier Name = tokenAs<Identifier>(Tok); }
    code                      { Builder.actOnTypedef(Name, Tok.getData()); }
  ;

typedecl
  :                           { Class::ClassType  CType; }
    ( "%node"                 { CType = Class::Node; }
    | "%base"                 { CType = Class::Base; }
    | "%plain"                { CType = Class::Plain; }
    )
    identifier                { Identifier Name = tokenAs<Identifier>(Tok); }
                              { llvm::StringRef Super; }
    ( super<Super> )?
    "="                       { llvm::SmallVector<Member*, 8> MemberList; }
    body<MemberList>
    ";"                       { Builder.actOnTypedecl(CType, Name, Super, MemberList); }
  ;

super<llvm::StringRef &Super>
  : "<:" identifier           { Super = Tok.getData(); }
  ;

body<llvm::SmallVectorImpl<Member*> &MemberList>
  : ( decl<MemberList> ( "," decl<MemberList> )* )?
  ;

decl<llvm::SmallVectorImpl<Member*> &MemberList>
  :                           { unsigned Properties = 0; }
    ( property<Properties> )?
    identifier                { Identifier Name = tokenAs<Identifier>(Tok);  }
    ":"                       { bool TypeIsList = false; }
    ( "%list"                 { TypeIsList = true; }
    )?
    identifier                { Builder.actOnField(MemberList, Properties, Name, Tok.getData(), TypeIsList); }
  | "%enum" identifier        { Identifier Name = tokenAs<Identifier>(Tok);  }
    code                      { Builder.actOnEnum(MemberList, Name, Tok.getData()); }
  ;

property<unsigned &Properties>
  : ( "%in"                   { Builder.actOnPropertyIn(Properties, Tok.getLoc()); }
    | "%out"                  { Builder.actOnPropertyOut(Properties, Tok.getLoc()); }
    )+
  ;