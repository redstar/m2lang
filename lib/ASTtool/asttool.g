/* Grammar for ASTtool */
%language "c++"
%define api.parser.class {Parser}
%token identifier, code, string
%token "%typedef" = kw_typedef, "%node" = kw_node, "%base" = kw_base
%token "%language" = kw_language, "%plain" = kw_plain, "%list" = kw_list
%token "%enum" = kw_enum, "%in" = kw_in, "%out" = kw_out, "%let" = kw_let
%token "%default" = kw_default
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
                              { Class *Super = nullptr; }
    ( super<Super> )?
                              { MemberList MemberList; LetList LetList; }
    ( "=" body<MemberList, LetList, Super> )?
    ";"                       { Builder.actOnTypedecl(CType, Name, Super, MemberList, LetList); }
  ;

super<Class *&Super>
  : "<:" identifier           { Builder.actOnSuperClass(Super, tokenAs<Identifier>(Tok)); }
  ;

body<llvm::SmallVectorImpl<Member*> &MemberList, llvm::SmallVectorImpl<Let *> &LetList, Class *Super>
  : ( decl<MemberList, LetList, Super> ( "," decl<MemberList, LetList, Super> )* )?
  ;

decl<llvm::SmallVectorImpl<Member*> &MemberList, llvm::SmallVectorImpl<Let *> &LetList, Class *Super>
  :                           { unsigned Properties = 0; }
    ( property<Properties> )?
    identifier                { Identifier Name = tokenAs<Identifier>(Tok);  }
    ":"                       { bool TypeIsList = false; }
    ( "%list"                 { TypeIsList = true; }
    )?
    identifier                { Identifier TypeName = tokenAs<Identifier>(Tok); }
                              { bool IsDefault = false; llvm::StringRef Code; }
    ( init<IsDefault, Code> )?
                              { Builder.actOnField(MemberList, Properties, Name, TypeName, TypeIsList, IsDefault, Code); }
  | "%enum" identifier        { Identifier Name = tokenAs<Identifier>(Tok);  }
    code                      { Builder.actOnEnum(MemberList, Name, Tok.getData()); }
  | "%let" identifier         { Identifier Name = tokenAs<Identifier>(Tok);  }
    "="                       { bool IsDefault; llvm::StringRef Code; }
    init<IsDefault, Code>     { Builder.actOnLet(LetList, Name, Super, IsDefault, Code); }
  ;

init<bool &IsDefault, llvm::StringRef &Code>
  : "=" ( "%default"          { IsDefault = true; }
        | code                { IsDefault = false; Code = Tok.getData(); }
        )
  ;

property<unsigned &Properties>
  : ( "%in"                   { Builder.actOnPropertyIn(Properties, Tok.getLoc()); }
    | "%out"                  { Builder.actOnPropertyOut(Properties, Tok.getLoc()); }
    )+
  ;