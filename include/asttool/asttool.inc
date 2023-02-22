#ifdef PARSER_DECLARATION
template <unsigned NBits> struct BitSet {
  typedef uintptr_t BitWord;

  enum { BITWORD_SIZE = (unsigned)sizeof(BitWord) * CHAR_BIT };
  enum { MEM_SIZE = (NBits + BITWORD_SIZE - 1) / BITWORD_SIZE };

  BitWord Data[MEM_SIZE];

  BitSet() { clear(); }

  template <typename... Ts> BitSet(Ts... BitsToSet) {
    clear();
    set(BitsToSet...);
  }

  void clear() { std::memset(Data, 0, sizeof(Data)); }

  template <typename T> bool contains(T Idx) const {
    return (Data[Idx / BITWORD_SIZE] & BitWord(1) << (Idx % BITWORD_SIZE)) != 0;
  }

  BitSet<NBits> &operator|=(const BitSet<NBits> &Other) {
    for (unsigned I = 0; I < MEM_SIZE; ++I)
      Data[I] |= Other.Data[I];
    return *this;
  }

  BitSet<NBits> operator|(const BitSet<NBits> &Other) const {
    BitSet<NBits> Result;
    for (unsigned I = 0; I < MEM_SIZE; ++I)
      Result.Data[I] = Data[I] | Other.Data[I];
    return Result;
  }

  template <typename T> void set(T Idx) {
    Data[Idx / BITWORD_SIZE] |= BitWord(1) << (Idx % BITWORD_SIZE);
  }
  template <typename T, typename... Ts> void set(T Idx, Ts... Idxs) {
    set(Idx);
    set(Idxs...);
  }
};
using __TokenBitSet = BitSet<tok::NUM_TOKENS>;
static const __TokenBitSet __FollowSets[];bool __skipUntil(const __TokenBitSet &ActiveSets, const __TokenBitSet &CurrentSet);
bool parseAsttool(const __TokenBitSet &__FollowSetCallers);
bool parseHeader(const __TokenBitSet &__FollowSetCallers);
bool parseTypedeflist(const __TokenBitSet &__FollowSetCallers);
bool parseTypedef(const __TokenBitSet &__FollowSetCallers);
bool parseTypedecl(const __TokenBitSet &__FollowSetCallers);
bool parseSuper(const __TokenBitSet &__FollowSetCallers, Class *&Super);
bool parseBody(const __TokenBitSet &__FollowSetCallers, llvm::SmallVectorImpl<Member*> &MemberList, llvm::SmallVectorImpl<Let *> &LetList, Class *Super);
bool parseDecl(const __TokenBitSet &__FollowSetCallers, llvm::SmallVectorImpl<Member*> &MemberList, llvm::SmallVectorImpl<Let *> &LetList, Class *Super);
bool parseInit(const __TokenBitSet &__FollowSetCallers, bool &IsDefault, llvm::StringRef &Code);
bool parseProperty(const __TokenBitSet &__FollowSetCallers, unsigned &Properties);
#endif
#ifdef PARSER_DEFINITION
const Parser::__TokenBitSet Parser::__FollowSets[] = {
  { tok::eoi },
  { tok::kw_node, tok::kw_base, tok::kw_plain },
  { tok::percentpercent },
  { tok::percentpercent, tok::comma },
  { tok::kw_node, tok::kw_base, tok::kw_plain, tok::eoi },
  { tok::equal, tok::semi },
  { tok::semi },
  { tok::comma, tok::semi },
  { tok::identifier },
};
bool Parser::__skipUntil(const Parser::__TokenBitSet &ActiveSets, const Parser::__TokenBitSet &CurrentSet) {
  Parser::__TokenBitSet StopSets = ActiveSets | tok::eoi;
  while (!StopSets.contains(Tok.getKind())) {
    advance();
  }
  return CurrentSet.contains(Tok.getKind());
}
bool Parser::parseAsttool(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  if ((__TokenBitSet{tok::kw_typedef, tok::kw_language, tok::kw_define, tok::percentpercent, }).contains(Tok.getKind())) {
    if (Parser::parseHeader(__FollowSet))
      return __errorhandler();
  }
  do {
    if (Parser::parseTypedecl(__FollowSet))
      return __errorhandler();
  } while ((__TokenBitSet{tok::kw_node, tok::kw_base, tok::kw_plain, }).contains(Tok.getKind()));
  return false;
}
bool Parser::parseHeader(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[1] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[1]);
  };
  while ((__TokenBitSet{tok::kw_typedef, tok::kw_language, tok::kw_define, }).contains(Tok.getKind())) {
    if (Tok.is(tok::kw_typedef)) {
      advance();
      if (Parser::parseTypedeflist(__FollowSet))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_language)) {
      advance();
      if (expect(tok::string))
        return __errorhandler();
      Builder.actOnLanguage(tokenAs<Identifier>(Tok));
      advance();
    }
    else if (Tok.is(tok::kw_define)) {
      SMLoc Loc; StringRef Ident, Value; var::VarType Type = var::Flag;
      advance();
      Loc = Tok.getLoc(); Ident = Tok.getData();
      if (Tok.is(tok::identifier)) {
        advance();
      }
      else if (Tok.is(tok::qualidentifier)) {
        advance();
      }
      else {
        error();
        return __errorhandler();
      }
      if ((__TokenBitSet{tok::identifier, tok::qualidentifier, tok::code, tok::string, }).contains(Tok.getKind())) {
        Value = Tok.getData();
        if (Tok.is(tok::code)) {
          Type = var::Code;
          advance();
        }
        else if (Tok.is(tok::identifier)) {
          Type = var::Identifier;
          advance();
        }
        else if (Tok.is(tok::qualidentifier)) {
          Type = var::Identifier;
          advance();
        }
        else if (Tok.is(tok::string)) {
          Type = var::String;
          advance();
        }
        else {
          error();
          return __errorhandler();
        }
      }
      Builder.actOnDefine(Loc, Ident, Value, Type);
    }
  }
  if (expect(tok::percentpercent))
    return __errorhandler();
  Builder.finalizeTypedefs();
  advance();
  return false;
}
bool Parser::parseTypedeflist(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[2] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[2]);
  };
  if (Parser::parseTypedef(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (Parser::parseTypedef(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool Parser::parseTypedef(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[3] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[3]);
  };
  if (expect(tok::identifier))
    return __errorhandler();
  Identifier Name = tokenAs<Identifier>(Tok);
  advance();
  if (expect(tok::code))
    return __errorhandler();
  Builder.actOnTypedef(Name, Tok.getData());
  advance();
  return false;
}
bool Parser::parseTypedecl(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[4] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[4]);
  };
  Class::ClassType  CType;
  if (Tok.is(tok::kw_node)) {
    CType = Class::Node;
    advance();
  }
  else if (Tok.is(tok::kw_base)) {
    CType = Class::Base;
    advance();
  }
  else if (Tok.is(tok::kw_plain)) {
    CType = Class::Plain;
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  if (expect(tok::identifier))
    return __errorhandler();
  Identifier Name = tokenAs<Identifier>(Tok);
  Class *Super = nullptr;
  advance();
  if (Tok.is(tok::lesscolon)) {
    if (Parser::parseSuper(__FollowSet, Super))
      return __errorhandler();
  }
  MemberList MemberList; LetList LetList;
  if (Tok.is(tok::equal)) {
    advance();
    if (Parser::parseBody(__FollowSet, MemberList, LetList, Super))
      return __errorhandler();
  }
  if (expect(tok::semi))
    return __errorhandler();
  Builder.actOnTypedecl(CType, Name, Super, MemberList, LetList);
  advance();
  return false;
}
bool Parser::parseSuper(const __TokenBitSet &__FollowSetCallers, Class *&Super) {
  const __TokenBitSet __FollowSet = __FollowSets[5] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[5]);
  };
  if (consume(tok::lesscolon))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  Builder.actOnSuperClass(Super, tokenAs<Identifier>(Tok));
  advance();
  return false;
}
bool Parser::parseBody(const __TokenBitSet &__FollowSetCallers, llvm::SmallVectorImpl<Member*> &MemberList, llvm::SmallVectorImpl<Let *> &LetList, Class *Super) {
  const __TokenBitSet __FollowSet = __FollowSets[6] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[6]);
  };
  if ((__TokenBitSet{tok::identifier, tok::kw_enum, tok::kw_in, tok::kw_out, tok::kw_let, }).contains(Tok.getKind())) {
    if (Parser::parseDecl(__FollowSet, MemberList, LetList, Super))
      return __errorhandler();
    while (Tok.is(tok::comma)) {
      advance();
      if (Parser::parseDecl(__FollowSet, MemberList, LetList, Super))
        return __errorhandler();
    }
  }
  return false;
}
bool Parser::parseDecl(const __TokenBitSet &__FollowSetCallers, llvm::SmallVectorImpl<Member*> &MemberList, llvm::SmallVectorImpl<Let *> &LetList, Class *Super) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if ((__TokenBitSet{tok::identifier, tok::kw_in, tok::kw_out, }).contains(Tok.getKind())) {
    unsigned Properties = 0;
    if ((__TokenBitSet{tok::kw_in, tok::kw_out, }).contains(Tok.getKind())) {
      if (Parser::parseProperty(__FollowSet, Properties))
        return __errorhandler();
    }
    if (expect(tok::identifier))
      return __errorhandler();
    Identifier Name = tokenAs<Identifier>(Tok);
    advance();
    if (expect(tok::colon))
      return __errorhandler();
    bool TypeIsList = false;
    advance();
    if (Tok.is(tok::kw_list)) {
      TypeIsList = true;
      advance();
    }
    if (expect(tok::identifier))
      return __errorhandler();
    Identifier TypeName = tokenAs<Identifier>(Tok);
    bool IsDefault = false; llvm::StringRef Code;
    advance();
    if (Tok.is(tok::equal)) {
      if (Parser::parseInit(__FollowSet, IsDefault, Code))
        return __errorhandler();
    }
    Builder.actOnField(MemberList, Properties, Name, TypeName, TypeIsList, IsDefault, Code);
  }
  else if (Tok.is(tok::kw_enum)) {
    advance();
    if (expect(tok::identifier))
      return __errorhandler();
    Identifier Name = tokenAs<Identifier>(Tok);
    advance();
    if (expect(tok::code))
      return __errorhandler();
    Builder.actOnEnum(MemberList, Name, Tok.getData());
    advance();
  }
  else if (Tok.is(tok::kw_let)) {
    advance();
    if (expect(tok::identifier))
      return __errorhandler();
    Identifier Name = tokenAs<Identifier>(Tok);
    bool IsDefault; llvm::StringRef Code;
    advance();
    if (Parser::parseInit(__FollowSet, IsDefault, Code))
      return __errorhandler();
    Builder.actOnLet(LetList, Name, Super, IsDefault, Code);
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool Parser::parseInit(const __TokenBitSet &__FollowSetCallers, bool &IsDefault, llvm::StringRef &Code) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if (consume(tok::equal))
    return __errorhandler();
  if (Tok.is(tok::kw_default)) {
    IsDefault = true;
    advance();
  }
  else if (Tok.is(tok::code)) {
    IsDefault = false; Code = Tok.getData();
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool Parser::parseProperty(const __TokenBitSet &__FollowSetCallers, unsigned &Properties) {
  const __TokenBitSet __FollowSet = __FollowSets[8] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[8]);
  };
  do {
    if (Tok.is(tok::kw_in)) {
      Builder.actOnPropertyIn(Properties, Tok.getLoc());
      advance();
    }
    else if (Tok.is(tok::kw_out)) {
      Builder.actOnPropertyOut(Properties, Tok.getLoc());
      advance();
    }
    else {
      error();
      return __errorhandler();
    }
  } while ((__TokenBitSet{tok::kw_in, tok::kw_out, }).contains(Tok.getKind()));
  return false;
}
#endif
