#ifdef PARSER_DECLARATION
template <unsigned NBits> struct BitSet {
  typedef uintptr_t BitWord;

  static constexpr unsigned BITWORD_SIZE = static_cast<unsigned>(sizeof(BitWord)) * CHAR_BIT;
  static constexpr unsigned MEM_SIZE = (NBits + BITWORD_SIZE - 1) / BITWORD_SIZE;

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
using _TokenBitSet = BitSet<tok::NUM_TOKENS>;
static const _TokenBitSet _FollowSets[];bool _skipUntil(const _TokenBitSet &ActiveSets, const _TokenBitSet &CurrentSet);
bool parseAsttool(const _TokenBitSet &_FollowSetCallers);
bool parseHeader(const _TokenBitSet &_FollowSetCallers);
bool parseTypedeflist(const _TokenBitSet &_FollowSetCallers);
bool parseTypedef(const _TokenBitSet &_FollowSetCallers);
bool parseTypedecl(const _TokenBitSet &_FollowSetCallers);
bool parseSuper(const _TokenBitSet &_FollowSetCallers, Class *&Super);
bool parseBody(const _TokenBitSet &_FollowSetCallers, llvm::SmallVectorImpl<Member*> &MemberList, llvm::SmallVectorImpl<Let *> &LetList, Class *Super);
bool parseDecl(const _TokenBitSet &_FollowSetCallers, llvm::SmallVectorImpl<Member*> &MemberList, llvm::SmallVectorImpl<Let *> &LetList, Class *Super);
bool parseInit(const _TokenBitSet &_FollowSetCallers, bool &IsDefault, llvm::StringRef &Code);
bool parseProperty(const _TokenBitSet &_FollowSetCallers, unsigned &Properties);
#endif
#ifdef PARSER_DEFINITION
const Parser::_TokenBitSet Parser::_FollowSets[] = {
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
bool Parser::_skipUntil(const Parser::_TokenBitSet &ActiveSets, const Parser::_TokenBitSet &CurrentSet) {
  Parser::_TokenBitSet StopSets = ActiveSets | tok::eoi;
  while (!StopSets.contains(Tok.getKind())) {
    advance();
  }
  return CurrentSet.contains(Tok.getKind());
}
bool Parser::parseAsttool(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[0] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[0]);
  };
  if ((_TokenBitSet{tok::kw_typedef, tok::kw_language, tok::kw_define, tok::percentpercent, }).contains(Tok.getKind())) {
    if (Parser::parseHeader(_FollowSet))
      return _errorhandler();
  }
  do {
    if (Parser::parseTypedecl(_FollowSet))
      return _errorhandler();
  } while ((_TokenBitSet{tok::kw_node, tok::kw_base, tok::kw_plain, }).contains(Tok.getKind()));
  return false;
}
bool Parser::parseHeader(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[1] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[1]);
  };
  while ((_TokenBitSet{tok::kw_typedef, tok::kw_language, tok::kw_define, }).contains(Tok.getKind())) {
    if (Tok.is(tok::kw_typedef)) {
      advance();
      if (Parser::parseTypedeflist(_FollowSet))
        return _errorhandler();
    }
    else if (Tok.is(tok::kw_language)) {
      advance();
      if (expect(tok::string))
        return _errorhandler();
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
        return _errorhandler();
      }
      if ((_TokenBitSet{tok::identifier, tok::qualidentifier, tok::code, tok::string, }).contains(Tok.getKind())) {
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
          return _errorhandler();
        }
      }
      Builder.actOnDefine(Loc, Ident, Value, Type);
    }
  }
  if (expect(tok::percentpercent))
    return _errorhandler();
  Builder.finalizeTypedefs();
  advance();
  return false;
}
bool Parser::parseTypedeflist(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[2] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[2]);
  };
  if (Parser::parseTypedef(_FollowSet))
    return _errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (Parser::parseTypedef(_FollowSet))
      return _errorhandler();
  }
  return false;
}
bool Parser::parseTypedef(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[3] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[3]);
  };
  if (expect(tok::identifier))
    return _errorhandler();
  Identifier Name = tokenAs<Identifier>(Tok);
  advance();
  if (expect(tok::code))
    return _errorhandler();
  Builder.actOnTypedef(Name, Tok.getData());
  advance();
  return false;
}
bool Parser::parseTypedecl(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[4] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[4]);
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
    return _errorhandler();
  }
  if (expect(tok::identifier))
    return _errorhandler();
  Identifier Name = tokenAs<Identifier>(Tok);
  Class *Super = nullptr;
  advance();
  if (Tok.is(tok::lesscolon)) {
    if (Parser::parseSuper(_FollowSet, Super))
      return _errorhandler();
  }
  MemberList MemberList; LetList LetList;
  if (Tok.is(tok::equal)) {
    advance();
    if (Parser::parseBody(_FollowSet, MemberList, LetList, Super))
      return _errorhandler();
  }
  if (expect(tok::semi))
    return _errorhandler();
  Builder.actOnTypedecl(CType, Name, Super, MemberList, LetList);
  advance();
  return false;
}
bool Parser::parseSuper(const _TokenBitSet &_FollowSetCallers, Class *&Super) {
  const _TokenBitSet _FollowSet = _FollowSets[5] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[5]);
  };
  if (consume(tok::lesscolon))
    return _errorhandler();
  if (expect(tok::identifier))
    return _errorhandler();
  Builder.actOnSuperClass(Super, tokenAs<Identifier>(Tok));
  advance();
  return false;
}
bool Parser::parseBody(const _TokenBitSet &_FollowSetCallers, llvm::SmallVectorImpl<Member*> &MemberList, llvm::SmallVectorImpl<Let *> &LetList, Class *Super) {
  const _TokenBitSet _FollowSet = _FollowSets[6] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[6]);
  };
  if ((_TokenBitSet{tok::identifier, tok::kw_enum, tok::kw_in, tok::kw_out, tok::kw_let, }).contains(Tok.getKind())) {
    if (Parser::parseDecl(_FollowSet, MemberList, LetList, Super))
      return _errorhandler();
    while (Tok.is(tok::comma)) {
      advance();
      if (Parser::parseDecl(_FollowSet, MemberList, LetList, Super))
        return _errorhandler();
    }
  }
  return false;
}
bool Parser::parseDecl(const _TokenBitSet &_FollowSetCallers, llvm::SmallVectorImpl<Member*> &MemberList, llvm::SmallVectorImpl<Let *> &LetList, Class *Super) {
  const _TokenBitSet _FollowSet = _FollowSets[7] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[7]);
  };
  if ((_TokenBitSet{tok::identifier, tok::kw_in, tok::kw_out, }).contains(Tok.getKind())) {
    unsigned Properties = 0;
    if ((_TokenBitSet{tok::kw_in, tok::kw_out, }).contains(Tok.getKind())) {
      if (Parser::parseProperty(_FollowSet, Properties))
        return _errorhandler();
    }
    if (expect(tok::identifier))
      return _errorhandler();
    Identifier Name = tokenAs<Identifier>(Tok);
    advance();
    if (expect(tok::colon))
      return _errorhandler();
    bool TypeIsList = false;
    advance();
    if (Tok.is(tok::kw_list)) {
      TypeIsList = true;
      advance();
    }
    if (expect(tok::identifier))
      return _errorhandler();
    Identifier TypeName = tokenAs<Identifier>(Tok);
    bool IsDefault = false; llvm::StringRef Code;
    advance();
    if (Tok.is(tok::equal)) {
      if (Parser::parseInit(_FollowSet, IsDefault, Code))
        return _errorhandler();
    }
    Builder.actOnField(MemberList, Properties, Name, TypeName, TypeIsList, IsDefault, Code);
  }
  else if (Tok.is(tok::kw_enum)) {
    advance();
    if (expect(tok::identifier))
      return _errorhandler();
    Identifier Name = tokenAs<Identifier>(Tok);
    advance();
    if (expect(tok::code))
      return _errorhandler();
    Builder.actOnEnum(MemberList, Name, Tok.getData());
    advance();
  }
  else if (Tok.is(tok::kw_let)) {
    advance();
    if (expect(tok::identifier))
      return _errorhandler();
    Identifier Name = tokenAs<Identifier>(Tok);
    bool IsDefault; llvm::StringRef Code;
    advance();
    if (Parser::parseInit(_FollowSet, IsDefault, Code))
      return _errorhandler();
    Builder.actOnLet(LetList, Name, Super, IsDefault, Code);
  }
  else {
    error();
    return _errorhandler();
  }
  return false;
}
bool Parser::parseInit(const _TokenBitSet &_FollowSetCallers, bool &IsDefault, llvm::StringRef &Code) {
  const _TokenBitSet _FollowSet = _FollowSets[7] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[7]);
  };
  if (consume(tok::equal))
    return _errorhandler();
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
    return _errorhandler();
  }
  return false;
}
bool Parser::parseProperty(const _TokenBitSet &_FollowSetCallers, unsigned &Properties) {
  const _TokenBitSet _FollowSet = _FollowSets[8] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[8]);
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
      return _errorhandler();
    }
  } while ((_TokenBitSet{tok::kw_in, tok::kw_out, }).contains(Tok.getKind()));
  return false;
}
#endif
