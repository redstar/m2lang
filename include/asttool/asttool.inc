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
bool parseSuper(const __TokenBitSet &__FollowSetCallers);
bool parseBody(const __TokenBitSet &__FollowSetCallers);
bool parseDecl(const __TokenBitSet &__FollowSetCallers);
bool parseProperty(const __TokenBitSet &__FollowSetCallers);
#endif
#ifdef PARSER_DEFINITION
const Parser::__TokenBitSet Parser::__FollowSets[] = {
  { tok::eoi },
  { tok::kw_node, tok::kw_base, tok::kw_plain },
  { tok::percentpercent },
  { tok::percentpercent, tok::comma },
  { tok::kw_node, tok::kw_base, tok::kw_plain, tok::eoi },
  { tok::equal },
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
  if ((__TokenBitSet{tok::kw_typedef, tok::kw_language, tok::percentpercent, }).contains(Tok.getKind())) {
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
  while ((__TokenBitSet{tok::kw_typedef, tok::kw_language, }).contains(Tok.getKind())) {
    if (Tok.is(tok::kw_typedef)) {
      advance();
      if (Parser::parseTypedeflist(__FollowSet))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_language)) {
      advance();
      if (expect(tok::string))
        return __errorhandler();
      /*Builder.language(Tok.getLoc(), Tok.getData());*/
      advance();
    }
  }
  if (consume(tok::percentpercent))
    return __errorhandler();
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
  if (consume(tok::identifier))
    return __errorhandler();
  if (consume(tok::code))
    return __errorhandler();
  return false;
}
bool Parser::parseTypedecl(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[4] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[4]);
  };
  if (Tok.is(tok::kw_node)) {
    advance();
  }
  else if (Tok.is(tok::kw_base)) {
    advance();
  }
  else if (Tok.is(tok::kw_plain)) {
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  if (consume(tok::identifier))
    return __errorhandler();
  if (Tok.is(tok::lesscolon)) {
    if (Parser::parseSuper(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::equal))
    return __errorhandler();
  if (Parser::parseBody(__FollowSet))
    return __errorhandler();
  if (consume(tok::semi))
    return __errorhandler();
  return false;
}
bool Parser::parseSuper(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[5] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[5]);
  };
  if (consume(tok::lesscolon))
    return __errorhandler();
  if (consume(tok::identifier))
    return __errorhandler();
  return false;
}
bool Parser::parseBody(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[6] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[6]);
  };
  if ((__TokenBitSet{tok::identifier, tok::kw_enum, tok::kw_in, tok::kw_out, }).contains(Tok.getKind())) {
    if (Parser::parseDecl(__FollowSet))
      return __errorhandler();
    while (Tok.is(tok::comma)) {
      advance();
      if (Parser::parseDecl(__FollowSet))
        return __errorhandler();
    }
  }
  return false;
}
bool Parser::parseDecl(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  if ((__TokenBitSet{tok::identifier, tok::kw_in, tok::kw_out, }).contains(Tok.getKind())) {
    if ((__TokenBitSet{tok::kw_in, tok::kw_out, }).contains(Tok.getKind())) {
      if (Parser::parseProperty(__FollowSet))
        return __errorhandler();
    }
    if (consume(tok::identifier))
      return __errorhandler();
    if (consume(tok::colon))
      return __errorhandler();
    if (Tok.is(tok::kw_list)) {
      advance();
    }
    if (consume(tok::identifier))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_enum)) {
    advance();
    if (consume(tok::identifier))
      return __errorhandler();
    if (consume(tok::code))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool Parser::parseProperty(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[8] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[8]);
  };
  if (Tok.is(tok::kw_in)) {
    advance();
  }
  else if (Tok.is(tok::kw_out)) {
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
#endif
