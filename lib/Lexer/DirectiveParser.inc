#ifdef DIRECTIVEPARSER_DECLARATION
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
bool parseDirective(const __TokenBitSet &__FollowSetCallers);
bool parseExpr(const __TokenBitSet &__FollowSetCallers, bool &Val);
bool parseTerm(const __TokenBitSet &__FollowSetCallers, bool &Val);
bool parseFactor(const __TokenBitSet &__FollowSetCallers, bool &Val);
#endif
#ifdef DIRECTIVEPARSER_DEFINITION
const DirectiveParser::__TokenBitSet DirectiveParser::__FollowSets[] = {
  { tok::eof },
  { tok::kw_THEN, tok::r_paren },
  { tok::kw_THEN, tok::kw_OR, tok::r_paren },
  { tok::kw_THEN, tok::kw_OR, tok::kw_AND, tok::r_paren },
};
bool DirectiveParser::__skipUntil(const DirectiveParser::__TokenBitSet &ActiveSets, const DirectiveParser::__TokenBitSet &CurrentSet) {
  DirectiveParser::__TokenBitSet StopSets = ActiveSets | tok::eof;
  while (!StopSets.contains(Tok.getKind())) {
    advance();
  }
  return CurrentSet.contains(Tok.getKind());
}
bool DirectiveParser::parseDirective(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  if (consume(tok::lessstar))
    return __errorhandler();
  if (Tok.is(tok::kw_IF)) {
    bool Val = false;
    SMLoc Loc = Tok.getLocation();
    advance();
    if (DirectiveParser::parseExpr(__FollowSet, Val))
      return __errorhandler();
    if (consume(tok::kw_THEN))
      return __errorhandler();
    if (expect(tok::stargreater))
      return __errorhandler();
    handleIf(Loc, Val);
    advance();
  }
  else if (Tok.is(tok::kw_ELSIF)) {
    bool Val = false;
    SMLoc Loc = Tok.getLocation();
    advance();
    if (DirectiveParser::parseExpr(__FollowSet, Val))
      return __errorhandler();
    if (consume(tok::kw_THEN))
      return __errorhandler();
    if (expect(tok::stargreater))
      return __errorhandler();
    handleElsif(Loc, Val);
    advance();
  }
  else if (Tok.is(tok::kw_ELSE)) {
    SMLoc Loc = Tok.getLocation();
    advance();
    if (expect(tok::stargreater))
      return __errorhandler();
    handleElse(Loc);
    advance();
  }
  else if (Tok.is(tok::kw_END)) {
    SMLoc Loc = Tok.getLocation();
    advance();
    if (expect(tok::stargreater))
      return __errorhandler();
    handleEnd(Loc);
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool DirectiveParser::parseExpr(const __TokenBitSet &__FollowSetCallers, bool &Val) {
  const __TokenBitSet __FollowSet = __FollowSets[1] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[1]);
  };
  if (DirectiveParser::parseTerm(__FollowSet, Val))
    return __errorhandler();
  while (Tok.is(tok::kw_OR)) {
    bool ValRight = false;
    advance();
    if (DirectiveParser::parseTerm(__FollowSet, ValRight))
      return __errorhandler();
    Val |= ValRight;
  }
  return false;
}
bool DirectiveParser::parseTerm(const __TokenBitSet &__FollowSetCallers, bool &Val) {
  const __TokenBitSet __FollowSet = __FollowSets[2] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[2]);
  };
  if (DirectiveParser::parseFactor(__FollowSet, Val))
    return __errorhandler();
  while (Tok.is(tok::kw_AND)) {
    bool ValRight = true;
    advance();
    if (DirectiveParser::parseFactor(__FollowSet, ValRight))
      return __errorhandler();
    Val &= ValRight;
  }
  return false;
}
bool DirectiveParser::parseFactor(const __TokenBitSet &__FollowSetCallers, bool &Val) {
  const __TokenBitSet __FollowSet = __FollowSets[3] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[3]);
  };
  if (Tok.is(tok::identifier)) {
    Val = lookup(Tok.getIdentifier());
    advance();
  }
  else if (Tok.is(tok::l_paren)) {
    advance();
    if (DirectiveParser::parseExpr(__FollowSet, Val))
      return __errorhandler();
    if (consume(tok::r_paren))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_NOT)) {
    advance();
    if (DirectiveParser::parseFactor(__FollowSet, Val))
      return __errorhandler();
    Val = !Val;
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
#endif
