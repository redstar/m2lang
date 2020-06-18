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
bool parseSingleDirective(const __TokenBitSet &__FollowSetCallers);
bool parseAssignment(const __TokenBitSet &__FollowSetCallers);
bool parseEnvironment(const __TokenBitSet &__FollowSetCallers);
bool parseDefinition(const __TokenBitSet &__FollowSetCallers);
bool parseSave_restore(const __TokenBitSet &__FollowSetCallers);
bool parseCondition(const __TokenBitSet &__FollowSetCallers);
bool parseIfpart(const __TokenBitSet &__FollowSetCallers);
bool parseElsifpart(const __TokenBitSet &__FollowSetCallers);
bool parseElsepart(const __TokenBitSet &__FollowSetCallers);
bool parseEndifpart(const __TokenBitSet &__FollowSetCallers);
bool parseExpr(const __TokenBitSet &__FollowSetCallers, StringRef &Val);
bool parseCondExpr(const __TokenBitSet &__FollowSetCallers, StringRef &Val);
bool parseCondTerm(const __TokenBitSet &__FollowSetCallers, StringRef &Val);
bool parseCondFactor(const __TokenBitSet &__FollowSetCallers, StringRef &Val);
bool parseValue(const __TokenBitSet &__FollowSetCallers, StringRef &Val);
#endif
#ifdef DIRECTIVEPARSER_DEFINITION
const DirectiveParser::__TokenBitSet DirectiveParser::__FollowSets[] = {
  { tok::eof },
  { tok::semi, tok::stargreater },
  { tok::r_paren, tok::kw_THEN },
  { tok::r_paren, tok::kw_THEN, tok::equal, tok::hash },
  { tok::r_paren, tok::kw_THEN, tok::equal, tok::hash, tok::kw_OR },
  { tok::r_paren, tok::kw_THEN, tok::equal, tok::hash, tok::kw_OR, tok::kw_AND },
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
  if (DirectiveParser::parseSingleDirective(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::semi)) {
    advance();
    if (DirectiveParser::parseSingleDirective(__FollowSet))
      return __errorhandler();
  }
  if (consume(tok::stargreater))
    return __errorhandler();
  return false;
}
bool DirectiveParser::parseSingleDirective(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[1] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[1]);
  };
  if ((__TokenBitSet{tok::identifier, tok::kw_ASSIGN, tok::kw_ENVIRON, tok::kw_DEFINE, tok::kw_PUSH, tok::kw_POP, tok::kw_IF, tok::kw_ELSIF, tok::kw_ELSE, tok::kw_END, }).contains(Tok.getKind())) {
    if ((__TokenBitSet{tok::identifier, tok::kw_ASSIGN, }).contains(Tok.getKind())) {
      if (DirectiveParser::parseAssignment(__FollowSet))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_ENVIRON)) {
      if (DirectiveParser::parseEnvironment(__FollowSet))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_DEFINE)) {
      if (DirectiveParser::parseDefinition(__FollowSet))
        return __errorhandler();
    }
    else if ((__TokenBitSet{tok::kw_PUSH, tok::kw_POP, }).contains(Tok.getKind())) {
      if (DirectiveParser::parseSave_restore(__FollowSet))
        return __errorhandler();
    }
    else if ((__TokenBitSet{tok::kw_IF, tok::kw_ELSIF, tok::kw_ELSE, tok::kw_END, }).contains(Tok.getKind())) {
      if (DirectiveParser::parseCondition(__FollowSet))
        return __errorhandler();
    }
  }
  return false;
}
bool DirectiveParser::parseAssignment(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[1] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[1]);
  };
  StringRef Val;
  if (Tok.is(tok::identifier)) {
    StringRef Identifier = Tok.getIdentifier(); SMLoc Loc = Tok.getLocation();
    advance();
    if (consume(tok::l_paren))
      return __errorhandler();
    if (DirectiveParser::parseValue(__FollowSet, Val))
      return __errorhandler();
    if (expect(tok::r_paren))
      return __errorhandler();
    actOnAssignment(Loc, Identifier, Val);
    advance();
  }
  else if (Tok.is(tok::kw_ASSIGN)) {
    advance();
    if (consume(tok::l_paren))
      return __errorhandler();
    if (expect(tok::identifier))
      return __errorhandler();
    StringRef Identifier = Tok.getIdentifier(); SMLoc Loc = Tok.getLocation();
    advance();
    if (consume(tok::comma))
      return __errorhandler();
    if (DirectiveParser::parseValue(__FollowSet, Val))
      return __errorhandler();
    if (expect(tok::r_paren))
      return __errorhandler();
    actOnAssignment(Loc, Identifier, Val);
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool DirectiveParser::parseEnvironment(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[1] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[1]);
  };
  if (consume(tok::kw_ENVIRON))
    return __errorhandler();
  if (consume(tok::l_paren))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  StringRef Identifier = Tok.getIdentifier(); SMLoc Loc = Tok.getLocation();
  advance();
  if (expect(tok::comma))
    return __errorhandler();
  StringRef Val;
  advance();
  if (DirectiveParser::parseValue(__FollowSet, Val))
    return __errorhandler();
  if (expect(tok::r_paren))
    return __errorhandler();
  actOnEnvironment(Loc, Identifier, Val);
  advance();
  return false;
}
bool DirectiveParser::parseDefinition(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[1] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[1]);
  };
  if (consume(tok::kw_DEFINE))
    return __errorhandler();
  if (consume(tok::l_paren))
    return __errorhandler();
  if (expect(tok::identifier))
    return __errorhandler();
  StringRef Identifier = Tok.getIdentifier(); SMLoc Loc = Tok.getLocation();
  advance();
  if (expect(tok::comma))
    return __errorhandler();
  StringRef Val;
  advance();
  if (DirectiveParser::parseValue(__FollowSet, Val))
    return __errorhandler();
  if (expect(tok::r_paren))
    return __errorhandler();
  actOnDefinition(Loc, Identifier, Val);
  advance();
  return false;
}
bool DirectiveParser::parseSave_restore(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[1] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[1]);
  };
  if (Tok.is(tok::kw_PUSH)) {
    advance();
  }
  else if (Tok.is(tok::kw_POP)) {
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool DirectiveParser::parseCondition(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[1] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[1]);
  };
  if (Tok.is(tok::kw_IF)) {
    if (DirectiveParser::parseIfpart(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_ELSIF)) {
    if (DirectiveParser::parseElsifpart(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_ELSE)) {
    if (DirectiveParser::parseElsepart(__FollowSet))
      return __errorhandler();
  }
  else if (Tok.is(tok::kw_END)) {
    if (DirectiveParser::parseEndifpart(__FollowSet))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool DirectiveParser::parseIfpart(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[1] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[1]);
  };
  if (expect(tok::kw_IF))
    return __errorhandler();
  SMLoc Loc = Tok.getLocation(); StringRef Val;
  advance();
  if (DirectiveParser::parseExpr(__FollowSet, Val))
    return __errorhandler();
  if (expect(tok::kw_THEN))
    return __errorhandler();
  actOnIf(Loc, Val);
  advance();
  return false;
}
bool DirectiveParser::parseElsifpart(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[1] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[1]);
  };
  if (expect(tok::kw_ELSIF))
    return __errorhandler();
  SMLoc Loc = Tok.getLocation(); StringRef Val;
  actOnElsIf(Loc);
  advance();
  if (DirectiveParser::parseExpr(__FollowSet, Val))
    return __errorhandler();
  if (expect(tok::kw_THEN))
    return __errorhandler();
  actOnElsIf(Loc, Val);
  advance();
  return false;
}
bool DirectiveParser::parseElsepart(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[1] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[1]);
  };
  if (expect(tok::kw_ELSE))
    return __errorhandler();
  actOnElse(Tok.getLocation());
  advance();
  return false;
}
bool DirectiveParser::parseEndifpart(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[1] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[1]);
  };
  if (expect(tok::kw_END))
    return __errorhandler();
  actOnEnd(Tok.getLocation());
  advance();
  return false;
}
bool DirectiveParser::parseExpr(const __TokenBitSet &__FollowSetCallers, StringRef &Val) {
  const __TokenBitSet __FollowSet = __FollowSets[2] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[2]);
  };
  if (DirectiveParser::parseCondExpr(__FollowSet, Val))
    return __errorhandler();
  if ((__TokenBitSet{tok::equal, tok::hash, }).contains(Tok.getKind())) {
    tok::TokenKind Op = tok::unknown;
    if (Tok.is(tok::equal)) {
      Op = Tok.getKind();
      advance();
    }
    else if (Tok.is(tok::hash)) {
      Op = Tok.getKind();
      advance();
    }
    else {
      error();
      return __errorhandler();
    }
    StringRef RightVal;
    if (DirectiveParser::parseCondExpr(__FollowSet, RightVal))
      return __errorhandler();
    Val = actOnRelation(Op, Val, RightVal);
  }
  return false;
}
bool DirectiveParser::parseCondExpr(const __TokenBitSet &__FollowSetCallers, StringRef &Val) {
  const __TokenBitSet __FollowSet = __FollowSets[3] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[3]);
  };
  if (DirectiveParser::parseCondTerm(__FollowSet, Val))
    return __errorhandler();
  while (Tok.is(tok::kw_OR)) {
    StringRef RightVal;
    advance();
    if (DirectiveParser::parseCondTerm(__FollowSet, Val))
      return __errorhandler();
    Val = actOnOr(Val, RightVal);
  }
  return false;
}
bool DirectiveParser::parseCondTerm(const __TokenBitSet &__FollowSetCallers, StringRef &Val) {
  const __TokenBitSet __FollowSet = __FollowSets[4] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[4]);
  };
  if (DirectiveParser::parseCondFactor(__FollowSet, Val))
    return __errorhandler();
  while (Tok.is(tok::kw_AND)) {
    StringRef RightVal;
    advance();
    if (DirectiveParser::parseCondFactor(__FollowSet, RightVal))
      return __errorhandler();
    Val = actOnAnd(Val, RightVal);
  }
  return false;
}
bool DirectiveParser::parseCondFactor(const __TokenBitSet &__FollowSetCallers, StringRef &Val) {
  const __TokenBitSet __FollowSet = __FollowSets[5] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[5]);
  };
  if (Tok.is(tok::kw_NOT)) {
    advance();
    if (DirectiveParser::parseCondFactor(__FollowSet, Val))
      return __errorhandler();
    actOnNot(Val);
  }
  else if ((__TokenBitSet{tok::identifier, tok::string_literal, }).contains(Tok.getKind())) {
    if (DirectiveParser::parseValue(__FollowSet, Val))
      return __errorhandler();
  }
  else if (Tok.is(tok::l_paren)) {
    advance();
    if (DirectiveParser::parseExpr(__FollowSet, Val))
      return __errorhandler();
    if (consume(tok::r_paren))
      return __errorhandler();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
bool DirectiveParser::parseValue(const __TokenBitSet &__FollowSetCallers, StringRef &Val) {
  const __TokenBitSet __FollowSet = __FollowSets[5] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[5]);
  };
  if (Tok.is(tok::string_literal)) {
    Val = Tok.getLiteralData().substr(1, Tok.getLiteralData().size()-2);
    advance();
  }
  else if (Tok.is(tok::identifier)) {
    Val = actOnIdentifierValue(Tok.getIdentifier());
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
#endif
