#ifdef DIRECTIVEPARSER_DECLARATION
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
bool parseDirective(const _TokenBitSet &_FollowSetCallers);
bool parseSingleDirective(const _TokenBitSet &_FollowSetCallers);
bool parseAssignment(const _TokenBitSet &_FollowSetCallers);
bool parseEnvironment(const _TokenBitSet &_FollowSetCallers);
bool parseDefinition(const _TokenBitSet &_FollowSetCallers);
bool parseSave_restore(const _TokenBitSet &_FollowSetCallers);
bool parseCondition(const _TokenBitSet &_FollowSetCallers);
bool parseIfpart(const _TokenBitSet &_FollowSetCallers);
bool parseElsifpart(const _TokenBitSet &_FollowSetCallers);
bool parseElsepart(const _TokenBitSet &_FollowSetCallers);
bool parseEndifpart(const _TokenBitSet &_FollowSetCallers);
bool parseExpr(const _TokenBitSet &_FollowSetCallers, StringRef &Val);
bool parseCondExpr(const _TokenBitSet &_FollowSetCallers, StringRef &Val);
bool parseCondTerm(const _TokenBitSet &_FollowSetCallers, StringRef &Val);
bool parseCondFactor(const _TokenBitSet &_FollowSetCallers, StringRef &Val);
bool parseValue(const _TokenBitSet &_FollowSetCallers, StringRef &Val);
#endif
#ifdef DIRECTIVEPARSER_DEFINITION
const DirectiveParser::_TokenBitSet DirectiveParser::_FollowSets[] = {
  { tok::eof },
  { tok::semi, tok::stargreater },
  { tok::r_paren, tok::kw_THEN },
  { tok::r_paren, tok::kw_THEN, tok::equal, tok::hash },
  { tok::r_paren, tok::kw_THEN, tok::equal, tok::hash, tok::kw_OR },
  { tok::r_paren, tok::kw_THEN, tok::equal, tok::hash, tok::kw_OR, tok::kw_AND },
};
bool DirectiveParser::_skipUntil(const DirectiveParser::_TokenBitSet &ActiveSets, const DirectiveParser::_TokenBitSet &CurrentSet) {
  DirectiveParser::_TokenBitSet StopSets = ActiveSets | tok::eof;
  while (!StopSets.contains(Tok.getKind())) {
    advance();
  }
  return CurrentSet.contains(Tok.getKind());
}
bool DirectiveParser::parseDirective(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[0] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[0]);
  };
  if (consume(tok::lessstar))
    return _errorhandler();
  if (DirectiveParser::parseSingleDirective(_FollowSet))
    return _errorhandler();
  while (Tok.is(tok::semi)) {
    advance();
    if (DirectiveParser::parseSingleDirective(_FollowSet))
      return _errorhandler();
  }
  if (consume(tok::stargreater))
    return _errorhandler();
  return false;
}
bool DirectiveParser::parseSingleDirective(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[1] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[1]);
  };
  if ((_TokenBitSet{tok::identifier, tok::kw_ASSIGN, tok::kw_ENVIRON, tok::kw_DEFINE, tok::kw_PUSH, tok::kw_POP, tok::kw_IF, tok::kw_ELSIF, tok::kw_ELSE, tok::kw_END, }).contains(Tok.getKind())) {
    if ((_TokenBitSet{tok::identifier, tok::kw_ASSIGN, }).contains(Tok.getKind())) {
      if (DirectiveParser::parseAssignment(_FollowSet))
        return _errorhandler();
    }
    else if (Tok.is(tok::kw_ENVIRON)) {
      if (DirectiveParser::parseEnvironment(_FollowSet))
        return _errorhandler();
    }
    else if (Tok.is(tok::kw_DEFINE)) {
      if (DirectiveParser::parseDefinition(_FollowSet))
        return _errorhandler();
    }
    else if ((_TokenBitSet{tok::kw_PUSH, tok::kw_POP, }).contains(Tok.getKind())) {
      if (DirectiveParser::parseSave_restore(_FollowSet))
        return _errorhandler();
    }
    else if ((_TokenBitSet{tok::kw_IF, tok::kw_ELSIF, tok::kw_ELSE, tok::kw_END, }).contains(Tok.getKind())) {
      if (DirectiveParser::parseCondition(_FollowSet))
        return _errorhandler();
    }
  }
  return false;
}
bool DirectiveParser::parseAssignment(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[1] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[1]);
  };
  StringRef Val;
  if (Tok.is(tok::identifier)) {
    StringRef Identifier = Tok.getIdentifier(); SMLoc Loc = Tok.getLocation();
    advance();
    if (consume(tok::l_paren))
      return _errorhandler();
    if (DirectiveParser::parseValue(_FollowSet, Val))
      return _errorhandler();
    if (expect(tok::r_paren))
      return _errorhandler();
    actOnAssignment(Loc, Identifier, Val);
    advance();
  }
  else if (Tok.is(tok::kw_ASSIGN)) {
    advance();
    if (consume(tok::l_paren))
      return _errorhandler();
    if (expect(tok::identifier))
      return _errorhandler();
    StringRef Identifier = Tok.getIdentifier(); SMLoc Loc = Tok.getLocation();
    advance();
    if (consume(tok::comma))
      return _errorhandler();
    if (DirectiveParser::parseValue(_FollowSet, Val))
      return _errorhandler();
    if (expect(tok::r_paren))
      return _errorhandler();
    actOnAssignment(Loc, Identifier, Val);
    advance();
  }
  else {
    error();
    return _errorhandler();
  }
  return false;
}
bool DirectiveParser::parseEnvironment(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[1] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[1]);
  };
  if (consume(tok::kw_ENVIRON))
    return _errorhandler();
  if (consume(tok::l_paren))
    return _errorhandler();
  if (expect(tok::identifier))
    return _errorhandler();
  StringRef Identifier = Tok.getIdentifier(); SMLoc Loc = Tok.getLocation();
  advance();
  if (expect(tok::comma))
    return _errorhandler();
  StringRef Val;
  advance();
  if (DirectiveParser::parseValue(_FollowSet, Val))
    return _errorhandler();
  if (expect(tok::r_paren))
    return _errorhandler();
  actOnEnvironment(Loc, Identifier, Val);
  advance();
  return false;
}
bool DirectiveParser::parseDefinition(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[1] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[1]);
  };
  if (consume(tok::kw_DEFINE))
    return _errorhandler();
  if (consume(tok::l_paren))
    return _errorhandler();
  if (expect(tok::identifier))
    return _errorhandler();
  StringRef Identifier = Tok.getIdentifier(); SMLoc Loc = Tok.getLocation();
  advance();
  if (expect(tok::comma))
    return _errorhandler();
  StringRef Val;
  advance();
  if (DirectiveParser::parseValue(_FollowSet, Val))
    return _errorhandler();
  if (expect(tok::r_paren))
    return _errorhandler();
  actOnDefinition(Loc, Identifier, Val);
  advance();
  return false;
}
bool DirectiveParser::parseSave_restore(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[1] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[1]);
  };
  if (Tok.is(tok::kw_PUSH)) {
    advance();
  }
  else if (Tok.is(tok::kw_POP)) {
    advance();
  }
  else {
    error();
    return _errorhandler();
  }
  return false;
}
bool DirectiveParser::parseCondition(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[1] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[1]);
  };
  if (Tok.is(tok::kw_IF)) {
    if (DirectiveParser::parseIfpart(_FollowSet))
      return _errorhandler();
  }
  else if (Tok.is(tok::kw_ELSIF)) {
    if (DirectiveParser::parseElsifpart(_FollowSet))
      return _errorhandler();
  }
  else if (Tok.is(tok::kw_ELSE)) {
    if (DirectiveParser::parseElsepart(_FollowSet))
      return _errorhandler();
  }
  else if (Tok.is(tok::kw_END)) {
    if (DirectiveParser::parseEndifpart(_FollowSet))
      return _errorhandler();
  }
  else {
    error();
    return _errorhandler();
  }
  return false;
}
bool DirectiveParser::parseIfpart(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[1] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[1]);
  };
  if (expect(tok::kw_IF))
    return _errorhandler();
  SMLoc Loc = Tok.getLocation(); StringRef Val;
  advance();
  if (DirectiveParser::parseExpr(_FollowSet, Val))
    return _errorhandler();
  if (expect(tok::kw_THEN))
    return _errorhandler();
  actOnIf(Loc, Val);
  advance();
  return false;
}
bool DirectiveParser::parseElsifpart(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[1] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[1]);
  };
  if (expect(tok::kw_ELSIF))
    return _errorhandler();
  SMLoc Loc = Tok.getLocation(); StringRef Val;
  actOnElsIf(Loc);
  advance();
  if (DirectiveParser::parseExpr(_FollowSet, Val))
    return _errorhandler();
  if (expect(tok::kw_THEN))
    return _errorhandler();
  actOnElsIf(Loc, Val);
  advance();
  return false;
}
bool DirectiveParser::parseElsepart(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[1] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[1]);
  };
  if (expect(tok::kw_ELSE))
    return _errorhandler();
  actOnElse(Tok.getLocation());
  advance();
  return false;
}
bool DirectiveParser::parseEndifpart(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[1] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[1]);
  };
  if (expect(tok::kw_END))
    return _errorhandler();
  actOnEnd(Tok.getLocation());
  advance();
  return false;
}
bool DirectiveParser::parseExpr(const _TokenBitSet &_FollowSetCallers, StringRef &Val) {
  const _TokenBitSet _FollowSet = _FollowSets[2] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[2]);
  };
  if (DirectiveParser::parseCondExpr(_FollowSet, Val))
    return _errorhandler();
  if ((_TokenBitSet{tok::equal, tok::hash, }).contains(Tok.getKind())) {
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
      return _errorhandler();
    }
    StringRef RightVal;
    if (DirectiveParser::parseCondExpr(_FollowSet, RightVal))
      return _errorhandler();
    Val = actOnRelation(Op, Val, RightVal);
  }
  return false;
}
bool DirectiveParser::parseCondExpr(const _TokenBitSet &_FollowSetCallers, StringRef &Val) {
  const _TokenBitSet _FollowSet = _FollowSets[3] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[3]);
  };
  if (DirectiveParser::parseCondTerm(_FollowSet, Val))
    return _errorhandler();
  while (Tok.is(tok::kw_OR)) {
    StringRef RightVal;
    advance();
    if (DirectiveParser::parseCondTerm(_FollowSet, Val))
      return _errorhandler();
    Val = actOnOr(Val, RightVal);
  }
  return false;
}
bool DirectiveParser::parseCondTerm(const _TokenBitSet &_FollowSetCallers, StringRef &Val) {
  const _TokenBitSet _FollowSet = _FollowSets[4] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[4]);
  };
  if (DirectiveParser::parseCondFactor(_FollowSet, Val))
    return _errorhandler();
  while (Tok.is(tok::kw_AND)) {
    StringRef RightVal;
    advance();
    if (DirectiveParser::parseCondFactor(_FollowSet, RightVal))
      return _errorhandler();
    Val = actOnAnd(Val, RightVal);
  }
  return false;
}
bool DirectiveParser::parseCondFactor(const _TokenBitSet &_FollowSetCallers, StringRef &Val) {
  const _TokenBitSet _FollowSet = _FollowSets[5] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[5]);
  };
  if (Tok.is(tok::kw_NOT)) {
    advance();
    if (DirectiveParser::parseCondFactor(_FollowSet, Val))
      return _errorhandler();
    actOnNot(Val);
  }
  else if ((_TokenBitSet{tok::identifier, tok::string_literal, }).contains(Tok.getKind())) {
    if (DirectiveParser::parseValue(_FollowSet, Val))
      return _errorhandler();
  }
  else if (Tok.is(tok::l_paren)) {
    advance();
    if (DirectiveParser::parseExpr(_FollowSet, Val))
      return _errorhandler();
    if (consume(tok::r_paren))
      return _errorhandler();
  }
  else {
    error();
    return _errorhandler();
  }
  return false;
}
bool DirectiveParser::parseValue(const _TokenBitSet &_FollowSetCallers, StringRef &Val) {
  const _TokenBitSet _FollowSet = _FollowSets[5] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[5]);
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
    return _errorhandler();
  }
  return false;
}
#endif
