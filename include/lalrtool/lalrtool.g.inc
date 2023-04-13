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
using _TokenBitSet = BitSet<tok::NUM_TOKENS>;
static const _TokenBitSet _FollowSets[];bool _skipUntil(const _TokenBitSet &ActiveSets, const _TokenBitSet &CurrentSet);
bool parseLalrtool(const _TokenBitSet &_FollowSetCallers);
bool parseHeader(const _TokenBitSet &_FollowSetCallers);
bool parseTokenlist(const _TokenBitSet &_FollowSetCallers);
bool parseTokendecl(const _TokenBitSet &_FollowSetCallers);
bool parseRule(const _TokenBitSet &_FollowSetCallers);
bool parseNonterminal(const _TokenBitSet &_FollowSetCallers, Nonterminal *&NT);
bool parseRhs(const _TokenBitSet &_FollowSetCallers, Nonterminal *NT);
bool parseSequence(const _TokenBitSet &_FollowSetCallers, Rule *R);
#endif
#ifdef PARSER_DEFINITION
const Parser::_TokenBitSet Parser::_FollowSets[] = {
  { tok::eoi },
  { tok::identifier },
  { tok::percentpercent },
  { tok::percentpercent, tok::comma },
  { tok::identifier, tok::eoi },
  { tok::colon },
  { tok::semi },
  { tok::semi, tok::pipe },
};
bool Parser::_skipUntil(const Parser::_TokenBitSet &ActiveSets, const Parser::_TokenBitSet &CurrentSet) {
  Parser::_TokenBitSet StopSets = ActiveSets | tok::eoi;
  while (!StopSets.contains(Tok.getKind())) {
    advance();
  }
  return CurrentSet.contains(Tok.getKind());
}
bool Parser::parseLalrtool(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[0] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[0]);
  };
  if ((_TokenBitSet{tok::kw_token, tok::kw_start, tok::kw_eoi, tok::kw_language, tok::kw_define, tok::kw_left, tok::kw_right, tok::percentpercent, }).contains(Tok.getKind())) {
    if (Parser::parseHeader(_FollowSet))
      return _errorhandler();
  }
  do {
    if (Parser::parseRule(_FollowSet))
      return _errorhandler();
  } while (Tok.is(tok::identifier));
  return false;
}
bool Parser::parseHeader(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[1] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[1]);
  };
  while ((_TokenBitSet{tok::kw_token, tok::kw_start, tok::kw_eoi, tok::kw_language, tok::kw_define, tok::kw_left, tok::kw_right, }).contains(Tok.getKind())) {
    if (Tok.is(tok::kw_start)) {
      advance();
      if (expect(tok::identifier))
        return _errorhandler();
      Builder.actOnStartSymbol(Tok.getLoc(), Tok.getData());
      advance();
    }
    else if (Tok.is(tok::kw_token)) {
      advance();
      if (Parser::parseTokenlist(_FollowSet))
        return _errorhandler();
    }
    else if (Tok.is(tok::kw_language)) {
      advance();
      if (expect(tok::string))
        return _errorhandler();
      Builder.actOnLanguage(Tok.getLoc(), Tok.getData());
      advance();
    }
    else if (Tok.is(tok::kw_define)) {
      SMLoc Loc; StringRef ident, value; var::VarType type = var::Flag;
      advance();
      Loc = Tok.getLoc(); ident = Tok.getData();
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
        value = Tok.getData();
        if (Tok.is(tok::code)) {
          type = var::Code;
          advance();
        }
        else if (Tok.is(tok::identifier)) {
          type = var::Identifier;
          advance();
        }
        else if (Tok.is(tok::qualidentifier)) {
          type = var::Identifier;
          advance();
        }
        else if (Tok.is(tok::string)) {
          type = var::String;
          advance();
        }
        else {
          error();
          return _errorhandler();
        }
      }
      Builder.actOnDefine(Loc, ident, value, type);
    }
    else if ((_TokenBitSet{tok::kw_left, tok::kw_right, }).contains(Tok.getKind())) {
      if (Tok.is(tok::kw_left)) {
        advance();
      }
      else if (Tok.is(tok::kw_right)) {
        advance();
      }
      else {
        error();
        return _errorhandler();
      }
      do {
        if (Tok.is(tok::identifier)) {
          advance();
        }
        else if (Tok.is(tok::string)) {
          advance();
        }
        else {
          error();
          return _errorhandler();
        }
      } while ((_TokenBitSet{tok::identifier, tok::string, }).contains(Tok.getKind()));
    }
    else if (Tok.is(tok::kw_eoi)) {
      advance();
      if (expect(tok::identifier))
        return _errorhandler();
      Builder.actOnEoiSymbol(Tok.getLoc(), Tok.getData());
      advance();
    }
  }
  if (consume(tok::percentpercent))
    return _errorhandler();
  return false;
}
bool Parser::parseTokenlist(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[2] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[2]);
  };
  if (Parser::parseTokendecl(_FollowSet))
    return _errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (Parser::parseTokendecl(_FollowSet))
      return _errorhandler();
  }
  return false;
}
bool Parser::parseTokendecl(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[3] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[3]);
  };
  SMLoc Loc; StringRef val, ext;
  if (Tok.is(tok::identifier)) {
    Loc = Tok.getLoc(); val = Tok.getData();
    advance();
  }
  else if (Tok.is(tok::string)) {
    Loc = Tok.getLoc(); val = Tok.getData();
    advance();
  }
  else {
    error();
    return _errorhandler();
  }
  if (Tok.is(tok::equal)) {
    advance();
    if (expect(tok::identifier))
      return _errorhandler();
    ext = Tok.getData();
    advance();
  }
  Builder.actOnTerminal(Loc, val, ext);
  return false;
}
bool Parser::parseRule(const _TokenBitSet &_FollowSetCallers) {
  const _TokenBitSet _FollowSet = _FollowSets[4] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[4]);
  };
  Nonterminal *NT = nullptr;
  if (Parser::parseNonterminal(_FollowSet, NT))
    return _errorhandler();
  if (consume(tok::colon))
    return _errorhandler();
  if (Parser::parseRhs(_FollowSet, NT))
    return _errorhandler();
  if (consume(tok::semi))
    return _errorhandler();
  return false;
}
bool Parser::parseNonterminal(const _TokenBitSet &_FollowSetCallers, Nonterminal *&NT) {
  const _TokenBitSet _FollowSet = _FollowSets[5] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[5]);
  };
  if (expect(tok::identifier))
    return _errorhandler();
  NT = Builder.actOnNonterminal(Tok.getLoc(), Tok.getData());
  advance();
  if (Tok.is(tok::argument)) {
    NT->setType(StringLoc(Tok.getLoc(), Tok.getData()));
    advance();
  }
  if (Tok.is(tok::code)) {
    advance();
  }
  return false;
}
bool Parser::parseRhs(const _TokenBitSet &_FollowSetCallers, Nonterminal *NT) {
  const _TokenBitSet _FollowSet = _FollowSets[6] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[6]);
  };
  Rule *R = Builder.actOnRule(NT);
  if (Parser::parseSequence(_FollowSet, R))
    return _errorhandler();
  while (Tok.is(tok::pipe)) {
    R = Builder.actOnRule(NT, R);
    advance();
    if ((_TokenBitSet{tok::identifier, tok::code, tok::string, tok::kw_if, tok::semi, tok::pipe, }).contains(Tok.getKind())) {
      if (Parser::parseSequence(_FollowSet, R))
        return _errorhandler();
    }
    else if (Tok.is(tok::kw_empty)) {
      advance();
    }
  }
  return false;
}
bool Parser::parseSequence(const _TokenBitSet &_FollowSetCallers, Rule *R) {
  const _TokenBitSet _FollowSet = _FollowSets[7] | _FollowSetCallers;
  auto _errorhandler = [this, _FollowSet] {
    return _skipUntil(_FollowSet, _FollowSets[7]);
  };
  while ((_TokenBitSet{tok::identifier, tok::string, tok::kw_if, }).contains(Tok.getKind())) {
    if (Tok.is(tok::identifier)) {
      Builder.actOnSymbolRef(R, Tok.getLoc(), Tok.getData());
      advance();
      if (Tok.is(tok::argument)) {
        advance();
      }
    }
    else if (Tok.is(tok::string)) {
      Builder.actOnSymbolRef(R, Tok.getLoc(), Tok.getData(), true);
      advance();
    }
    else if (Tok.is(tok::kw_if)) {
      advance();
      if (expect(tok::code))
        return _errorhandler();
      Builder.actOnPredicate(R, Tok.getLoc(), Tok.getData());
      advance();
    }
  }
  if (Tok.is(tok::code)) {
    R->setAction(StringLoc(Tok.getLoc(), Tok.getData()));
    advance();
  }
  return false;
}
#endif
