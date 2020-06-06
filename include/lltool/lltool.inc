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
bool parseLltool(const __TokenBitSet &__FollowSetCallers);
bool parseHeader(const __TokenBitSet &__FollowSetCallers);
bool parseTokenlist(const __TokenBitSet &__FollowSetCallers);
bool parseTokendecl(const __TokenBitSet &__FollowSetCallers);
bool parseRule(const __TokenBitSet &__FollowSetCallers);
bool parseNonterminal(const __TokenBitSet &__FollowSetCallers, Node *&node);
bool parseRhs(const __TokenBitSet &__FollowSetCallers, Node *&node);
bool parseSequence(const __TokenBitSet &__FollowSetCallers, Node *&node);
bool parseGroup(const __TokenBitSet &__FollowSetCallers, Node *&node);
#endif
#ifdef PARSER_DEFINITION
const Parser::__TokenBitSet Parser::__FollowSets[] = {
  { tok::eoi },
  { tok::identifier },
  { tok::percentpercent },
  { tok::percentpercent, tok::comma },
  { tok::identifier, tok::eoi },
  { tok::colon },
  { tok::semi, tok::r_paren, tok::r_parenquestion, tok::r_parenstar, tok::r_parenplus },
  { tok::semi, tok::pipe, tok::r_paren, tok::r_parenquestion, tok::r_parenstar, tok::r_parenplus },
  {  },
};
bool Parser::__skipUntil(const Parser::__TokenBitSet &ActiveSets, const Parser::__TokenBitSet &CurrentSet) {
  Parser::__TokenBitSet StopSets = ActiveSets | tok::eoi;
  while (!StopSets.contains(Tok.getKind())) {
    advance();
  }
  return CurrentSet.contains(Tok.getKind());
}
bool Parser::parseLltool(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[0] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[0]);
  };
  if ((__TokenBitSet{tok::kw_token, tok::kw_start, tok::kw_eoi, tok::kw_language, tok::kw_define, tok::percentpercent, }).contains(Tok.getKind())) {
    if (Parser::parseHeader(__FollowSet))
      return __errorhandler();
  }
  do {
    if (Parser::parseRule(__FollowSet))
      return __errorhandler();
  } while (Tok.is(tok::identifier));
  return false;
}
bool Parser::parseHeader(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[1] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[1]);
  };
  while ((__TokenBitSet{tok::kw_token, tok::kw_start, tok::kw_eoi, tok::kw_language, tok::kw_define, }).contains(Tok.getKind())) {
    if (Tok.is(tok::kw_start)) {
      advance();
      if (expect(tok::identifier))
        return __errorhandler();
      Builder.startSymbol(Tok.getLoc(), Tok.getData());
      advance();
    }
    else if (Tok.is(tok::kw_token)) {
      advance();
      if (Parser::parseTokenlist(__FollowSet))
        return __errorhandler();
    }
    else if (Tok.is(tok::kw_language)) {
      advance();
      if (expect(tok::string))
        return __errorhandler();
      Builder.language(Tok.getLoc(), Tok.getData());
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
        return __errorhandler();
      }
      if ((__TokenBitSet{tok::identifier, tok::qualidentifier, tok::code, tok::string, }).contains(Tok.getKind())) {
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
          return __errorhandler();
        }
      }
      Builder.define(Loc, ident, value, type);
    }
    else if (Tok.is(tok::kw_eoi)) {
      advance();
      if (expect(tok::identifier))
        return __errorhandler();
      Builder.eoiSymbol(Tok.getLoc(), Tok.getData());
      advance();
    }
  }
  if (consume(tok::percentpercent))
    return __errorhandler();
  return false;
}
bool Parser::parseTokenlist(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[2] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[2]);
  };
  if (Parser::parseTokendecl(__FollowSet))
    return __errorhandler();
  while (Tok.is(tok::comma)) {
    advance();
    if (Parser::parseTokendecl(__FollowSet))
      return __errorhandler();
  }
  return false;
}
bool Parser::parseTokendecl(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[3] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[3]);
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
    return __errorhandler();
  }
  if (Tok.is(tok::equal)) {
    advance();
    if (expect(tok::identifier))
      return __errorhandler();
    ext = Tok.getData();
    advance();
  }
  Builder.terminal(Loc, val, ext);
  return false;
}
bool Parser::parseRule(const __TokenBitSet &__FollowSetCallers) {
  const __TokenBitSet __FollowSet = __FollowSets[4] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[4]);
  };
  Node *node = nullptr;
  if (Parser::parseNonterminal(__FollowSet, node))
    return __errorhandler();
  if (consume(tok::colon))
    return __errorhandler();
  if (Parser::parseRhs(__FollowSet, node->Link))
    return __errorhandler();
  node->Link->Back = node;
  if (consume(tok::semi))
    return __errorhandler();
  return false;
}
bool Parser::parseNonterminal(const __TokenBitSet &__FollowSetCallers, Node *&node) {
  const __TokenBitSet __FollowSet = __FollowSets[5] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[5]);
  };
  if (expect(tok::identifier))
    return __errorhandler();
  node = Builder.nonterminal(Tok.getLoc(), Tok.getData());
  advance();
  if (Tok.is(tok::argument)) {
    Builder.argument(node, Tok.getData());
    advance();
  }
  if (Tok.is(tok::code)) {
    advance();
  }
  return false;
}
bool Parser::parseRhs(const __TokenBitSet &__FollowSetCallers, Node *&node) {
  const __TokenBitSet __FollowSet = __FollowSets[6] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[6]);
  };
  if (Parser::parseSequence(__FollowSet, node))
    return __errorhandler();
  if (Tok.is(tok::pipe)) {
    node = Builder.alternative(node->Loc, node);
                                           Node *alt = node->Link; alt->Back = node;
    do {
      advance();
      if (Parser::parseSequence(__FollowSet, alt->Link))
        return __errorhandler();
      alt = alt->Link; alt->Back = node;
    } while (Tok.is(tok::pipe));
  }
  return false;
}
bool Parser::parseSequence(const __TokenBitSet &__FollowSetCallers, Node *&node) {
  const __TokenBitSet __FollowSet = __FollowSets[7] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[7]);
  };
  Node *last = nullptr; node = Builder.sequence(Tok.getLoc());
  while ((__TokenBitSet{tok::identifier, tok::code, tok::string, tok::kw_if, tok::l_paren, }).contains(Tok.getKind())) {
    Node *n = nullptr;
    if (Tok.is(tok::l_paren)) {
      if (Parser::parseGroup(__FollowSet, n))
        return __errorhandler();
    }
    else if (Tok.is(tok::identifier)) {
      n = Builder.symbol(Tok.getLoc(), Tok.getData());
      advance();
      if (Tok.is(tok::argument)) {
        Builder.argument(n, Tok.getData());
        advance();
      }
    }
    else if (Tok.is(tok::string)) {
      n = Builder.symbol(Tok.getLoc(), Tok.getData(), true);
      advance();
    }
    else if (Tok.is(tok::code)) {
      n = Builder.code(Tok.getLoc(), Tok.getData());
      advance();
    }
    else if (Tok.is(tok::kw_if)) {
      advance();
      if (expect(tok::code))
        return __errorhandler();
      n = Builder.code(Tok.getLoc(), Tok.getData());
                                          cast<Code>(n)->Type = Code::Condition;
      advance();
    }
    else {
      error();
      return __errorhandler();
    }
    if (!last) node->Inner = last = n;
                                           else last->Next = n, last = n;
  }
  if (last) last->Back = node;
  return false;
}
bool Parser::parseGroup(const __TokenBitSet &__FollowSetCallers, Node *&node) {
  const __TokenBitSet __FollowSet = __FollowSets[8] | __FollowSetCallers;
  auto __errorhandler = [this, __FollowSet] {
    return __skipUntil(__FollowSet, __FollowSets[8]);
  };
  if (expect(tok::l_paren))
    return __errorhandler();
  node = Builder.group(Tok.getLoc(), Group::One);
  advance();
  if (Parser::parseRhs(__FollowSet, node->Link))
    return __errorhandler();
  node->Link->Back = node;
  if (Tok.is(tok::r_paren)) {
    advance();
  }
  else if (Tok.is(tok::r_parenquestion)) {
    cast<Group>(node)->Cardinality = Group::ZeroOrOne;
    advance();
  }
  else if (Tok.is(tok::r_parenstar)) {
    cast<Group>(node)->Cardinality = Group::ZeroOrMore;
    advance();
  }
  else if (Tok.is(tok::r_parenplus)) {
    cast<Group>(node)->Cardinality = Group::OneOrMore;
    advance();
  }
  else {
    error();
    return __errorhandler();
  }
  return false;
}
#endif
