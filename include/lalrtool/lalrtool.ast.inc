#ifdef AST_DECLARATION
#undef AST_DECLARATION
class Rule;

class Symbol {
  friend class Terminal;
  friend class Nonterminal;
public:
  enum class __KindType : unsigned {
    K_Terminal,
    K_Nonterminal,
    Last = K_Nonterminal
  };
protected:
  const __KindType __Kind;
private:
  llvm::SMLoc _Loc;
  llvm::StringRef _Name;
  unsigned _ID;

protected:
  Symbol(__KindType __Kind, llvm::SMLoc _Loc, llvm::StringRef _Name, unsigned _ID)
    : __Kind(__Kind), _Loc(_Loc), _Name(_Name), _ID(_ID) {}
public:

  llvm::SMLoc getLoc() const {
    return _Loc;
  }

  llvm::StringRef getName() const {
    return _Name;
  }

  unsigned getID() const {
    return _ID;
  }

  __KindType kind() const { return __Kind; }
};

class Terminal : public Symbol {
public:
  enum ConflictResolution {
    Unspecified, Shift, Reduce
  };
private:
  llvm::StringRef _ExternalName;
  ConflictResolution _Resolution = Unspecified;

public:
  Terminal(llvm::SMLoc _Loc, llvm::StringRef _Name, unsigned _ID, llvm::StringRef _ExternalName)
    : Symbol(__KindType::K_Terminal, _Loc, _Name, _ID), _ExternalName(_ExternalName) {}

  llvm::StringRef getExternalName() const {
    return _ExternalName;
  }

  ConflictResolution getResolution() const {
    return _Resolution;
  }

  void setResolution(ConflictResolution _Resolution) {
    this->_Resolution = _Resolution;
  }

  static bool classof(const Symbol* T) {
    return T->__Kind == __KindType::K_Terminal;
  }
};

class Nonterminal : public Symbol {
  Rule * _Rule = nullptr;
  FirstSetType _FirstSet;
  FollowSetType _FollowSet;
  bool _Reachable = false;
  bool _DerivesEpsilon = false;
  bool _Productive = false;

public:
  Nonterminal(llvm::SMLoc _Loc, llvm::StringRef _Name, unsigned _ID)
    : Symbol(__KindType::K_Nonterminal, _Loc, _Name, _ID) {}

  Rule * getRule() const {
    return _Rule;
  }

  void setRule(Rule * _Rule) {
    this->_Rule = _Rule;
  }

  FirstSetType getFirstSet() const {
    return _FirstSet;
  }

  void setFirstSet(FirstSetType _FirstSet) {
    this->_FirstSet = _FirstSet;
  }

  FollowSetType getFollowSet() const {
    return _FollowSet;
  }

  void setFollowSet(FollowSetType _FollowSet) {
    this->_FollowSet = _FollowSet;
  }

  bool isReachable() const {
    return _Reachable;
  }

  void setReachable(bool _Reachable) {
    this->_Reachable = _Reachable;
  }

  bool isDerivesEpsilon() const {
    return _DerivesEpsilon;
  }

  void setDerivesEpsilon(bool _DerivesEpsilon) {
    this->_DerivesEpsilon = _DerivesEpsilon;
  }

  bool isProductive() const {
    return _Productive;
  }

  void setProductive(bool _Productive) {
    this->_Productive = _Productive;
  }

  static bool classof(const Symbol* T) {
    return T->__Kind == __KindType::K_Nonterminal;
  }
};

class RuleElement {
  friend class TerminalRef;
  friend class NonterminalRef;
  friend class Predicate;
  friend class Action;
public:
  enum class __KindType : unsigned {
    K_TerminalRef,
    K_NonterminalRef,
    K_Predicate,
    K_Action,
    Last = K_Action
  };
protected:
  const __KindType __Kind;
private:
  llvm::SMLoc _Loc;

protected:
  RuleElement(__KindType __Kind, llvm::SMLoc _Loc)
    : __Kind(__Kind), _Loc(_Loc) {}
public:

  llvm::SMLoc getLoc() const {
    return _Loc;
  }

  __KindType kind() const { return __Kind; }
};

class TerminalRef : public RuleElement {
  Terminal * _Terminal;

public:
  TerminalRef(llvm::SMLoc _Loc, Terminal * _Terminal)
    : RuleElement(__KindType::K_TerminalRef, _Loc), _Terminal(_Terminal) {}

  Terminal * getTerminal() const {
    return _Terminal;
  }

  static bool classof(const RuleElement* T) {
    return T->__Kind == __KindType::K_TerminalRef;
  }
};

class NonterminalRef : public RuleElement {
  Nonterminal * _Nonterminal;

public:
  NonterminalRef(llvm::SMLoc _Loc, Nonterminal * _Nonterminal)
    : RuleElement(__KindType::K_NonterminalRef, _Loc), _Nonterminal(_Nonterminal) {}

  Nonterminal * getNonterminal() const {
    return _Nonterminal;
  }

  static bool classof(const RuleElement* T) {
    return T->__Kind == __KindType::K_NonterminalRef;
  }
};

class Predicate : public RuleElement {
  llvm::StringRef _Code;

public:
  Predicate(llvm::SMLoc _Loc, llvm::StringRef _Code)
    : RuleElement(__KindType::K_Predicate, _Loc), _Code(_Code) {}

  llvm::StringRef getCode() const {
    return _Code;
  }

  static bool classof(const RuleElement* T) {
    return T->__Kind == __KindType::K_Predicate;
  }
};

class Action : public RuleElement {
  llvm::StringRef _Code;

public:
  Action(llvm::SMLoc _Loc, llvm::StringRef _Code)
    : RuleElement(__KindType::K_Action, _Loc), _Code(_Code) {}

  llvm::StringRef getCode() const {
    return _Code;
  }

  static bool classof(const RuleElement* T) {
    return T->__Kind == __KindType::K_Action;
  }
};

class Rule {
  Nonterminal * _Nonterminal;
  unsigned _ID;
  llvm::SmallVector<RuleElement *, 4> _RHS;
  Rule * _Next = nullptr;

public:
  Rule(Nonterminal * _Nonterminal, unsigned _ID)
    : _Nonterminal(_Nonterminal), _ID(_ID) {}

  Nonterminal * getNonterminal() const {
    return _Nonterminal;
  }

  unsigned getID() const {
    return _ID;
  }

  llvm::SmallVector<RuleElement *, 4> &getRHS() {
    return _RHS;
  }

  void setRHS(const llvm::SmallVector<RuleElement *, 4> &_RHS) {
    this->_RHS = _RHS;
  }

  Rule * getNext() const {
    return _Next;
  }

  void setNext(Rule * _Next) {
    this->_Next = _Next;
  }
};
#ifdef AST_DISPATCHER
#undef AST_DISPATCHER
namespace dispatcher {
template <typename T, typename R = void>
class SymbolDispatcher {
  typedef R (T::*Func)(Symbol *);
  Func Table[static_cast<unsigned>(Symbol::__KindType::Last)+1];

public:
  SymbolDispatcher(
    R (T::*TerminalFn)(Terminal *),
    R (T::*NonterminalFn)(Nonterminal *)) {
    Table[static_cast<unsigned>(Symbol::__KindType::K_Terminal)] = reinterpret_cast<Func>(TerminalFn);
    Table[static_cast<unsigned>(Symbol::__KindType::K_Nonterminal)] = reinterpret_cast<Func>(NonterminalFn);
  }
  template<class U = R>
  std::enable_if_t<std::is_void_v<U>, U> operator()(T *Obj, Symbol *Arg) const {
    (Obj->*Table[static_cast<unsigned>(Arg->kind())])(Arg);
  }
  template<class U = R>
  std::enable_if_t<!std::is_void_v<U>, U> operator()(T *Obj, Symbol *Arg) const {
    return (Obj->*Table[static_cast<unsigned>(Arg->kind())])(Arg);
  }
};

template <typename T, typename R = void>
class RuleElementDispatcher {
  typedef R (T::*Func)(RuleElement *);
  Func Table[static_cast<unsigned>(RuleElement::__KindType::Last)+1];

public:
  RuleElementDispatcher(
    R (T::*TerminalRefFn)(TerminalRef *),
    R (T::*NonterminalRefFn)(NonterminalRef *),
    R (T::*PredicateFn)(Predicate *),
    R (T::*ActionFn)(Action *)) {
    Table[static_cast<unsigned>(RuleElement::__KindType::K_TerminalRef)] = reinterpret_cast<Func>(TerminalRefFn);
    Table[static_cast<unsigned>(RuleElement::__KindType::K_NonterminalRef)] = reinterpret_cast<Func>(NonterminalRefFn);
    Table[static_cast<unsigned>(RuleElement::__KindType::K_Predicate)] = reinterpret_cast<Func>(PredicateFn);
    Table[static_cast<unsigned>(RuleElement::__KindType::K_Action)] = reinterpret_cast<Func>(ActionFn);
  }
  template<class U = R>
  std::enable_if_t<std::is_void_v<U>, U> operator()(T *Obj, RuleElement *Arg) const {
    (Obj->*Table[static_cast<unsigned>(Arg->kind())])(Arg);
  }
  template<class U = R>
  std::enable_if_t<!std::is_void_v<U>, U> operator()(T *Obj, RuleElement *Arg) const {
    return (Obj->*Table[static_cast<unsigned>(Arg->kind())])(Arg);
  }
};

}; // namespace dispatcher
#endif
#endif
#ifdef AST_DEFINITION
#undef AST_DEFINITION
#endif
