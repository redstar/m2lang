//===--- Grammar.h - LALRtool grammar definition ----------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the grammar.
///
//===----------------------------------------------------------------------===//

#ifndef LALRTOOL_GRAMMAR_H
#define LALRTOOL_GRAMMAR_H

#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/SMLoc.h"

namespace lalrtool {

class Diagnostic;

using FirstSetType = llvm::BitVector;

/**
 * @brief Grammar definition
 *
 * A context-free grammar is a tuple (NT, T, P, S)...
 */

#define AST_DECLARATION
#include "lalrtool/lalrtool.ast.inc"

#if 0
class Symbol {
public:
  enum SymbolKind {
    SK_Terminal,
    SK_Nonterminal,
  };

private:
  const SymbolKind Kind;
  /*const*/ llvm::SMLoc Loc;
  llvm::StringRef Name;

protected:
  Symbol(SymbolKind Kind, llvm::SMLoc Loc, llvm::StringRef Name)
      : Kind(Kind), Loc(Loc), Name(Name) {}

public:
  llvm::SMLoc getLoc() const { return Loc; }
  const llvm::StringRef getName() const { return Name; }

  SymbolKind kind() const { return Kind; }
};

class Terminal : public Symbol {
  llvm::StringRef ExternalName; // Name given with %token

public:
  Terminal(llvm::SMLoc Loc, llvm::StringRef Name, llvm::StringRef ExternalName)
      : Symbol(SK_Terminal, Loc, Name), ExternalName(ExternalName) {}

  const llvm::StringRef getExternalName() const { return ExternalName; }

  static bool classof(const Symbol *S) { return S->kind() == SK_Terminal; }
};

class Nonterminal : public Symbol {
  FirstSetType FirstSet;

  bool IsReachable;
  bool DerivesEpsilon;
  bool IsProductive;

public:
  Nonterminal(llvm::SMLoc Loc, llvm::StringRef Name)
      : Symbol(SK_Terminal, Loc, Name), IsReachable(false),
        DerivesEpsilon(false), IsProductive(false) {}

  bool isReachable() const { return IsReachable; }
  void setReachable(bool V) { IsReachable = V; }
  bool derivesEpsilon() const { return DerivesEpsilon; }
  void setDerivesEpsilon(bool V) { DerivesEpsilon = V; }
  bool isProductive() const { return IsProductive; }
  void setProductive(bool V) { IsProductive = V; }

  static bool classof(const Symbol *S) { return S->kind() == SK_Nonterminal; }
};

class RuleElement {
public:
  enum RuleElementKind {
    RK_Terminal,
    RK_Nonterminal,
    RK_Predicate,
    RK_Action,
  };

private:
  const RuleElementKind Kind;
  const llvm::SMLoc Loc;

protected:
  RuleElement(RuleElementKind Kind, llvm::SMLoc Loc) : Kind(Kind), Loc(Loc) {}

public:
  llvm::SMLoc getLoc() const { return Loc; }

  RuleElementKind kind() const { return Kind; }
};

class TerminalRef : RuleElement {
  Terminal *T;

public:
  TerminalRef(llvm::SMLoc Loc, Terminal *T)
      : RuleElement(RK_Terminal, Loc), T(T) {}

  Terminal *getTerminal() const { return T; }
  static bool classof(const RuleElement *E) { return E->kind() == RK_Terminal; }
};

class NonterminalRef : RuleElement {
  Nonterminal *NT;

public:
  NonterminalRef(llvm::SMLoc Loc, Nonterminal *NT)
      : RuleElement(RK_Nonterminal, Loc), NT(NT) {}

  Nonterminal *getNonterminal() const { return NT; }

  static bool classof(const RuleElement *E) {
    return E->kind() == RK_Nonterminal;
  }
};

class Predicate : RuleElement {
  llvm::StringRef Code;

public:
  Predicate(llvm::SMLoc Loc, llvm::StringRef Code)
      : RuleElement(RK_Predicate, Loc), Code(Code) {}

  const llvm::StringRef getCode() const { return Code; }

  static bool classof(const RuleElement *E) {
    return E->kind() == RK_Predicate;
  }
};

class Action : RuleElement {
  llvm::StringRef Code;

public:
  Action(llvm::SMLoc Loc, llvm::StringRef Code)
      : RuleElement(RK_Action, Loc), Code(Code) {}

  const llvm::StringRef getCode() const { return Code; }

  static bool classof(const RuleElement *E) { return E->kind() == RK_Action; }
};

typedef llvm::SmallVector<RuleElement *, 0> RightHandSide;

class Rule {
  Nonterminal *NT;
  RightHandSide RHS;
  Rule *Next;

public:
  Rule(Nonterminal *NT, RightHandSide RHS, Rule *Next = nullptr)
      : NT(NT), RHS(RHS), Next(Next) {}

  RightHandSide &getRHS() { return RHS; }
  const RightHandSide &getRHS() const { return RHS; }

  Rule *getNext() const { return Next; }
  void setNext(Rule *R) { Next = R; }
};
#endif

using RightHandSide = llvm::SmallVector<RuleElement *, 0>;

/*
class LR0Item {
  Rule *R;
  size_t Dot;
}
*/

class Grammar {
  Nonterminal *StartSymbol;
  Nonterminal *SyntheticStartSymbol;
  Terminal *EoiTerminal;
  llvm::SmallVector<Symbol *, 0> Symbols;
  // llvm::IndexedMap<Terminal *> TerminalMap;

public:
  using range_type =
      llvm::iterator_range<llvm::SmallVector<Symbol *, 0>::iterator>;

  Grammar()
      : StartSymbol(nullptr), SyntheticStartSymbol(nullptr),
        EoiTerminal(nullptr), Symbols() /*, TerminalMap()*/ {}
  Grammar(Nonterminal *StartSymbol, Nonterminal *SyntheticStartSymbol,
          Terminal *EoiTerminal, llvm::SmallVector<Symbol *, 0> &Symbols /*,
          llvm::IndexedMap<Terminal *> &TerminalMap*/)
      : StartSymbol(StartSymbol), SyntheticStartSymbol(SyntheticStartSymbol),
        EoiTerminal(EoiTerminal), Symbols(Symbols) /*, TerminalMap(TerminalMap)*/ {}

  Nonterminal *startSymbol() const { return StartSymbol; }
  Nonterminal *syntheticStartSymbol() const { return SyntheticStartSymbol; }
  Terminal *eoiTerminal() const { return EoiTerminal; }
  const llvm::SmallVector<Symbol *, 0> &symbols() const { return Symbols; }
  llvm::iterator_range<llvm::SmallVector<Symbol *, 0>::iterator> nodeRange() {
    return llvm::make_range(Symbols.begin(), Symbols.end());
  }

  // Terminal *map(unsigned N) const { return TerminalMap[N]; }
  // unsigned numberOfTerminals() const {
  //   return static_cast<unsigned>(TerminalMap.size());
  // }

  void performAnalysis(Diagnostic &Diag);
};
} // namespace lalrtool
#endif
