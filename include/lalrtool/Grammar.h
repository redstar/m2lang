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
#include "llvm/ADT/IndexedMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"

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
  llvm::SmallVector<Nonterminal *, 0> Nonterminals;
  llvm::IndexedMap<Terminal *> TerminalMap;
  size_t NumberOfRules;

public:
  using range_type =
      llvm::iterator_range<llvm::SmallVector<Symbol *, 0>::iterator>;

  Grammar()
      : StartSymbol(nullptr), SyntheticStartSymbol(nullptr),
        EoiTerminal(nullptr), Symbols(), TerminalMap(), NumberOfRules(0) {}
  Grammar(Nonterminal *StartSymbol, Nonterminal *SyntheticStartSymbol,
          Terminal *EoiTerminal, llvm::SmallVector<Symbol *, 0> &Symbols,
          size_t NumberOfRules)
      : StartSymbol(StartSymbol), SyntheticStartSymbol(SyntheticStartSymbol),
        EoiTerminal(EoiTerminal), Symbols(Symbols),
        NumberOfRules(NumberOfRules) {
    for (auto Sym : Symbols) {
      if (auto *T = llvm::dyn_cast<Terminal>(Sym)) {
        TerminalMap.resize(T->getID() + 1);
        TerminalMap[T->getID()] = T;
      }
      if (auto *NT = llvm::dyn_cast<Nonterminal>(Sym))
        Nonterminals.push_back(NT);
    }
  }

  Nonterminal *startSymbol() const { return StartSymbol; }
  Nonterminal *syntheticStartSymbol() const { return SyntheticStartSymbol; }
  Terminal *eoiTerminal() const { return EoiTerminal; }
  size_t getNumberOfRules() const {return NumberOfRules; }
  // const llvm::SmallVector<Symbol *, 0> &symbols() const { return Symbols; }
  // llvm::iterator_range<llvm::SmallVector<Symbol *, 0>::iterator> nodeRange()
  // {
  //   return llvm::make_range(Symbols.begin(), Symbols.end());
  // }

  const llvm::SmallVector<Nonterminal *, 0> &nonterminals() const {
    return Nonterminals;
  }
  llvm::iterator_range<llvm::SmallVector<Nonterminal *, 0>::iterator>
  nonterminalsRange() {
    return llvm::make_range(Nonterminals.begin(), Nonterminals.end());
  }
  template <typename Callable> void forAllRules(Callable &&C) {
    for (auto *NT : Nonterminals) {
      for (Rule *R = NT->getRule(); R; R = R->getNext()) {
        C(R);
      }
    }
  }

  FirstSetType createEmptyFirstSet() const {
    return llvm::BitVector(TerminalMap.size());
  }
  // Terminal *map(unsigned N) const { return TerminalMap[N]; }
  // unsigned numberOfTerminals() const {
  //   return static_cast<unsigned>(TerminalMap.size());
  // }

  void performAnalysis(Diagnostic &Diag);

  void writeYAML(llvm::raw_ostream &OS) const;
};
} // namespace lalrtool
#endif
