//===--- LR0Automaton.h - LR(0) automaton used by LALRtool ------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the LR(0) automaton.
///
//===----------------------------------------------------------------------===//

#ifndef LALRTOOL_LR0AUTOMATON_H
#define LALRTOOL_LR0AUTOMATON_H

#include "lalrtool/Grammar.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/DirectedGraph.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/TypeSwitch.h"

namespace lalrtool {

class LR0Item {
  Rule *R;
  unsigned Dot;

public:
  LR0Item(Rule *R, unsigned Dot) : R(R), Dot(Dot) {}

  Rule *getRule() const { return R; }
  unsigned getDot() const { return Dot; }
  bool isReduce() { return Dot == R->getRHS().size(); }
  LR0Item moveDot() const {
    assert(!isReduce() && "Can't move dot of reduce item");
    return LR0Item(R, Dot + 1);
  }

  struct DenseMapInfo {
    using RuleInfo = llvm::DenseMapInfo<Rule *>;
    using DotInfo = llvm::DenseMapInfo<unsigned>;

    static inline LR0Item getEmptyKey() {
      return LR0Item(RuleInfo::getEmptyKey(), DotInfo::getEmptyKey());
    }

    static inline LR0Item getTombstoneKey() {
      return LR0Item(RuleInfo::getTombstoneKey(), DotInfo::getTombstoneKey());
    }

    static unsigned getHashValue(const LR0Item &Item) {
      return llvm::detail::combineHashValue(
          RuleInfo::getHashValue(Item.getRule()),
          DotInfo::getHashValue(Item.getDot()));
    }

    static bool isEqual(const LR0Item &LHS, const LR0Item &RHS) {
      return RuleInfo::isEqual(LHS.getRule(), RHS.getRule()) &&
             DotInfo::isEqual(LHS.getDot(), RHS.getDot());
    }
  };
};

class LR0State : public llvm::FoldingSetNode {
public:
  using KernelList = llvm::SmallVector<LR0Item, 4>;
  using ItemSet = llvm::DenseSet<LR0Item>;

private:
  KernelList Kernels;
  ItemSet Items;

public:
  LR0State(const KernelList &Kernels) : Kernels(Kernels) {
    Items.insert(Kernels.begin(), Kernels.end());
  }

  LR0State(const ItemSet &Items) : Items(Items) { orderKernel(Items, Kernels); }

  static void orderKernel(const ItemSet &Unsorted, KernelList &Sorted) {
    std::copy(Unsorted.begin(), Unsorted.end(), std::back_inserter(Sorted));
    llvm::sort(Sorted, [](const LR0Item &Item1, const LR0Item &Item2) {
      return Item1.getRule()->getID() < Item2.getRule()->getID();
    });
  }

  static void Profile(llvm::FoldingSetNodeID &ID, const KernelList &Kernels) {
    for (const LR0Item Kernel : Kernels) {
      ID.AddPointer(Kernel.getRule());
      ID.AddInteger(Kernel.getDot());
    }
  }

  void Profile(llvm::FoldingSetNodeID &ID) const { Profile(ID, Kernels); }

  const KernelList &kernels() const { return Kernels; }
  const ItemSet &items() const { return Items; }

  void add(LR0Item &&Item) { Items.insert(Item); }
};

class LR0Automaton {
  llvm::FoldingSet<LR0State> States;

public:
  std::pair<LR0State *, bool>
  getOrCreate(const llvm::DenseSet<LR0Item> &KernelItems) {
    LR0State::KernelList Kernels;
    LR0State::orderKernel(KernelItems, Kernels);
    llvm::FoldingSetNodeID ID;
    LR0State::Profile(ID, Kernels);

    void *InsertPoint;
    LR0State *State = States.FindNodeOrInsertPos(ID, InsertPoint);
    if (State)
      return std::pair<LR0State *, bool>(State, false);

    State = new LR0State(Kernels);
    States.InsertNode(State, InsertPoint);
    return std::pair<LR0State *, bool>(State, true);
  }

  void writeDot(llvm::raw_ostream &OS);
};

class LR0AutomatonBuilder {
  LR0Automaton *Automaton;

  LR0State *getStart(Nonterminal *Start) {
    Rule *StartRule = Start->getRule();
    LR0State::ItemSet Kernel;
    Kernel.insert(LR0Item(StartRule, 0));
    return Automaton->getOrCreate(Kernel).first;
  }

  LR0State::ItemSet next(LR0State *Q, Symbol *S) {
    LR0State::ItemSet Kernels;
    for (const LR0Item &Item : Q->items()) {
      auto &RHS = Item.getRule()->getRHS();
      if (Item.getDot() < RHS.size()) {
        RuleElement *RE = RHS[Item.getDot()];
        llvm::TypeSwitch(RE).Case<NonterminalRef *>([&S, &Kernels, &Item](
                                                        NonterminalRef *NTRef) {
          if (NTRef->getNonterminal() == S)
            Kernels.insert(Item.moveDot());
        });
      }
    }
  }

  void closure(LR0State &State) {
    for (const LR0Item &Kernel : State.kernels()) {
      // Nothing to do for a reduce item.
      if (Kernel.getRule()->getRHS().size() >= Kernel.getDot())
        continue;
      RuleElement *RE = Kernel.getRule()->getRHS()[Kernel.getDot()];
      if (NonterminalRef *NTRef = llvm::dyn_cast<NonterminalRef>(RE)) {
        Nonterminal *NT = NTRef->getNonterminal();
        for (Rule *R : rules(NT)) {
          State.add(LR0Item(R, 0));
        }
      }
      // TODO Handle predicate.
    }
  }

  llvm::DenseSet<Symbol *> getTransitionSymbols(LR0State *Q) {
    llvm::DenseSet<Symbol *> Ret;
    for (LR0Item &Item : Q->items()) {
      auto &RHS = Item.getRule()->getRHS();
      if (Item.getDot() < RHS.size()) {
        RuleElement *RE = RHS[Item.getDot()];
        if (NonterminalRef *NTRef = llvm::dyn_cast<NonterminalRef>(RE)) {
          Ret.insert(NTRef->getNonterminal());
        } else if (TerminalRef *TRef = llvm::dyn_cast<TerminalRef>(RE)) {
          Ret.insert(TRef->getTerminal());
        }
        // TODO Handle predicate.
      }
    }
    return Ret;
  }

public:
  LR0AutomatonBuilder() { Automaton = new LR0Automaton(); }

  void operator()(const Grammar &G) {
    llvm::SmallVector<LR0State *, 16> Work;
    Work.push_back(getStart(G.syntheticStartSymbol()));
    while (!Work.empty()) {
      LR0State *Q = Work.pop_back_val();
      for (Symbol *Sym : getTransitionSymbols(Q)) {
      }
    }
  }
};
} // namespace lalrtool

namespace llvm {

template <>
struct ::llvm::DenseMapInfo<lalrtool::LR0Item>
    : lalrtool::LR0Item::DenseMapInfo {};

} // end namespace llvm

#endif
