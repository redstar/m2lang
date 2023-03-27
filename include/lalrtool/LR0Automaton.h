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

/**
 * @brief An LR(0) item.
 *
 * An LR(0) item describes how much much of a rule has been recognozed.
 */
class LR0Item {
  Rule *R;
  unsigned Dot;

public:
  LR0Item(Rule *R, unsigned Dot) : R(R), Dot(Dot) {}

  Rule *getRule() const { return R; }
  unsigned getDot() const { return Dot; }
  bool isReduceItem() const { return Dot == R->getRHS().size(); }
  bool isShiftItem() const { return Dot < R->getRHS().size(); }
  LR0Item moveDot() const {
    assert(!isReduceItem() && "Can't move dot of reduce item");
    return LR0Item(R, Dot + 1);
  }

  // Support for DenseMap.
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
  llvm::DenseMap<LR0State *, llvm::DenseMap<Symbol *, LR0State *>> Transitions;

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

  void addTransition(LR0State *Qold, Symbol *Sym, LR0State *Qnew) {
    Transitions[Qold][Sym] = Qnew;
  }

  void writeDot(llvm::raw_ostream &OS);
};

/**
 * @brief Constructs the LR0 automaton.
 *
 * Follows the algorithm described in:
 * - Wilhelm/Maurer, p. 367
 * - Grune/Jacobs, p. 289
 */
class LR0AutomatonBuilder {
  std::unique_ptr<LR0Automaton> Automaton;

  LR0State *getStart(Nonterminal *Start) {
    Rule *StartRule = Start->getRule();
    LR0State::ItemSet Kernel;
    Kernel.insert(LR0Item(StartRule, 0));
    return Automaton->getOrCreate(Kernel).first;
  }

  std::pair<LR0State *, bool> nextState(LR0State *Q, Symbol *S) {
    LR0State::ItemSet Kernels;
    for (const LR0Item &Item : Q->items()) {
      auto &RHS = Item.getRule()->getRHS();
      if (Item.isShiftItem()) {
        RuleElement *RE = RHS[Item.getDot()];
        llvm::TypeSwitch<RuleElement *>(RE)
            .Case<NonterminalRef>([&S, &Kernels, &Item](NonterminalRef *NTRef) {
              if (NTRef->getNonterminal() == S)
                Kernels.insert(Item.moveDot());
            })
            .Case<TerminalRef>([&S, &Kernels, &Item](TerminalRef *TRef) {
              if (TRef->getTerminal() == S)
                Kernels.insert(Item.moveDot());
            })
            .Default([](RuleElement *) {
              // TODO Handle predicate.
            });
      }
    }
    std::pair<LR0State *, bool> Pair = Automaton->getOrCreate(Kernels);
    if (Pair.second)
      closure(Pair.first);
    return Pair;
  }

  void closure(LR0State *State) {
    for (const LR0Item &Kernel : State->kernels()) {
      // Nothing to do for a reduce item.
      if (Kernel.isReduceItem())
        continue;
      RuleElement *RE = Kernel.getRule()->getRHS()[Kernel.getDot()];
      if (NonterminalRef *NTRef = llvm::dyn_cast<NonterminalRef>(RE)) {
        Nonterminal *NT = NTRef->getNonterminal();
        for (Rule *R : rules(NT)) {
          State->add(LR0Item(R, 0));
        }
      }
      // TODO Handle predicate.
    }
  }

  llvm::DenseSet<Symbol *> getTransitionSymbols(LR0State *Q) {
    llvm::DenseSet<Symbol *> Ret;
    for (const LR0Item &Item : Q->items()) {
      auto &RHS = Item.getRule()->getRHS();
      if (Item.isShiftItem()) {
        RuleElement *RE = RHS[Item.getDot()];
        llvm::TypeSwitch<RuleElement *>(RE)
            .Case<NonterminalRef>([&Ret](NonterminalRef *NTRef) {
              Ret.insert(NTRef->getNonterminal());
            })
            .Case<TerminalRef>(
                [&Ret](TerminalRef *TRef) { Ret.insert(TRef->getTerminal()); })
            .Default([](RuleElement *) {
              // TODO Handle predicate.
            });
      }
    }
    return Ret;
  }

public:
  LR0AutomatonBuilder() { Automaton.reset(new LR0Automaton()); }

  std::unique_ptr<LR0Automaton> operator()(const Grammar &G) {
    llvm::SmallVector<LR0State *, 16> Work;
    Work.push_back(getStart(G.syntheticStartSymbol()));
    while (!Work.empty()) {
      LR0State *Q = Work.pop_back_val();
      for (Symbol *Sym : getTransitionSymbols(Q)) {
        LR0State *Qnew;
        bool Inserted;
        std::tie(Qnew, Inserted) = nextState(Q, Sym);
        if (Inserted)
          Work.push_back(Qnew);
        Automaton->addTransition(Q, Sym, Qnew);
      }
    }
    return std::move(Automaton);
  }
};
} // namespace lalrtool

namespace llvm {

template <>
struct ::llvm::DenseMapInfo<lalrtool::LR0Item>
    : lalrtool::LR0Item::DenseMapInfo {};

} // end namespace llvm

#endif
