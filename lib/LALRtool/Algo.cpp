//===--- Algo.cpp - LLtool algorithms definition ----------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the graph algorithms.
///
//===----------------------------------------------------------------------===//

#include "lalrtool/Algo.h"
#include "lalrtool/Grammar.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include <map>
#include <set>
#include <vector>

using namespace lalrtool;

/**
 * Marks all symbols reachable from the start symbol.
 *
 * Params:
 * 		grammar = grammar for which the reachability of the symbols is
 *                computed
 */
void lalrtool::calculateReachable(Grammar &G) {
  llvm::SmallVector<Nonterminal *, 16> Work;
  Work.push_back(G.syntheticStartSymbol());
  while (!Work.empty()) {
    Nonterminal *NT = Work.pop_back_val();
    if (!NT->isReachable()) {
      NT->setReachable(true);
      for (Rule *R : rules(NT)) {
        for (RuleElement *RE : R->getRHS()) {
          if (auto *NTRef = llvm::dyn_cast<NonterminalRef>(RE)) {
            if (!NTRef->getNonterminal()->isReachable())
              Work.push_back(NTRef->getNonterminal());
          }
        }
      }
    }
  }
}

/**
 * Calculates the epsilon productivity for each element of the grammar.
 *
 * Params:
 * 		grammar = grammar for which the epsilon productivity of the
 * symbols is computed
 */
void lalrtool::calculateDerivesEpsilon(Grammar &G) {
  unsigned NumberOfRules = G.getNumberOfRules();
  llvm::DenseMap<Nonterminal *, llvm::DenseSet<Rule *>> Occ;

  llvm::SmallVector<unsigned, 0> Count(NumberOfRules);
  llvm::SmallVector<Nonterminal *, 16> WorkList;

  // Initialize counter and work list.
  G.forAllRules([&Count, &Occ, &WorkList](Rule *R) {
    unsigned ID = R->getID();
    for (RuleElement *RE : R->getRHS())
      if (llvm::isa<TerminalRef>(RE))
        return;
    for (RuleElement *RE : R->getRHS()) {
      if (auto *NTRef = llvm::dyn_cast<NonterminalRef>(RE)) {
        ++Count[ID];
        Occ[NTRef->getNonterminal()].insert(R);
      }
    }
    if (Count[ID] == 0)
      WorkList.push_back(R->getNT());
  });

  while (!WorkList.empty()) {
    Nonterminal *NT = WorkList.pop_back_val();
    if (!NT->isDerivesEpsilon()) {
      NT->setDerivesEpsilon(true);
      for (Rule *R : Occ[NT]) {
        --Count[R->getID()];
        if (Count[R->getID()] == 0)
          WorkList.push_back(R->getNT());
      }
    }
  }
}

/**
 * Calculates the productivity of each symbol of the grammar.
 *
 * Params:
 * 		grammar = grammar for which the productivity of the symbols is
 *                computed
 */
void lalrtool::calculateProductive(Grammar &G) {
  unsigned NumberOfRules = G.getNumberOfRules();
  llvm::DenseMap<Nonterminal *, llvm::DenseSet<Rule *>> Occ;

  llvm::SmallVector<unsigned, 0> Count(NumberOfRules);
  llvm::SmallVector<Nonterminal *, 16> WorkList;

  // Initialize counter and work list.
  for (Nonterminal *NT : G.nonterminals()) {
    bool IsProductive = false;
    for (Rule *R : rules(NT)) {
      unsigned ID = R->getID();
      for (RuleElement *RE : R->getRHS()) {
        if (auto *NTRef = llvm::dyn_cast<NonterminalRef>(RE)) {
          ++Count[ID];
          Occ[NTRef->getNonterminal()].insert(R);
        }
      }
      if (Count[ID] == 0)
        IsProductive = true;
    }
    if (IsProductive)
      WorkList.push_back(NT);
  }

  while (!WorkList.empty()) {
    Nonterminal *NT = WorkList.pop_back_val();
    if (!NT->isProductive()) {
      NT->setProductive(true);
      for (Rule *R : Occ[NT]) {
        --Count[R->getID()];
        if (Count[R->getID()] == 0)
          WorkList.push_back(R->getNT());
      }
    }
  }
}

namespace {

/// Grammar Flow Analysis algorithm.
///
/// See Wilhelm, Maurer. p. 309
/// This is Tarjan's strongly connected components algorithm
template <typename T, typename GetterLambda, typename SetterLambda,
          typename AdderLambda, typename StartValueLambda,
          typename RelationLambda, typename RangeType>
struct ComputeSetValuedFunc {

  ComputeSetValuedFunc(GetterLambda Getter, SetterLambda Setter,
                       AdderLambda Adder, StartValueLambda StartValue,
                       RelationLambda Relation)
      : Getter(Getter), Setter(Setter), Adder(Adder), StartValue(StartValue),
        Relation(Relation) {}

  void operator()(RangeType R) {
    for (T V : R) {
      if (Numbers.find(V) == Numbers.end()) {
        dfs(V);
      }
    }
  }

private:
  static constexpr int Infinity = -1;
  GetterLambda Getter;
  SetterLambda Setter;
  AdderLambda Adder;
  StartValueLambda StartValue;
  RelationLambda Relation;
  std::vector<T> Stack;
  std::map<T, size_t> Numbers;

  void dfs(T A) {
    Stack.push_back(A);
    const size_t D = Stack.size();
    Numbers[A] = D;

    Setter(A, StartValue(A));
    for (T B : Relation(A)) {
      assert(B && "Node is null");
      if (Numbers.find(B) == Numbers.end()) {
        dfs(B);
      }
      Numbers[A] = std::min(Numbers[A], Numbers[B]);
      Adder(A, B);
    }

    if (Numbers[A] == D) {
      while (true) {
        auto U = Stack.back();
        Numbers[U] = Infinity;
        Setter(U, Getter(A));
        Stack.pop_back();
        if (U == A)
          break;
      }
    }
  }
};
} // namespace

/**
 * Computes the epsilon-free first sets of the grammar.
 *
 * Params:
 * 		grammar = grammar for which the epsilon-free first sets is
 * computed
 */
void lalrtool::calculateFirstSets(Grammar &G) {
  auto Getter = [](Nonterminal *NT) { return NT->getFirstSet(); };
  auto Setter = [](Nonterminal *NT, const FirstSetType &Set) {
    NT->setFirstSet(Set);
  };
  auto Adder = [](Nonterminal *A, Nonterminal *B) {
    FirstSetType Set(A->getFirstSet());
    Set |= B->getFirstSet();
    A->setFirstSet(Set);
  };

  // Start value is nonempty only for A terminal
  auto StartValue = [&G](Nonterminal *A) {
    FirstSetType Set = G.createEmptyFirstSet();
    for (Rule *R = A->getRule(); R; R = R->getNext()) {
      if (R->getRHS().size() > 0) {
        if (auto *TRef = llvm::dyn_cast<TerminalRef>(R->getRHS()[0]))
          Set[TRef->getTerminal()->getID()] = true;
      }
    }
    return Set;
  };

  // Definition of the relation:
  // a R b <=> 1. a is a nonterminal and b its right hand side or
  //           2. b is a direct subexpression of a and contributes to
  //              the first set of a
  auto Relation = [](Nonterminal *A) {
    assert(A && "NT is null");
    llvm::DenseSet<Nonterminal *> Rel;
    for (Rule *R = A->getRule(); R; R = R->getNext()) {
      for (RuleElement *RE : R->getRHS()) {
        if (NonterminalRef *BRef = llvm::dyn_cast<NonterminalRef>(RE)) {
          Nonterminal *B = BRef->getNonterminal();
          Rel.insert(B);
          if (!B->isDerivesEpsilon())
            break;
        }
      }
    }

    return Rel;
  };

  auto R = G.nonterminals();
  using GetterLambda = decltype(Getter);
  using SetterLambda = decltype(Setter);
  using AdderLambda = decltype(Adder);
  using StartValueLambda = decltype(StartValue);
  using RelationLambda = decltype(Relation);
  using RangeType = decltype(R);
  ComputeSetValuedFunc<Nonterminal *, GetterLambda, SetterLambda, AdderLambda,
                       StartValueLambda, RelationLambda, RangeType>(
      Getter, Setter, Adder, StartValue, Relation)(R);
}

/**
 * Computes the follow sets of the grammar.
 *
 * Params:
 * 		grammar = grammar for which the follow sets is computed
 */
void lalrtool::calculateFollowSets(Grammar &G) {
  auto Getter = [](Nonterminal *NT) { return NT->getFollowSet(); };
  auto Setter = [](Nonterminal *NT, const FollowSetType &Set) {
    NT->setFollowSet(Set);
  };
  auto Adder = [](Nonterminal *A, Nonterminal *B) {
    FollowSetType Set(A->getFollowSet());
    Set |= B->getFollowSet();
    A->setFollowSet(Set);
  };

  struct Occurance {
    Rule *R;
    size_t Index;
    bool RightDerivesEpsilon;
  };
  llvm::DenseMap<Nonterminal *, llvm::SmallVector<Occurance, 0>> Occ;

  // Initialize follow relation.
  for (Nonterminal *NT : G.nonterminals()) {
    for (Rule *R : rules(NT)) {
      bool RightDerivesEpsilon = true;
      for (size_t I = 0, E = R->getRHS().size(); I < E; ++I) {
        size_t Index = E - I - 1;
        RuleElement *RE = R->getRHS()[Index];
        if (auto *NTRef = llvm::dyn_cast<NonterminalRef>(RE)) {
          Occ[NTRef->getNonterminal()].emplace_back(
              Occurance{R, Index, RightDerivesEpsilon});
          RightDerivesEpsilon &= NTRef->getNonterminal()->isDerivesEpsilon();
        } else if (llvm::isa<TerminalRef>(RE))
          RightDerivesEpsilon = false;
      }
    }
  }

  // Start values are the epsilon-free first sets right to nonterminal A.
  auto StartValue = [&G,&Occ](Nonterminal *A) {
    FollowSetType Set = G.createEmptyFollowSet();
    for (auto &O : Occ[A]) {
      for (size_t I = O.Index + 1, E = O.R->getRHS().size(); I < E; ++I) {
        RuleElement *RE = O.R->getRHS()[I];
        if (auto *NTRef = llvm::dyn_cast<NonterminalRef>(RE)) {
          Set |= NTRef->getNonterminal()->getFirstSet();
          if (!NTRef->getNonterminal()->isDerivesEpsilon())
            break;
        } else if (auto *TRef = llvm::dyn_cast<TerminalRef>(RE)) {
          Set.set(TRef->getTerminal()->getID());
          break;
        }
      }
    }
    return Set;
  };

  // Definition of the relation:
  // a R b <=> 1. a and b are extended context-free items
  //           2. b contributes to the follow set of a
  //
  // Let Y -> a b c be a production. If b is a regular subexpression
  // then the extended context-free item is written as
  // [ Y -> a .b c ]
  auto Relation = [&Occ](Nonterminal *A) {
    llvm::DenseSet<Nonterminal *> Rel;
    for (auto &O : Occ[A]) {
      if (O.RightDerivesEpsilon)
        Rel.insert(O.R->getNT());
    }
    return Rel;
  };

  auto R = G.nonterminals();

  using GetterLambda = decltype(Getter);
  using SetterLambda = decltype(Setter);
  using AdderLambda = decltype(Adder);
  using StartValueLambda = decltype(StartValue);
  using RelationLambda = decltype(Relation);
  using RangeType = decltype(R);
  ComputeSetValuedFunc<Nonterminal *, GetterLambda, SetterLambda, AdderLambda,
                       StartValueLambda, RelationLambda, RangeType>(
      Getter, Setter, Adder, StartValue, Relation)(R);
}
