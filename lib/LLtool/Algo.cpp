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

#include "lltool/Algo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Casting.h"
#include <map>
#include <set>
#include <vector>

using namespace lltool;

namespace {
void markReachable(Node *node) {
  switch (node->Kind) {
  case Node::NK_Terminal:
    node->IsReachable = true;
    break;
  case Node::NK_Nonterminal:
    node->IsReachable = true;
    markReachable(node->Link);
    break;
  case Node::NK_Group:
  case Node::NK_Alternative:
    node->IsReachable = true;
    for (auto n = node->Link; n; n = n->Link)
      markReachable(n);
    break;
  case Node::NK_Sequence:
    node->IsReachable = true;
    for (auto n = node->Inner; n; n = n->Next)
      markReachable(n);
    break;
  case Node::NK_Symbol:
    node->IsReachable = true;
    if (!node->Inner->IsReachable)
      markReachable(node->Inner);
    break;
  case Node::NK_Code:
    node->IsReachable = true;
    break;
  }
}
} // namespace

/**
 * Marks all symbols reachable from the start symbol.
 *
 * Params:
 * 		grammar = grammar for which the reachability of the symbols is
 *                computed
 */
void lltool::calculateReachable(Grammar &G) {
  markReachable(G.syntheticStartSymbol());
}

namespace {
/**
 * Performs a fixed point computation on boolean values.
 *
 * Params:
 *		prop = name of property
 *		terminalVal = value for terminals
 *		groupval = function / delegate which returns additional value
 *for group symbols
 */
struct FixedPointComputation {
  FixedPointComputation(bool (*Getter)(Node *), void (*Setter)(Node *, bool),
                        bool (*GroupVal)(Node *), bool TerminalVal)
      : Getter(Getter), Setter(Setter), GroupVal(GroupVal),
        TerminalVal(TerminalVal) {}

  void operator()(const Grammar &G) {
    do {
      Changes = false;
      for (auto &N : G.nodes())
        if (llvm::isa<Nonterminal>(N))
          traverse(N);
    } while (Changes);
  }

private:
  bool (*Getter)(Node *);
  void (*Setter)(Node *, bool);
  bool (*GroupVal)(Node *);
  const bool TerminalVal;
  bool Changes;

  void mark(Node *N, bool Val) {
    if (Getter(N) != Val) {
      Setter(N, Val);
      Changes = true;
    }
  }

  void traverse(Node *node) {
    for (; node; node = node->Next) {
      switch (node->Kind) {
      case Node::NK_Terminal:
        mark(node, TerminalVal);
        break;
      case Node::NK_Nonterminal:
        traverse(node->Link);
        mark(node, Getter(node->Link));
        break;
      case Node::NK_Group:
        traverse(node->Link);
        mark(node, GroupVal(node) || Getter(node->Link));
        break;
      case Node::NK_Alternative: {
        bool Val = false;
        for (auto *I = node->Link; I; I = I->Link) {
          if (!llvm::isa<Code>(I)) {
            traverse(I);
            Val |= Getter(I);
          }
        }
        mark(node, Val);
      } break;
      case Node::NK_Sequence: {
        bool Val = true;
        traverse(node->Inner);
        for (auto *I = node->Inner; I && Val; I = I->Next) {
          if (!llvm::isa<Code>(I))
            Val &= Getter(I);
        }
        mark(node, Val);
      } break;
      case Node::NK_Symbol:
        if (node->Inner->Kind == Node::NK_Terminal)
          traverse(node->Inner);
        mark(node, Getter(node->Inner));
        break;
      case Node::NK_Code:
        break;
      }
    }
  }
};
} // namespace

/**
 * Calculates the epsilon productivity for each element of the grammar.
 *
 * Params:
 * 		grammar = grammar for which the epsilon productivity of the
 * symbols is computed
 */
void lltool::calculateDerivesEpsilon(Grammar &G) {
  auto Getter = [](Node *N) { return N->DerivesEpsilon; };
  auto Setter = [](Node *N, bool Val) { N->DerivesEpsilon = Val; };
  auto GroupVal = [](Node *N) {
    if (auto G = llvm::dyn_cast<Group>(N)) {
      return G->Cardinality == Group::ZeroOrMore ||
             G->Cardinality == Group::ZeroOrOne;
    }
    return false;
  };
  FixedPointComputation(Getter, Setter, GroupVal, false)(G);
}

/**
 * Calculates the productivity of each symbol of the grammar.
 *
 * Params:
 * 		grammar = grammar for which the productivity of the symbols is
 *                computed
 */
void lltool::calculateProductive(Grammar &G) {
  auto Getter = [](Node *N) { return N->IsProductive; };
  auto Setter = [](Node *N, bool Val) { N->IsProductive = Val; };
  auto GroupVal = [](Node *N) { return false; };
  FixedPointComputation(Getter, Setter, GroupVal, true)(G);
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
      : V_INFINITY(-1), Getter(Getter), Setter(Setter), Adder(Adder),
        StartValue(StartValue), Relation(Relation) {}

  void operator()(RangeType R) {
    for (T v : R) {
      if (Numbers.find(v) == Numbers.end()) {
        dfs(v);
      }
    }
  }

private:
  const int V_INFINITY;
  GetterLambda Getter;
  SetterLambda Setter;
  AdderLambda Adder;
  StartValueLambda StartValue;
  RelationLambda Relation;
  std::vector<T> Stack;
  std::map<T, size_t> Numbers;

  void dfs(Node *a) {
    Stack.push_back(a);
    const size_t d = Stack.size();
    Numbers[a] = d;

    Setter(a, StartValue(a));
    for (T b : Relation(a)) {
      assert(b && "Node is null");
      if (Numbers.find(b) == Numbers.end()) {
        dfs(b);
      }
      Numbers[a] = std::min(Numbers[a], Numbers[b]);
      Adder(a, b);
    }

    if (Numbers[a] == d) {
      while (true) {
        auto t = Stack.back();
        Numbers[t] = V_INFINITY;
        Setter(t, Getter(a));
        Stack.pop_back();
        if (t == a)
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
void lltool::calculateFirstSets(Grammar &G) {
  auto Getter = [](Node *N) { return N->FirstSet; };
  auto Setter = [](Node *N, const FirstSetType &Set) { N->FirstSet = Set; };
  auto Adder = [](Node *A, Node *B) {
      A->FirstSet |= B->FirstSet;
  };

  // Start value is nonempty only for a terminal
  auto StartValue = [G](Node *A) {
    FirstSetType Set(G.numberOfTerminals());
    if (auto *T = llvm::dyn_cast<Terminal>(A)) {
      Set[T->No] = true;
    }
    return Set;
  };

  // Definition of the relation:
  // a R b <=> 1. a is a nonterminal and b its right hand side or
  //           2. b is a direct subexpression of a and contributes to
  //              the first set of a
  auto Relation = [](Node *a) {
    assert(a && "Node is null");
    std::vector<Node *> rel;
    switch (a->Kind) {
    case Node::NK_Nonterminal:
      assert(a->Link && "Link is null");
      rel.push_back(a->Link);
      break;
    case Node::NK_Group:
    case Node::NK_Alternative:
      assert(a->Link && "Link is null");
      for (Node *n = a->Link; n; n = n->Link) {
        assert(n && "Node is null (group)");
        rel.push_back(n);
      }
      break;
    case Node::NK_Sequence:
      for (Node *n = a->Inner; n; n = n->Next) {
        if (llvm::isa<Code>(n))
          continue;
        assert(n && "Node is null (sequence)");
        rel.push_back(n);
        if (!n->DerivesEpsilon)
          break;
      }
      break;
    case Node::NK_Symbol:
      assert(a->Inner && "Inner is null");
      rel.push_back(a->Inner);
      break;
    case Node::NK_Terminal:
      // A terminal has no relation
      break;
    case Node::NK_Code:
      llvm_unreachable("Statement not reachable");
    }
    return rel;
  };

  auto R = make_filter_range(G.nodeRange(),
                             [](Node *n) { return !llvm::isa<Code>(n); });
  using GetterLambda = decltype(Getter);
  using SetterLambda = decltype(Setter);
  using AdderLambda = decltype(Adder);
  using StartValueLambda = decltype(StartValue);
  using RelationLambda = decltype(Relation);
  using RangeType = decltype(R);
  ComputeSetValuedFunc<Node *, GetterLambda, SetterLambda, AdderLambda,
                       StartValueLambda, RelationLambda, RangeType>(
      Getter, Setter, Adder, StartValue, Relation)(R);
}

/**
 * Computes the follow sets of the grammar.
 *
 * Params:
 * 		grammar = grammar for which the follow sets is computed
 */
void lltool::calculateFollowSets(Grammar &G) {
  auto Getter = [](Node *N) { return N->FollowSet; };
  auto Setter = [](Node *N, const FollowSetType &Set) { N->FollowSet = Set; };
  auto Adder = [](Node *A, Node *B) {
    A->FollowSet |= B->FollowSet;
  };

  // Start values are the epsilon-free first sets
  auto StartValue = [G](Node *a) {
    FollowSetType set(G.numberOfTerminals());
    // Equation 5
    if (llvm::isa<Sequence>(a) && llvm::isa<Group>(a->Back) &&
        llvm::cast<Group>(a->Back)->isUnlimited()) {
      set |= a->FirstSet;
    }
    // Equation 1
    else if (llvm::isa<Sequence>(a) && llvm::isa<Nonterminal>(a->Back) &&
             !a->Back->Back) {
      set[G.eoiTerminal()->No] = true;
    }
    // Equation 3
    else {
      for (Node *n = a->Next; n; n = n->Next) {
        if (!llvm::isa<Code>(n)) {
          set |= n->FirstSet;
          break;
        }
      }
    }
    return set;
  };

  // Definition of the relation:
  // a R b <=> 1. a and b are extended context-free items
  //           2. b contributes to the follow set of a
  //
  // Let Y -> a b c be a production. If b is a regular subexpression
  // then the extended context-free item is written as
  // [ Y -> a .b c ]
  auto Relation = [](Node *a) {
    std::vector<Node *> rel;
    /*
                    void add(Node n)
                    {
                            if (n.type == NodeType.Nonterminal)
                            {
                                    foreach (v; NodeLinkRange(n.back))
                                    {
                                            assert(v.type ==
       NodeType.Symbol); rel ~= v;
                                    }
                            }
                            else
                            {
                                    rel ~= n;
                            }
                    }
    */
    switch (a->Kind) {
    case Node::NK_Symbol:
    case Node::NK_Group:
    case Node::NK_Alternative: {
      Node *n = a->Next;
      while (n && llvm::isa<Code>(n))
        n = n->Next;
      if (!n) {
        if (a->Back) {
          /* add */
          if (llvm::isa<Nonterminal>(a->Back)) {
            for (Node *v = a->Back->Back; v; v = v->Link) {
              assert(llvm::isa<Symbol>(v) && "Link must be symbol");
              rel.push_back(v);
            }
          } else
            rel.push_back(a->Back);
        }
      } else {
        // Equation (3)
        if (n->DerivesEpsilon) {
          rel.push_back(n);
        }
      }
    } break;
    case Node::NK_Sequence:
      // Equation: (2), (5), (6)
      assert(a->Back && "Back is null");
      assert(!a->Next && "Next is not null");
      /* add */
      if (llvm::isa<Nonterminal>(a->Back)) {
        for (Node *v = a->Back->Back; v; v = v->Link) {
          assert(llvm::isa<Symbol>(v) && "Link must be symbol");
          rel.push_back(v);
        }
      } else
        rel.push_back(a->Back);
      break;
    case Node::NK_Terminal:
      assert(false && "Statement not reachable");
    case Node::NK_Nonterminal:
      assert(false && "Statement not reachable");
    case Node::NK_Code:
      assert(false && "Statement not reachable");
    }
    return rel;
  };

  auto R = make_filter_range(G.nodeRange(), [](Node *n) {
    return llvm::isa<Alternative>(n) || llvm::isa<Group>(n) ||
           llvm::isa<Sequence>(n) || llvm::isa<Symbol>(n);
  });

  using GetterLambda = decltype(Getter);
  using SetterLambda = decltype(Setter);
  using AdderLambda = decltype(Adder);
  using StartValueLambda = decltype(StartValue);
  using RelationLambda = decltype(Relation);
  using RangeType = decltype(R);
  ComputeSetValuedFunc<Node *, GetterLambda, SetterLambda, AdderLambda,
                       StartValueLambda, RelationLambda, RangeType>(
      Getter, Setter, Adder, StartValue, Relation)(R);
}