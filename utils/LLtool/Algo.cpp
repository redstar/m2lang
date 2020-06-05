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

#include "Algo.h"
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
  FixedPointComputation(bool (*getter)(Node *), void (*setter)(Node *, bool),
                        bool (*groupVal)(Node *), bool terminalVal)
      : getter(getter), setter(setter), groupVal(groupVal),
        terminalVal(terminalVal) {}

  void operator()(const Grammar &G) {
    do {
      changes = false;
      for (auto &N : G.nodes())
        if (llvm::isa<Nonterminal>(N))
          traverse(N);
    } while (changes);
  }

private:
  bool (*getter)(Node *);
  void (*setter)(Node *, bool);
  bool (*groupVal)(Node *);
  const bool terminalVal;
  bool changes;

  void mark(Node *node, bool val) {
    if (getter(node) != val) {
      setter(node, val);
      changes = true;
    }
  }

  void traverse(Node *node) {
    for (; node; node = node->Next) {
      switch (node->Kind) {
      case Node::NK_Terminal:
        mark(node, terminalVal);
        break;
      case Node::NK_Nonterminal:
        traverse(node->Link);
        mark(node, getter(node->Link));
        break;
      case Node::NK_Group:
        traverse(node->Link);
        mark(node, groupVal(node) || getter(node->Link));
        break;
      case Node::NK_Alternative: {
        bool val = false;
        for (auto n = node->Link; n; n = n->Link) {
          if (!llvm::isa<Code>(n)) {
            traverse(n);
            val |= getter(n);
          }
        }
        mark(node, val);
      } break;
      case Node::NK_Sequence: {
        bool val = true;
        traverse(node->Inner);
        for (auto n = node->Inner; n && val; n = n->Next) {
          if (!llvm::isa<Code>(n))
            val &= getter(n);
        }
        mark(node, val);
      } break;
      case Node::NK_Symbol:
        if (node->Inner->Kind == Node::NK_Terminal)
          traverse(node->Inner);
        mark(node, getter(node->Inner));
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
  auto getter = [](Node *n) { return n->DerivesEpsilon; };
  auto setter = [](Node *n, bool b) { n->DerivesEpsilon = b; };
  auto groupval = [](Node *n) {
    if (auto G = llvm::dyn_cast<Group>(n)) {
      return G->Cardinality == Group::ZeroOrMore ||
             G->Cardinality == Group::ZeroOrOne;
    }
    return false;
  };
  FixedPointComputation(getter, setter, groupval, false)(G);
}

/**
 * Calculates the productivity of each symbol of the grammar.
 *
 * Params:
 * 		grammar = grammar for which the productivity of the symbols is
 *                computed
 */
void lltool::calculateProductive(Grammar &G) {
  auto getter = [](Node *n) { return n->IsProductive; };
  auto setter = [](Node *n, bool b) { n->IsProductive = b; };
  auto groupval = [](Node *n) { return false; };
  FixedPointComputation(getter, setter, groupval, true)(G);
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

  ComputeSetValuedFunc(GetterLambda getter, SetterLambda setter,
                       AdderLambda adder, StartValueLambda startValue,
                       RelationLambda relation)
      : V_INFINITY(-1), getter(getter), setter(setter), adder(adder),
        startValue(startValue), relation(relation) {}

  void operator()(RangeType R) {
    for (T v : R) {
      if (numbers.find(v) == numbers.end()) {
        dfs(v);
      }
    }
  }

private:
  const int V_INFINITY;
  GetterLambda getter;
  SetterLambda setter;
  AdderLambda adder;
  StartValueLambda startValue;
  RelationLambda relation;
  std::vector<T> stack;
  std::map<T, size_t> numbers;

  void dfs(Node *a) {
    stack.push_back(a);
    const size_t d = stack.size();
    numbers[a] = d;

    setter(a, startValue(a));
    for (T b : relation(a)) {
      assert(b && "Node is null");
      if (numbers.find(b) == numbers.end()) {
        dfs(b);
      }
      numbers[a] = std::min(numbers[a], numbers[b]);
      adder(a, b);
    }

    if (numbers[a] == d) {
      while (true) {
        auto t = stack.back();
        numbers[t] = V_INFINITY;
        setter(t, getter(a));
        stack.pop_back();
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
  auto getter = [](Node *n) { return n->FirstSet; };
  auto setter = [](Node *n, const FirstSetType &set) { n->FirstSet = set; };
  auto adder = [](Node *a, Node *b) {
      a->FirstSet |= b->FirstSet;
  };

  // Start value is nonempty only for a terminal
  auto startValue = [G](Node *a) {
    FirstSetType set(G.numberOfTerminals());
    if (a->Kind == Node::NK_Terminal) {
      set[llvm::cast<Terminal>(a)->No] = true;
    }
    return set;
  };

  // Definition of the relation:
  // a R b <=> 1. a is a nonterminal and b its right hand side or
  //           2. b is a direct subexpression of a and contributes to
  //              the first set of a
  auto relation = [](Node *a) {
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
  using GetterLambda = decltype(getter);
  using SetterLambda = decltype(setter);
  using AdderLambda = decltype(adder);
  using StartValueLambda = decltype(startValue);
  using RelationLambda = decltype(relation);
  using RangeType = decltype(R);
  ComputeSetValuedFunc<Node *, GetterLambda, SetterLambda, AdderLambda,
                       StartValueLambda, RelationLambda, RangeType>(
      getter, setter, adder, startValue, relation)(R);
}

/**
 * Computes the follow sets of the grammar.
 *
 * Params:
 * 		grammar = grammar for which the follow sets is computed
 */
void lltool::calculateFollowSets(Grammar &G) {
  auto getter = [](Node *n) { return n->FollowSet; };
  auto setter = [](Node *n, const FollowSetType &set) { n->FollowSet = set; };
  auto adder = [](Node *a, Node *b) {
    a->FollowSet |= b->FollowSet;
  };

  // Start values are the epsilon-free first sets
  auto startValue = [G](Node *a) {
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
  auto relation = [](Node *a) {
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

  using GetterLambda = decltype(getter);
  using SetterLambda = decltype(setter);
  using AdderLambda = decltype(adder);
  using StartValueLambda = decltype(startValue);
  using RelationLambda = decltype(relation);
  using RangeType = decltype(R);
  ComputeSetValuedFunc<Node *, GetterLambda, SetterLambda, AdderLambda,
                       StartValueLambda, RelationLambda, RangeType>(
      getter, setter, adder, startValue, relation)(R);
}