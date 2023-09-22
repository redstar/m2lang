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
#include "llvm/ADT/TypeSwitch.h"
#include "llvm/Support/Casting.h"
#include <map>
#include <set>
#include <vector>

using namespace lltool;

namespace {
void markReachable(Node *Node) {
  llvm::TypeSwitch<lltool::Node *>(Node)
      .Case([](Terminal *N) { N->setReachable(); })
      .Case([](Nonterminal *N) {
        N->setReachable();
        markReachable(N->getRHS());
      })
      .Case<Group, Alternative>([](auto *N) {
        N->setReachable();
        for (auto *I = N->Link; I; I = I->Link)
          markReachable(I);
      })
      .Case([](Sequence *N) {
        N->setReachable();
        for (auto *I : N->elements())
          markReachable(I);
      })
      .Case([](SymbolRef *N) {
        N->setReachable();
        if (!N->getSymbol()->isReachable())
          markReachable(N->getSymbol());
      })
      .Case([](Code *N) { N->setReachable(); });
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
 *               for group symbols
 */
struct FixedPointComputation {
  FixedPointComputation(bool (*Getter)(Node *), void (*Setter)(Node *, bool),
                        bool (*GroupVal)(Node *), bool TerminalVal)
      : Getter(Getter), Setter(Setter), GroupVal(GroupVal),
        TerminalVal(TerminalVal) {}

  void operator()(Grammar &G) {
    do {
      Changes = false;
      for (auto *NT : G.nonterminals())
        traverse(NT);
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

  void traverse(Node *Node) {
    // This loop is weird...
    for (; Node; Node = Node->Next) {
      llvm::TypeSwitch<lltool::Node *>(Node)
          .Case([this](Terminal *Node) { mark(Node, TerminalVal); })
          .Case([this](Nonterminal *Node) {
            traverse(Node->getRHS());
            mark(Node, Getter(Node->getRHS()));
          })
          .Case([this](Group *Node) {
            traverse(Node->Link);
            mark(Node, GroupVal(Node) || Getter(Node->Link));
          })
          .Case([this](Alternative *Node) {
            bool Val = false;
            for (auto *I = Node->Link; I; I = I->Link) {
              if (!llvm::isa<Code>(I)) {
                traverse(I);
                Val |= Getter(I);
              }
            }
            mark(Node, Val);
          })
          .Case([this](Sequence *Node) {
            bool Val = true;
            traverse(Node->Inner);
            for (auto *I = Node->Inner; I && Val; I = I->Next) {
              if (!llvm::isa<Code>(I))
                Val &= Getter(I);
            }
            mark(Node, Val);
          })
          .Case([this](SymbolRef *Node) {
            if (Node->getTerminal())
              traverse(Node->getTerminal());
            mark(Node, Getter(Node->getSymbol()));
          })
          .Case([](Code *) {});
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
    if (auto *G = llvm::dyn_cast<Group>(N)) {
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

  void dfs(Node *A) {
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
void lltool::calculateFirstSets(Grammar &G) {
  auto Getter = [](Node *N) { return N->FirstSet; };
  auto Setter = [](Node *N, const FirstSetType &Set) { N->FirstSet = Set; };
  auto Adder = [](Node *A, Node *B) { A->FirstSet |= B->FirstSet; };

  // Start value is nonempty only for A terminal
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
  auto Relation = [](Node *A) {
    assert(A && "Node is null");
    std::vector<Node *> Rel;
    llvm::TypeSwitch<lltool::Node *>(A)
        .Case([&](Nonterminal *A) {
          assert(A->Link && "Link is null");
          Rel.push_back(A->Link);
        })
        .Case<Group, Alternative>([&](auto *A) {
          assert(A->Link && "Link is null");
          for (Node *N = A->Link; N; N = N->Link) {
            assert(N && "Node is null (group)");
            Rel.push_back(N);
          }
        })
        .Case([&](Sequence *A) {
          for (Node *N : A->elements()) {
            if (llvm::isa<Code>(N))
              continue;
            assert(N && "Node is null (sequence)");
            Rel.push_back(N);
            if (!N->DerivesEpsilon)
              break;
          }
        })
        .Case([&](SymbolRef *A) {
          assert(A->Inner && "Inner is null");
          Rel.push_back(A->Inner);
        })
        .Case([&](Terminal *) {
          /* A terminal has no relation. */
        })
        .Case([&](Code *) { llvm_unreachable("Statement not reachable"); });
    return Rel;
  };

  auto R = make_filter_range(G.nodeRange(),
                             [](Node *N) { return !llvm::isa<Code>(N); });
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
  auto Adder = [](Node *A, Node *B) { A->FollowSet |= B->FollowSet; };

  // Start values are the epsilon-free first sets
  auto StartValue = [G](Node *A) {
    FollowSetType Set(G.numberOfTerminals());
    // Equation 5
    if (llvm::isa<Sequence>(A) && llvm::isa<Group>(A->Back) &&
        llvm::cast<Group>(A->Back)->isUnlimited()) {
      Set |= A->FirstSet;
    }
    // Equation 1
    else if (llvm::isa<Sequence>(A) && llvm::isa<Nonterminal>(A->Back) &&
             !A->Back->Back) {
      Set[G.eoiTerminal()->No] = true;
    }
    // Equation 3
    else {
      for (Node *N = A->Next; N; N = N->Next) {
        if (!llvm::isa<Code>(N)) {
          Set |= N->FirstSet;
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
  auto Relation = [](Node *A) {
    std::vector<Node *> Rel;

    auto Add = [&Rel](Node *N) {
      if (llvm::isa<Nonterminal>(N)) {
        for (Node *V = N->Back; V; V = V->Link) {
          assert(llvm::isa<SymbolRef>(V) && "Link must be symbol");
          Rel.push_back(V);
        }
      } else
        Rel.push_back(N);
    };

    switch (A->Kind) {
    case Node::NK_SymbolRef:
    case Node::NK_Group:
    case Node::NK_Alternative: {
      Node *N = A->Next;
      while (llvm::isa_and_nonnull<Code>(N))
        N = N->Next;
      if (!N) {
        // Equation (4)
        Add(A->parent());
      } else {
        // Equation (3)
        if (N->DerivesEpsilon) {
          Rel.push_back(N);
        }
      }
    } break;
    case Node::NK_Sequence:
      // Equation: (2), (5), (6)
      assert(A->Back && "Back is null");
      assert(!A->Next && "Next is not null");
      /* add */
      Add(A->Back);
      break;
    case Node::NK_Terminal:
    case Node::NK_Nonterminal:
    case Node::NK_Code:
      llvm_unreachable("Statement not reachable");
    }
    return Rel;
  };

  auto R = make_filter_range(G.nodeRange(), [](Node *N) {
    return llvm::isa<Alternative>(N) || llvm::isa<Group>(N) ||
           llvm::isa<Sequence>(N) || llvm::isa<SymbolRef>(N);
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