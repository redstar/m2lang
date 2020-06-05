//===--- Grammar.cpp - LLtool grammar definition ----------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
///
///
//===----------------------------------------------------------------------===//

#include "Grammar.h"
#include "Algo.h"
#include "Diagnostic.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/raw_ostream.h"

using namespace lltool;

namespace {
void checkGrammar(Diagnostic &Diag, const std::vector<Node *> &Nodes) {
  for (Node *n : llvm::make_filter_range(
           Nodes, [](Node *n) { return n->Kind == Node::NK_Nonterminal; })) {
    Nonterminal *node = llvm::cast<Nonterminal>(n);
    if (!node->IsReachable)
      Diag.error(node->Loc, llvm::Twine("Nonterminal ")
                                .concat(node->Name)
                                .concat(" is not reachable"));
    if (!node->IsProductive)
      Diag.error(node->Loc, llvm::Twine("Nonterminal ")
                                .concat(node->Name)
                                .concat(" is not productive"));
  }
}

#if 0
/**
 * Check if LL(1) conflicts exist in the grammar.
 *
 * Params:
 *      buffer = content of grammar file (for error messages)
 *      grammar = grammar to check
 */
void checkLL(const(char)[] buffer, Grammar grammar)
{

    void checkAlternative(Node node)
    {
        // Assume the best: no conflict
        TerminalSet set = new TerminalSet;
        size_t count = 0;
        foreach (n; NodeLinkRange(node.link))
        {
            set.insert(n.firstSet[]);
            count += n.firstSet.length;
            if (n.derivesEpsilon)
            {
                set.insert(node.followSet[]);
                count += n.followSet.length;
            }
            if (count != set.length)
                goto conflict;
        }
        return ;
conflict:
        TerminalSet a = new TerminalSet;
        TerminalSet b = new TerminalSet;
        foreach (ni; NodeLinkRange(node.link))
        {
            a.clear;
            a.insert(ni.firstSet[]);
            if (ni.derivesEpsilon)
                a.insert(ni.followSet[]);
            foreach (nj; NodeLinkRange(ni.link))
            {
                b.clear;
                b.insert(nj.firstSet[]);
                if (nj.derivesEpsilon)
                    b.insert(nj.followSet[]);
                if (setIntersection(a[], b[]).count > 0)
                {
                    ni.hasConflict = true;
                    if (isCondition(ni.inner))
                        makeResolver(ni.inner);
                    else
                    {
                        warning(buffer, ni.pos, "LL conflict in %s: same start of several alternatives", symbolOf(ni).name);
                        warning(buffer, nj.pos, "LL conflict in %s: conflicting alternative", symbolOf(nj).name);
                    }
                }
            }
        }
    }


	foreach (node; filter!(n => n.type == NodeType.Code)(grammar.nodes))
    {
        if (isCondition(node))
            warning(buffer, node.pos, "No LL conflict in %s: misplaced resolver", symbolOf(node).name);
    }
}
#endif

struct LL1Condition {
  LL1Condition(Diagnostic &Diag) : Diag(Diag) {}

  void operator()(const std::vector<Node *> &Nodes) {
    for (Node *node : Nodes) {
      if (auto N = llvm::dyn_cast<Group>(node))
        checkGroup(N);
      else if (auto N = llvm::dyn_cast<Alternative>(node)) {
        checkAlternative(N);
        checkAlternativeForPredicate(N);
      }
    }
  }

private:
  Diagnostic &Diag;
  void checkGroup(Group *group) {
    if (group->isOptional()) {
      if (group->Link->DerivesEpsilon ||
          nonEmptyIntersection(group->Link->FirstSet, group->FollowSet)) {
        group->Link->HasConflict = true;
        if (isCondition(group->Link->Inner))
          makeResolver(group->Link->Inner);
        else {
          if (group->Link->DerivesEpsilon)
            Diag.warning(group->Loc,
                         llvm::Twine("LL conflict in ")
                             .concat(symbolOf(group)->Name)
                             .concat(": contents of [...] or {...} must "
                                     "not be deletable"));
          else
            Diag.warning(group->Loc, llvm::Twine("LL conflict in ")
                                         .concat(symbolOf(group)->Name)
                                         .concat(": same start and sucessor of "
                                                 "deletable element"));
        }
      } else if (isCondition(group->Link->Inner)) {
        // The group is optional but there is no conflict.
        // Turn resolver into predicate.
        makePredicate(group->Link->Inner);
      }
    }
  }

  void checkAlternative(Alternative *alt) {
    // Assume the best: no conflict
    llvm::BitVector set;
    size_t count = 0;
    for (Node *N = alt->Link; N; N = N->Link) {
      set |= N->FirstSet;
      count += N->FirstSet.count();
      if (N->DerivesEpsilon) {
        set |= N->FollowSet;
        count += N->FollowSet.count();
      }
      if (count != set.count())
        goto conflict;
    }
    return;
  conflict:
    llvm::BitVector a;
    llvm::BitVector b;
    ;
    for (Node *ni = alt->Link; ni; ni = ni->Link) {
      a.clear();
      a |= ni->FirstSet;
      if (ni->DerivesEpsilon)
                a |= ni->FollowSet;
      for (Node *nj = ni->Link; nj; nj = nj->Link) {
        b.clear();
        b |= nj->FirstSet;
        if (nj->DerivesEpsilon)
          b |= nj->FollowSet;
        if (nonEmptyIntersection(a, b)) {
          ni->HasConflict = true;
          if (isCondition(ni->Inner))
            makeResolver(ni->Inner);
          else {
            Diag.warning(ni->Loc,
                         llvm::Twine("LL conflict in ")
                             .concat(symbolOf(ni)->Name)
                             .concat(": same start of several alternatives"));
            Diag.note(nj->Loc, llvm::Twine("LL conflict in ")
                                   .concat(symbolOf(nj)->Name)
                                   .concat(": conflicting alternative"));
          }
        }
      }
    }
  }

  void checkAlternativeForPredicate(Alternative *alt) {
    bool parentEps = (alt->Back && alt->Back->DerivesEpsilon);
    for (Node *n = alt->Link; n; n = n->Link) {
      if ((parentEps || n->DerivesEpsilon) && !n->HasConflict &&
          isCondition(n->Inner)) {
        makePredicate(n->Inner);
      }
    }
  }

  bool isCondition(Node *n) {
    if (auto C = llvm::dyn_cast_or_null<Code>(n))
      return C->Type == Code::Condition;
    return false;
  }

  void makePredicate(Node *n) {
    assert(isCondition(n) && "Node must be of type code with condition");
    llvm::cast<Code>(n)->Type = Code::Predicate;
  }

  void makeResolver(Node *n) {
    assert(isCondition(n) && "Node must be of type code with condition");
    llvm::cast<Code>(n)->Type = Code::Resolver;
  }

  bool nonEmptyIntersection(const llvm::BitVector &A,
                            const llvm::BitVector &B) {
    llvm::BitVector C(A);
    C &= B;
    return C.any();
  }

  /**
   * Returns the left hand symbol (nonterminal) of the given node.
   *
   * Params:
   *      n = right hand side element (no terminal!)
   *
   * Returns:
   *      left hand side symbol
   */
  Nonterminal *symbolOf(Node *n) {
    if (llvm::isa<Nonterminal>(n))
      return llvm::cast<Nonterminal>(n);
  repeat:
    while (!n->Back) {
      assert(n->Next && "Next is null");
      n = n->Next;
    }
    while (!llvm::isa<Nonterminal>(n)) {
      if (!n->Back)
        goto repeat;
      assert(n->Back && "Back is null");
      n = n->Back;
    }
    return llvm::cast<Nonterminal>(n);
  }
};

// Check for LL(1) conflicts
void checkLL(Diagnostic &Diag, const std::vector<Node *> &Nodes) {
  LL1Condition LL(Diag);
  LL(Nodes);
}
} // namespace

void Grammar::performAnalysis(Diagnostic &Diag) {
  if (Diag.errorsOccured())
    return;
  calculateReachable(*this);
  calculateDerivesEpsilon(*this);
  calculateProductive(*this);
  checkGrammar(Diag, nodes());
  if (Diag.errorsOccured())
    return;
  calculateFirstSets(*this);
  calculateFollowSets(*this);
  checkLL(Diag, nodes());
}