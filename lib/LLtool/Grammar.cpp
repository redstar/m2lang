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

#include "lltool/Grammar.h"
#include "lltool/Algo.h"
#include "lltool/Diagnostic.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/raw_ostream.h"

using namespace lltool;

namespace {
void checkGrammar(Diagnostic &Diag, const std::vector<Node *> &Nodes) {
  for (Node *N : llvm::make_filter_range(
           Nodes, [](Node *N) { return N->Kind == Node::NK_Nonterminal; })) {
    Nonterminal *Node = llvm::cast<Nonterminal>(N);
    if (!Node->IsReachable)
      Diag.error(Node->Loc, llvm::Twine("Nonterminal ")
                                .concat(Node->Name)
                                .concat(" is not reachable"));
    if (!Node->IsProductive)
      Diag.error(Node->Loc, llvm::Twine("Nonterminal ")
                                .concat(Node->Name)
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

    void checkAlternative(Node Node)
    {
        // Assume the best: no conflict
        TerminalSet Set = new TerminalSet;
        size_t Count = 0;
        foreach (N; NodeLinkRange(Node.link))
        {
            Set.insert(N.firstSet[]);
            Count += N.firstSet.length;
            if (N.derivesEpsilon)
            {
                Set.insert(Node.followSet[]);
                Count += N.followSet.length;
            }
            if (Count != Set.length)
                goto conflict;
        }
        return ;
conflict:
        TerminalSet A = new TerminalSet;
        TerminalSet B = new TerminalSet;
        foreach (Ni; NodeLinkRange(Node.link))
        {
            A.clear;
            A.insert(Ni.firstSet[]);
            if (Ni.derivesEpsilon)
                A.insert(Ni.followSet[]);
            foreach (Nj; NodeLinkRange(Ni.link))
            {
                B.clear;
                B.insert(Nj.firstSet[]);
                if (Nj.derivesEpsilon)
                    B.insert(Nj.followSet[]);
                if (setIntersection(A[], B[]).Count > 0)
                {
                    Ni.hasConflict = true;
                    if (isCondition(Ni.inner))
                        makeResolver(Ni.inner);
                    else
                    {
                        warning(buffer, Ni.pos, "LL conflict in %s: same start of several alternatives", symbolOf(Ni).name);
                        warning(buffer, Nj.pos, "LL conflict in %s: conflicting alternative", symbolOf(Nj).name);
                    }
                }
            }
        }
    }


	foreach (Node; filter!(N => N.type == NodeType.Code)(grammar.nodes))
    {
        if (isCondition(Node))
            warning(buffer, Node.pos, "No LL conflict in %s: misplaced resolver", symbolOf(Node).name);
    }
}
#endif

struct LL1Condition {
  LL1Condition(Diagnostic &Diag) : Diag(Diag) {}

  void operator()(const std::vector<Node *> &Nodes) {
    for (Node *Node : Nodes) {
      if (auto *N = llvm::dyn_cast<Group>(Node))
        checkGroup(N);
      else if (auto *N = llvm::dyn_cast<Alternative>(Node)) {
        checkAlternative(N);
        checkAlternativeForPredicate(N);
      }
    }
  }

private:
  Diagnostic &Diag;
  void checkGroup(Group *Group) {
    if (Group->isOptional()) {
      if (Group->Link->DerivesEpsilon ||
          nonEmptyIntersection(Group->Link->FirstSet, Group->FollowSet)) {
        Group->Link->HasConflict = true;
        if (isCondition(Group->Link->Inner))
          makeResolver(Group->Link->Inner);
        else {
          if (Group->Link->DerivesEpsilon)
            Diag.warning(Group->Loc,
                         llvm::Twine("LL conflict in ")
                             .concat(symbolOf(Group)->Name)
                             .concat(": contents of [...] or {...} must "
                                     "not be deletable"));
          else
            Diag.warning(Group->Loc, llvm::Twine("LL conflict in ")
                                         .concat(symbolOf(Group)->Name)
                                         .concat(": same start and sucessor of "
                                                 "deletable element"));
        }
      } else if (isCondition(Group->Link->Inner)) {
        // The Group is optional but there is no conflict.
        // Turn resolver into predicate.
        makePredicate(Group->Link->Inner);
      }
    }
  }

  void checkAlternative(Alternative *Alt) {
    // Assume the best: no conflict
    llvm::BitVector Set;
    size_t Count = 0;
    for (Node *N = Alt->Link; N; N = N->Link) {
      Set |= N->FirstSet;
      Count += N->FirstSet.count();
      if (N->DerivesEpsilon) {
        Set |= N->FollowSet;
        Count += N->FollowSet.count();
      }
      if (Count != Set.count())
        goto conflict;
    }
    return;
  conflict:
    llvm::BitVector A;
    llvm::BitVector B;
    ;
    for (Node *Ni = Alt->Link; Ni; Ni = Ni->Link) {
      A.clear();
      A |= Ni->FirstSet;
      if (Ni->DerivesEpsilon)
                A |= Ni->FollowSet;
      for (Node *Nj = Ni->Link; Nj; Nj = Nj->Link) {
        B.clear();
        B |= Nj->FirstSet;
        if (Nj->DerivesEpsilon)
          B |= Nj->FollowSet;
        if (nonEmptyIntersection(A, B)) {
          Ni->HasConflict = true;
          if (isCondition(Ni->Inner))
            makeResolver(Ni->Inner);
          else {
            Diag.warning(Ni->Loc,
                         llvm::Twine("LL conflict in ")
                             .concat(symbolOf(Ni)->Name)
                             .concat(": same start of several alternatives"));
            Diag.note(Nj->Loc, llvm::Twine("LL conflict in ")
                                   .concat(symbolOf(Nj)->Name)
                                   .concat(": conflicting alternative"));
          }
        }
      }
    }
  }

  void checkAlternativeForPredicate(Alternative *Alt) {
    bool ParentEps = (Alt->Back && Alt->Back->DerivesEpsilon);
    for (Node *N = Alt->Link; N; N = N->Link) {
      if ((ParentEps || N->DerivesEpsilon) && !N->HasConflict &&
          isCondition(N->Inner)) {
        makePredicate(N->Inner);
      }
    }
  }

  bool isCondition(Node *N) {
    if (auto *C = llvm::dyn_cast_or_null<Code>(N))
      return C->Type == Code::Condition;
    return false;
  }

  void makePredicate(Node *N) {
    assert(isCondition(N) && "Node must be of type code with condition");
    llvm::cast<Code>(N)->Type = Code::Predicate;
  }

  void makeResolver(Node *N) {
    assert(isCondition(N) && "Node must be of type code with condition");
    llvm::cast<Code>(N)->Type = Code::Resolver;
  }

  bool nonEmptyIntersection(const llvm::BitVector &A,
                            const llvm::BitVector &B) {
    llvm::BitVector C(A);
    C &= B;
    return C.any();
  }

  /**
   * Returns the left hand symbol (nonterminal) of the given Node.
   *
   * Params:
   *      N = right hand side element (no terminal!)
   *
   * Returns:
   *      left hand side symbol
   */
  Nonterminal *symbolOf(Node *N) {
    if (llvm::isa<Nonterminal>(N))
      return llvm::cast<Nonterminal>(N);
  repeat:
    while (!N->Back) {
      assert(N->Next && "Next is null");
      N = N->Next;
    }
    while (!llvm::isa<Nonterminal>(N)) {
      if (!N->Back)
        goto repeat;
      assert(N->Back && "Back is null");
      N = N->Back;
    }
    return llvm::cast<Nonterminal>(N);
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