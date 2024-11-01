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
void checkGrammar(Diagnostic &Diag, Grammar &G) {
  for (const Nonterminal *Node : G.nonterminals()) {
    if (!Node->isReachable())
      Diag.error(Node->Loc, llvm::Twine("Nonterminal ")
                                .concat(Node->name())
                                .concat(" is not reachable"));
    if (!Node->isProductive())
      Diag.error(Node->Loc, llvm::Twine("Nonterminal ")
                                .concat(Node->name())
                                .concat(" is not productive"));
  }
}

struct LL1Condition {
  LL1Condition(Diagnostic &Diag) : Diag(Diag) {}

  void operator()(Grammar &G) {
    for (Node *Node : G.nodeRange()) {
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
      RightHandSide *Elem = Group->element();
      if (Elem->derivesEpsilon() ||
          nonEmptyIntersection(Elem->FirstSet, Group->FollowSet)) {
        Elem->HasConflict = true;
        if (isCondition(Elem->Inner))
          makeResolver(Elem->Inner);
        else {
          if (Elem->derivesEpsilon())
            Diag.warning(Group->Loc,
                         llvm::Twine("LL conflict in ")
                             .concat(symbolOf(Group)->name())
                             .concat(": contents of [...] or {...} must "
                                     "not be deletable"));
          else
            Diag.warning(Group->Loc, llvm::Twine("LL conflict in ")
                                         .concat(symbolOf(Group)->name())
                                         .concat(": same start and sucessor of "
                                                 "deletable element"));
        }
      } else if (isCondition(Group->element()->Inner)) {
        // The Group is optional but there is no conflict.
        // Turn resolver into predicate.
        makePredicate(Group->element()->Inner);
      }
    }
  }

  void checkAlternative(Alternative *Alt) {
    // Assume the best: no conflict
    llvm::BitVector Set;
    size_t Count = 0;
    for (RightHandSide *N : Alt->alternatives()) {
      Set |= N->FirstSet;
      Count += N->FirstSet.count();
      if (N->derivesEpsilon()) {
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

    for (Node *Ni = Alt->Link; Ni; Ni = Ni->Link) {
      A.clear();
      A |= Ni->FirstSet;
      if (Ni->derivesEpsilon())
        A |= Ni->FollowSet;
      for (Node *Nj = Ni->Link; Nj; Nj = Nj->Link) {
        B.clear();
        B |= Nj->FirstSet;
        if (Nj->derivesEpsilon())
          B |= Nj->FollowSet;
        if (nonEmptyIntersection(A, B)) {
          Ni->HasConflict = true;
          if (isCondition(Ni->Inner))
            makeResolver(Ni->Inner);
          else {
            Diag.warning(Ni->Loc,
                         llvm::Twine("LL conflict in ")
                             .concat(symbolOf(Ni)->name())
                             .concat(": same start of several alternatives"));
            Diag.note(Nj->Loc, llvm::Twine("LL conflict in ")
                                   .concat(symbolOf(Nj)->name())
                                   .concat(": conflicting alternative"));
          }
        }
      }
    }
  }

  void checkAlternativeForPredicate(Alternative *Alt) {
    bool ParentEps = (Alt->Back && Alt->Back->derivesEpsilon());
    for (Node *N = Alt->Link; N; N = N->Link) {
      if ((ParentEps || N->derivesEpsilon()) && !N->HasConflict &&
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
void checkLL(Diagnostic &Diag, Grammar &G) {
  LL1Condition LL(Diag);
  LL(G);
}
} // namespace

void Grammar::writeYAML(llvm::raw_ostream &OS) {
  for (Nonterminal *NT : nonterminals()) {
    OS << NT->name() << ":\n"
       << "  isReachable: " << (NT->isReachable() ? "true" : "false") << "\n"
       << "  isProductive: " << (NT->isProductive() ? "true" : "false") << "\n"
       << "  derivesEpsilon: " << (NT->derivesEpsilon() ? "true" : "false")
       << "\n";
    OS << "  first: [";
    bool First = true;
    for (int TID = NT->FirstSet.find_first(); TID != -1;
         TID = NT->FirstSet.find_next(TID)) {
      Terminal *T = TerminalMap[TID];
      OS << (First ? " " : ", ") << T->name();
      First = false;
    }
    OS << " ]\n";
    OS << "  follow: [";
    First = true;
    for (int TID = NT->FollowSet.find_first(); TID != -1;
         TID = NT->FollowSet.find_next(TID)) {
      Terminal *T = TerminalMap[TID];
      OS << (First ? " " : ", ") << T->name();
      First = false;
    }
    OS << " ]\n";
  }
}

void Grammar::performAnalysis(Diagnostic &Diag) {
  if (Diag.errorsOccured())
    return;
  calculateReachable(*this);
  calculateDerivesEpsilon(*this);
  calculateProductive(*this);
  checkGrammar(Diag, *this);
  if (Diag.errorsOccured())
    return;
  calculateFirstSets(*this);
  calculateFollowSets(*this);
  checkLL(Diag, *this);
}