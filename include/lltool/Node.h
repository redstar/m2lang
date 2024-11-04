//===--- Node.h - LLtool node definition ------------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the nodes of the graph representing the grammar.
///
/// Each elemement of the graph is a Node.
/// The left-hand side of the grammar is a Symbol, which can be a Terminal or
/// a Nonterminal.
/// The right-hand side of the grammar consists of MetaSymbol, SymbolRef, or
/// Code.
///
//===----------------------------------------------------------------------===//

#ifndef LLTOOL_NODE_H
#define LLTOOL_NODE_H

#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/iterator.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"

namespace lltool {
class RightHandSide;
class Symbol;
class Terminal;
class Nonterminal;

using FirstSetType = llvm::BitVector;
using FollowSetType = llvm::BitVector;

// The NodeIterator allows to iterate over grammar nodes.
// The parameters are:
// T - the type you are iterating over
// PtrT - the type of the underlying pointer
// Next - a pointer to a member function returning the next element
template <typename T, typename PtrT, auto Next>
class NodeIterator
    : public llvm::iterator_facade_base<NodeIterator<T, PtrT, Next>,
                                        std::forward_iterator_tag, T *> {
  T *Ptr;

public:
  NodeIterator(T *Ptr) : Ptr(Ptr) {}

  // Either define both copy constructor && copy assignment or use default.

  bool operator==(const NodeIterator<T, PtrT, Next> &Iter) const {
    return Ptr == Iter.Ptr;
  }

  T *operator*() const { return Ptr; }

  NodeIterator<T, PtrT, Next> &operator++() {
    if (Ptr)
      Ptr = llvm::cast_or_null<T>((Ptr->*Next)());
    return *this;
  }
};

class Node {
public:
  enum NodeKind {
    NK_Terminal,
    NK_Nonterminal,
    NK_Group,
    NK_Alternative,
    NK_Sequence,
    NK_SymbolRef,
    NK_Code
  };

  const llvm::SMLoc Loc;

  /* Next and Link form a 2-dimensional structure.
   *
   * Next points to the element to the right.
   * Links points down.
   *
   * Other pointers are:
   * Inner is a cross-reference.
   * Back points back to the parent iff Next is nullptr.
   *
   * E.g. the rule A -> a | b can be depicted as:
   *
   *   A (NT)
   *    |
   *    | Link
   *    |
   *   Alt --------> SymRef --------> a (T)
   *    |   Next             Inner
   *    |
   *   SymRef --------> b (T)
   *           Inner
   *
   * Unfortunately, there are some exceptions to this picture:
   * Nonterminals form a single-linked list via Next.
   * SymbolRef's to Nonterminals form a single-linked list via Link, with head
   * in Back of the Nonterminal.
   *
   * TODO Refactoring!
   *
   * In general, the structure is not bad, but more encapsulation is needed.
   * If the abuse of the Link pointer in SymbolRef is fixed, then the class
   * Sequence can most likely removed.
   * Next and Link are only required on the right-hand side of a rule.
   * A Nonterminal needs 3 pointer, but they can be named differently.
   * A Terminal does not need pointers at all.
   * Replacing SymbolRef with NonterminalRef and TerminalRef may be useful.
   */

  // Next node in sequence.
  Node *Next;

  // Link to next sequence (nonterminal, group or alternative)
  // or linked list of nonterminal occurances.
  Node *Link;

  // Content of a group or alternative or symbol.
  Node *Inner;

  // Parent node in graph.
  Node *Back;

  const NodeKind Kind;

private:
  bool IsReachable;
  bool DerivesEpsilon;
  bool IsProductive;

public:
  // True if there is a LL(1) conflict.
  bool HasConflict;

  FirstSetType FirstSet;
  FollowSetType FollowSet;

  Node(NodeKind K, llvm::SMLoc Loc)
      : Loc(Loc), Next(nullptr), Link(nullptr), Inner(nullptr), Back(nullptr),
        Kind(K), IsReachable(false), DerivesEpsilon(false), IsProductive(false),
        HasConflict(false) {}

  void setReachable() { IsReachable = true; }
  bool isReachable() const { return IsReachable; }

  void setDerivesEpsilon() { DerivesEpsilon = true; }
  bool derivesEpsilon() const { return DerivesEpsilon; }

  void setProductive() { IsProductive = true; }
  bool isProductive() const { return IsProductive; }

  Node *parent() {
    // The back pointer is only set for the first and last element of
    // a sequence.
    Node *N = this;
    while (!N->Back) {
      assert(N->Next && "Node-> next is null");
      N = N->Next;
    }
    return N->Back;
  }

  Node *next() const { return Next; }
  Node *link() const { return Link; }
};

template <typename T>
using NodeNextIterator = NodeIterator<T, Node, &Node::next>;

template <typename T>
using NodeLinkIterator = NodeIterator<T, Node, &Node::link>;

class RightHandSide : public Node {
protected:
  RightHandSide(NodeKind Kind, llvm::SMLoc Loc) : Node(Kind, Loc) {}

public:
  // All right-hand side elements are in a list.
  void setNext(RightHandSide *RHS) { Next = RHS; }
  RightHandSide *next() { return llvm::cast_or_null<RightHandSide>(Next); }

  static bool classof(const Node *N) {
    return N->Kind >= NK_Group && N->Kind <= NK_Code;
  }
};

class SymbolRef : public RightHandSide {
  llvm::StringRef Name;
  llvm::StringRef ActualArgs;

public:
  struct {
    bool UseExpect;
    bool AtStart;
  } GenAttr;

  SymbolRef(llvm::SMLoc Loc, llvm::StringRef Name)
      : RightHandSide(NK_SymbolRef, Loc), Name(Name), GenAttr({false, false}) {}

  Symbol *getSymbol() { return llvm::dyn_cast<Symbol>(Inner); }
  Terminal *getTerminal() { return llvm::dyn_cast<Terminal>(Inner); }
  Nonterminal *getNonterminal() { return llvm::dyn_cast<Nonterminal>(Inner); }

  const llvm::StringRef name() const { return Name; }
  const llvm::StringRef actualArgs() const { return ActualArgs; }

  // TODO Can this be moved to the constructor?
  void setActualArgs(llvm::StringRef Args) { ActualArgs = Args; }

  static bool classof(const Node *N) { return N->Kind == NK_SymbolRef; }
};

class MetaSymbol : public RightHandSide {
protected:
  MetaSymbol(NodeKind Kind, llvm::SMLoc Loc) : RightHandSide(Kind, Loc) {}

public:
  static bool classof(const Node *N) {
    return N->Kind >= NK_Group && N->Kind <= NK_Sequence;
  }
};

class Group : public MetaSymbol {
  // Encoding of Cardinalitys is:
  //  .b : lower bound 0 or 1
  //  b. : upper bound 1 or unlimted
  enum { L = 0b01, U = 0b10 };

public:
  enum CardinalityKind {
    One = 0b01,
    OneOrMore = 0b11,
    ZeroOrOne = 0b00,
    ZeroOrMore = 0b10
  };

  const CardinalityKind Cardinality;

  Group(llvm::SMLoc Loc, CardinalityKind Cardinality)
      : MetaSymbol(NK_Group, Loc), Cardinality(Cardinality) {}

  RightHandSide *element() const {
    return llvm::dyn_cast_or_null<RightHandSide>(Link);
  }

  bool isUnlimited() const { return Cardinality & U; }
  bool isOptional() const { return !(Cardinality & L); }
  bool isExactlyOne() const { return Cardinality == One; }

  static bool classof(const Node *N) { return N->Kind == NK_Group; }
};

class Alternative : public MetaSymbol {
public:
  // Attributes for code generation
  struct {
    bool NeedsErrorBranch;
    bool CanUseSwitch;
  } GenAttr;

  Alternative(llvm::SMLoc Loc)
      : MetaSymbol(NK_Alternative, Loc), GenAttr({false, false}) {}

  llvm::iterator_range<NodeLinkIterator<RightHandSide>> alternatives() {
    return llvm::iterator_range<NodeLinkIterator<RightHandSide>>(
        NodeLinkIterator<RightHandSide>(
            llvm::cast_or_null<RightHandSide>(Link)),
        nullptr);
  }

  static bool classof(const Node *N) { return N->Kind == NK_Alternative; }
};

class Sequence : public MetaSymbol {
public:
  Sequence(llvm::SMLoc Loc) : MetaSymbol(NK_Sequence, Loc) {}

  llvm::iterator_range<NodeNextIterator<RightHandSide>> elements() {
    return llvm::iterator_range<NodeNextIterator<RightHandSide>>(
        NodeNextIterator<RightHandSide>(llvm::cast<RightHandSide>(Inner)),
        nullptr);
  }

  static bool classof(const Node *N) { return N->Kind == NK_Sequence; }
};

class Code : public RightHandSide {
public:
  enum CodeType { Normal, Condition, Resolver, Predicate };

  llvm::StringRef Text;
  CodeType Type;

public:
  Code(llvm::SMLoc Loc, llvm::StringRef Text)
      : RightHandSide(NK_Code, Loc), Text(Text), Type(Normal) {}

  static bool classof(const Node *N) { return N->Kind == NK_Code; }
};

class Symbol : public Node {
  llvm::StringRef Name;
  llvm::StringRef ExternalName;

protected:
  Symbol(NodeKind Kind, llvm::SMLoc Loc, llvm::StringRef Name,
         llvm::StringRef ExternalName)
      : Node(Kind, Loc), Name(Name), ExternalName(ExternalName) {}

public:
  const llvm::StringRef name() const { return Name; }
  const llvm::StringRef externalName() const { return ExternalName; }

  static bool classof(const Node *N) {
    return N->Kind >= NK_Terminal && N->Kind <= NK_Nonterminal;
  }
};

class Terminal : public Symbol {
public:
  unsigned No;

  Terminal(llvm::SMLoc Loc, llvm::StringRef Name, llvm::StringRef ExternalName,
           unsigned No)
      : Symbol(NK_Terminal, Loc, Name, ExternalName), No(No) {}

  static bool classof(const Node *N) { return N->Kind == NK_Terminal; }
};

class Nonterminal : public Symbol {
  // Linked list of all nonterminal symbols.
  Nonterminal *NextNT;

  // First element of list of occurances.
  SymbolRef *FirstOccurance;

  llvm::StringRef FormalArgs;

public:
  // Attributes for code generation
  struct {
    bool NeedsErrorHandling;
    unsigned FollowSetIndex;
  } GenAttr;

  Nonterminal(llvm::SMLoc Loc, llvm::StringRef Name)
      : Symbol(NK_Nonterminal, Loc, Name, ""), NextNT(nullptr),
        FirstOccurance(nullptr), GenAttr({false, 0}) {}

  // All nonterminal symbols are in a list.
  Nonterminal *next() const { return NextNT; }
  void linkAtBegin(Nonterminal *&Head) {
    NextNT = Head->NextNT;
    Head = this;
  }
  void linkAtEnd(Nonterminal *&Head, Nonterminal *&Last) {
    if (Last) {
      Last->NextNT = this;
      Last = this;
    } else
      Head = Last = this;
  }

  RightHandSide *getRHS() { return llvm::cast_or_null<RightHandSide>(Link); }

  const llvm::StringRef formalArgs() const { return FormalArgs; }

  // TODO Can this be moved to the constructor?
  void setFormalArgs(llvm::StringRef Args) { FormalArgs = Args; }

  // The occurances of the Nonterminal are in a list.
  void linkOccurance(SymbolRef *Sym) {
    assert(Sym->getNonterminal() == this && "Need NonTerminal to create link");
    Sym->Link = Back;
    Back = Sym;
  }

  llvm::iterator_range<NodeLinkIterator<SymbolRef>> occurances() {
    return llvm::iterator_range<NodeLinkIterator<SymbolRef>>(
        NodeLinkIterator<SymbolRef>(llvm::cast_or_null<SymbolRef>(Back)),
        nullptr);
  }

  static bool classof(const Node *N) { return N->Kind == NK_Nonterminal; }
};

} // namespace lltool
#endif
