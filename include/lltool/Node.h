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
/// The right-hand side of the grammar consists of Metada, SymbolRef, or Code.
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

using FirstSetType = llvm::BitVector;
using FollowSetType = llvm::BitVector;

template <class T>
class NodeIterator
    : public llvm::iterator_facade_base<NodeIterator<T>,
                                        std::forward_iterator_tag, T *> {
  T *Ptr;

public:
  NodeIterator(T *Ptr) : Ptr(Ptr) {}

  // Either define both copy constructor && copy assignment or use default.

  bool operator==(const NodeIterator<T> &Iter) const { return Ptr == Iter.Ptr; }

  T *operator*() const { return Ptr; }

  NodeIterator<T> &operator++() {
    if (Ptr)
      Ptr = Ptr->getNext();
    return *this;
  }
};

template <class T>
class NodeLinkIterator
    : public llvm::iterator_facade_base<NodeLinkIterator<T>,
                                        std::forward_iterator_tag, T *> {
  T *Ptr;

public:
  NodeLinkIterator(T *Ptr) : Ptr(Ptr) {}

  // Either define both copy constructor && copy assignment or use default.

  bool operator==(const NodeLinkIterator<T> &Iter) const {
    return Ptr == Iter.Ptr;
  }

  T *operator*() const { return Ptr; }

  NodeLinkIterator<T> &operator++() {
    if (Ptr)
      Ptr = llvm::cast_or_null<T>(Ptr->Link);
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

  bool IsReachable;
  bool DerivesEpsilon;
  bool IsProductive;

  // True if there is a LL(1) conflict.
  bool HasConflict;

  FirstSetType FirstSet;
  FollowSetType FollowSet;

  Node(NodeKind K, llvm::SMLoc Loc)
      : Loc(Loc), Next(nullptr), Link(nullptr), Inner(nullptr), Back(nullptr),
        Kind(K), IsReachable(false), DerivesEpsilon(false), IsProductive(false),
        HasConflict(false) {}

  void setReachable(bool V = true) { IsReachable = V; }
  bool isReachable() { return IsReachable; }

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

  Node *getNext() { return Next; }
};

class Symbol : public Node {
public:
  llvm::StringRef Name;
  llvm::StringRef ExternalName;

protected:
  Symbol(NodeKind Kind, llvm::SMLoc Loc, llvm::StringRef Name,
         llvm::StringRef ExternalName)
      : Node(Kind, Loc), Name(Name), ExternalName(ExternalName) {}

public:
  llvm::StringRef name() { return Name; }
  llvm::StringRef externalName() { return ExternalName; }

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

public:
  llvm::StringRef FormalArgs;

  // Attributes for code generation
  struct {
    bool NeedsErrorHandling;
    unsigned FollowSetIndex;
  } GenAttr;

  Nonterminal(llvm::SMLoc Loc, llvm::StringRef Name)
      : Symbol(NK_Nonterminal, Loc, Name, ""), NextNT(nullptr),
        GenAttr({false, 0}) {}

  // All nonterminal symbols are in a list.
  void setNext(Nonterminal *NT) { NextNT = NT; }
  Nonterminal *getNext() { return NextNT; }

  RightHandSide *getRHS() { return llvm::cast_or_null<RightHandSide>(Link); }

  static bool classof(const Node *N) { return N->Kind == NK_Nonterminal; }
};

class RightHandSide : public Node {
public:
protected:
  RightHandSide(NodeKind Kind, llvm::SMLoc Loc) : Node(Kind, Loc) {}

public:
  // All right-hand side elements are in a list.
  void setNext(RightHandSide *RHS) { Next = RHS; }
  RightHandSide *getNext() { return llvm::cast_or_null<RightHandSide>(Next); }

  static bool classof(const Node *N) {
    return N->Kind >= NK_Group && N->Kind <= NK_Code;
  }
};

class SymbolRef : public RightHandSide {
public:
  llvm::StringRef Name;
  llvm::StringRef ActualArgs;

  struct {
    bool UseExpect;
    bool AtStart;
  } GenAttr;

  SymbolRef(llvm::SMLoc Loc, llvm::StringRef Name)
      : RightHandSide(NK_SymbolRef, Loc), Name(Name), GenAttr({false, false}) {}

  Symbol *getSymbol() { return llvm::dyn_cast<Symbol>(Inner); }
  Terminal *getTerminal() { return llvm::dyn_cast<Terminal>(Inner); }
  Nonterminal *getNonterminal() { return llvm::dyn_cast<Nonterminal>(Inner); }

  static bool classof(const Node *N) { return N->Kind == NK_SymbolRef; }
};

class Metadata : public RightHandSide {
public:
protected:
  Metadata(NodeKind Kind, llvm::SMLoc Loc) : RightHandSide(Kind, Loc) {}

public:
  static bool classof(const Node *N) {
    return N->Kind >= NK_Group && N->Kind <= NK_Sequence;
  }
};

class Group : public Metadata {
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

  CardinalityKind Cardinality;

  Group(llvm::SMLoc Loc, CardinalityKind Cardinality)
      : Metadata(NK_Group, Loc), Cardinality(Cardinality) {}

  bool isUnlimited() { return Cardinality & U; }
  bool isOptional() { return !(Cardinality & L); }

  static bool classof(const Node *N) { return N->Kind == NK_Group; }
};

class Alternative : public Metadata {
public:
  // Attributes for code generation
  struct {
    bool NeedsErrorBranch;
    bool CanUseSwitch;
  } GenAttr;

  Alternative(llvm::SMLoc Loc)
      : Metadata(NK_Alternative, Loc), GenAttr({false, false}) {}

  llvm::iterator_range<NodeLinkIterator<RightHandSide>> alternatives() {
    return llvm::iterator_range<NodeLinkIterator<RightHandSide>>(
        NodeLinkIterator<RightHandSide>(
            llvm::cast_or_null<RightHandSide>(Inner)),
        nullptr);
  }

  static bool classof(const Node *N) { return N->Kind == NK_Alternative; }
};

class Sequence : public Metadata {
public:
  Sequence(llvm::SMLoc Loc) : Metadata(NK_Sequence, Loc) {}

  llvm::iterator_range<NodeIterator<RightHandSide>> elements() {
    return llvm::iterator_range<NodeIterator<RightHandSide>>(
        NodeIterator<RightHandSide>(llvm::cast<RightHandSide>(Inner)), nullptr);
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

} // namespace lltool
#endif
