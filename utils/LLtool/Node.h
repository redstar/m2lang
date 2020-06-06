//===--- Node.h - LLtool node definition ------------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the definition of a graph node.
///
//===----------------------------------------------------------------------===//

#ifndef LLTOOL_NODE_H
#define LLTOOL_NODE_H

#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"

namespace lltool {
using FirstSetType = llvm::BitVector;
using FollowSetType = llvm::BitVector;

class Node {
public:
  enum NodeKind {
    NK_Terminal,
    NK_Nonterminal,
    NK_Group,
    NK_Alternative,
    NK_Sequence,
    NK_Symbol,
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
};

class Terminal : public Node {
public:
  llvm::StringRef Name;
  llvm::StringRef ExternalName;
  unsigned No;

  Terminal(llvm::SMLoc Loc, llvm::StringRef Name, llvm::StringRef ExternalName,
           unsigned No)
      : Node(NK_Terminal, Loc), Name(Name), ExternalName(ExternalName), No(No) {
  }

  llvm::StringRef name() { return Name; }
  llvm::StringRef externalName() { return ExternalName; }

  static bool classof(const Node *N) { return N->Kind == NK_Terminal; }
};

class Nonterminal : public Node {
public:
  llvm::StringRef Name;
  llvm::StringRef ExternalName;
  llvm::StringRef FormalArgs;

  // Attributes for code generation
  struct {
    bool NeedsErrorHandling;
    unsigned FollowSetIndex;
  } GenAttr;

  Nonterminal(llvm::SMLoc Loc, llvm::StringRef Name)
      : Node(NK_Nonterminal, Loc), Name(Name), GenAttr({false, 0}) {}

  static bool classof(const Node *N) { return N->Kind == NK_Nonterminal; }
};

class Symbol : public Node {
public:
  struct {
    bool UseExpect;
    bool AtStart;
  } GenAttr;

  llvm::StringRef Name;
  llvm::StringRef ActualArgs;

  Symbol(llvm::SMLoc Loc, llvm::StringRef Name)
      : Node(NK_Symbol, Loc), Name(Name), GenAttr({false, false}) {}

  static bool classof(const Node *N) { return N->Kind == NK_Symbol; }
};

class Group : public Node {
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
      : Node(NK_Group, Loc), Cardinality(Cardinality) {}

  bool isUnlimited() { return Cardinality & U; }
  bool isOptional() { return !(Cardinality & L); }

  static bool classof(const Node *N) { return N->Kind == NK_Group; }
};

class Alternative : public Node {
public:
  // Attributes for code generation
  struct {
    bool NeedsErrorBranch;
    bool CanUseSwitch;
  } GenAttr;

  Alternative(llvm::SMLoc Loc)
      : Node(NK_Alternative, Loc), GenAttr({false, false}) {}

  static bool classof(const Node *N) { return N->Kind == NK_Alternative; }
};

class Sequence : public Node {
public:
  Sequence(llvm::SMLoc Loc) : Node(NK_Sequence, Loc) {}

  static bool classof(const Node *N) { return N->Kind == NK_Sequence; }
};

class Code : public Node {
public:
  enum CodeType { Normal, Condition, Resolver, Predicate };

  llvm::StringRef Text;
  CodeType Type;

public:
  Code(llvm::SMLoc Loc, llvm::StringRef Text)
      : Node(NK_Code, Loc), Text(Text), Type(Normal) {}

  static bool classof(const Node *N) { return N->Kind == NK_Code; }
};
} // namespace lltool
#endif
