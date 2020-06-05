//===--- GrammarBuilder.cpp - LLtool ast and graph construction -*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the ast and graph construction.
///
//===----------------------------------------------------------------------===//

#include "GrammarBuilder.h"

using namespace lltool;

void GrammarBuilder::error(llvm::SMLoc Loc, llvm::Twine Msg) {
  Diag.error(Loc, Msg);
}

void GrammarBuilder::warning(llvm::SMLoc Loc, llvm::Twine Msg) {
  Diag.warning(Loc, Msg);
}

void GrammarBuilder::note(llvm::SMLoc Loc, llvm::Twine Msg) {
  Diag.note(Loc, Msg);
}

Nonterminal *GrammarBuilder::addSyntheticStart(Nonterminal *startSymbol,
                                               Terminal *eoiTerminal) {
  // The following adds a synthetic rule "" = <Symbol> "_eof" .
  // Create start node. This is always the first node in array.
  Nonterminal *start = nonterminal(llvm::SMLoc(), "");
  Node *node = symbol(llvm::SMLoc(), startSymbol->Name);
  node->Inner = startSymbol;
  start->Link = sequence(llvm::SMLoc());
  start->Link->Inner = node;
  start->Link->Back = start;
  node->Next = symbol(llvm::SMLoc(), eoiTerminal->Name);
  node->Next->Back = start->Link;
  return start;
}

Nonterminal *GrammarBuilder::findStartSymbol() {
  if (!startName.empty()) {
    for (auto n : llvm::make_filter_range(
             nodes, [](Node *n) { return n->Kind == Node::NK_Nonterminal; })) {
      Nonterminal *nt = llvm::cast<Nonterminal>(n);
      if (nt->Name == startName)
        return nt;
    }
    error(startLoc,
          llvm::Twine("Start symbol ").concat(startName).concat(" not found."));
  } else {
    // Return first non-terminal in vector.
    for (auto n : llvm::make_filter_range(
             nodes, [](Node *n) { return llvm::isa<Nonterminal>(n); })) {
      return llvm::cast<Nonterminal>(n);
    }
    error(llvm::SMLoc(), "No start symbol found.");
  }
  return nullptr;
}

void GrammarBuilder::resolve() {
  llvm::StringMap<Nonterminal *> namesOfNonterminals;
  for (Node *n : llvm::make_filter_range(
           nodes, [](Node *n) { return llvm::isa<Nonterminal>(n); })) {
    Nonterminal *nt = llvm::cast<Nonterminal>(n);
    Nonterminal *other = namesOfNonterminals.lookup(nt->Name);
    if (other) {
      error(nt->Loc, llvm::Twine("Duplicate nontermial ").concat(nt->Name));
      note(other->Loc,
           llvm::Twine("First definition of nontermial ").concat(other->Name));
    } else
      namesOfNonterminals[nt->Name] = nt;
  }

  for (Node *n : llvm::make_filter_range(
           nodes, [](Node *n) { return llvm::isa<Symbol>(n); })) {
    if (!n->Inner) {
      Symbol *sym = llvm::cast<Symbol>(n);
      if (auto v = namesOfNonterminals.lookup(sym->Name))
        n->Inner = v;
      else if (auto t = terminals.lookup(sym->Name))
        n->Inner = t;
      else {
        error(sym->Loc, llvm::Twine("Missing definition of nonterminal ")
                            .concat(sym->Name));
        continue;
      }
    }

    // Link node to chain of occurances
    if (llvm::isa<Nonterminal>(n->Inner)) {
      n->Link = n->Inner->Back;
      n->Inner->Back = n;
    }
  }
}

Grammar GrammarBuilder::build() {
  if (Diag.errorsOccured()) // Bail out if there was a syntax error
    return Grammar();
  Terminal *eoiTerminal = terminal(llvm::SMLoc(), "_eoi", eoiName);
  Nonterminal *startSymbol = findStartSymbol();
  Nonterminal *syntheticStartSymbol =
      addSyntheticStart(startSymbol, eoiTerminal);
  resolve();
  if (Diag.errorsOccured()) // Bail out if there was a syntax error
    return Grammar();
  // foreach (n; nodes) n.check;
  llvm::IndexedMap<Terminal *> terminalMap;
  terminalMap.resize(NextTerminalNo+1);
  for (auto n : llvm::make_filter_range(
           nodes, [](Node *n) { return llvm::isa<Terminal>(n); })) {
    Terminal *T = llvm::cast<Terminal>(n);
    terminalMap[T->No] = T;
  }
  return Grammar(startSymbol, syntheticStartSymbol, eoiTerminal, nodes,
                 terminalMap);
}

Nonterminal *GrammarBuilder::nonterminal(const llvm::SMLoc Loc,
                                         llvm::StringRef name) {
  Nonterminal *node = new Nonterminal(Loc, name);
  nodes.push_back(node);
  return node;
}

Terminal *GrammarBuilder::terminal(const llvm::SMLoc Loc, llvm::StringRef name,
                                   llvm::StringRef externalName) {
  if (terminals.find(name) != terminals.end()) {
    error(Loc,
          llvm::Twine("Terminal ").concat(name).concat(" already declared"));
    return nullptr;
  } else {
    Terminal *node = new Terminal(Loc, name, externalName, NextTerminalNo++);
    nodes.push_back(node);
    terminals[name] = node;
    return node;
  }

  return nullptr;
}

Symbol *GrammarBuilder::symbol(const llvm::SMLoc Loc, llvm::StringRef name,
                               bool isTerminal) {
  Symbol *node = new Symbol(Loc, name);
  nodes.push_back(node);
  if (isTerminal) {
    if (Node *t = terminals.lookup(name))
      node->Inner = t;
    else
      node->Inner = terminal(Loc, name);
  }
  return node;
}

Code *GrammarBuilder::code(const llvm::SMLoc Loc, llvm::StringRef code) {
  // Drop { } or {. .} from string
  const size_t ofs = (code[1] == '.') ? 2 : 1;
  code = code.substr(ofs, code.size() - 2*ofs).trim();
  Code *node = new Code(Loc, code);
  nodes.push_back(node);
  return node;
}

Sequence *GrammarBuilder::sequence(const llvm::SMLoc Loc) {
  Sequence *node = new Sequence(Loc);
  nodes.push_back(node);
  return node;
}

Group *GrammarBuilder::group(const llvm::SMLoc Loc,
                             Group::CardinalityKind Cardinality) {
  Group *node = new Group(Loc, Cardinality);
  nodes.push_back(node);
  return node;
}

Alternative *GrammarBuilder::alternative(const llvm::SMLoc Loc, Node *seq) {
  assert(seq->Kind == Node::NK_Sequence && "Alternative needs sequence");
  Alternative *node = new Alternative(Loc);
  nodes.push_back(node);
  node->Link = seq;
  return node;
}

void GrammarBuilder::argument(Node *Node, llvm::StringRef arg) {
  // Drop < > or <. .> from string
  const size_t ofs = arg[1] == '.' ? 2 : 1;
  arg = arg.substr(ofs, arg.size() - 2*ofs).trim();
  if (auto *NT = llvm::dyn_cast<Nonterminal>(Node))
    NT->FormalArgs = arg;
  else if (auto *Sym = llvm::dyn_cast<Symbol>(Node))
    Sym->ActualArgs = arg;
  else
    llvm_unreachable("Node neither Nonterminal not Symbol");
}

void GrammarBuilder::startSymbol(const llvm::SMLoc loc, llvm::StringRef name) {
  if (!startName.empty()) {
    warning(loc, "Start symbol is already defined. Ignoring new definition.");
  } else {
    startLoc = loc;
    startName = name;
  }
}

void GrammarBuilder::eoiSymbol(const llvm::SMLoc loc, llvm::StringRef name) {
  if (!eoiName.empty()) {
    warning(loc,
            "End-of-input symbol is already defined. Ignoring new definition.");
  } else {
    eoiLoc = loc;
    eoiName = name;
  }
}

void GrammarBuilder::language(const llvm::SMLoc loc, llvm::StringRef name) {
  if (!variables.getVar(var::Language).empty()) {
    warning(loc, "Language is already defined. Ignoring new definition.");
  } else {
    std::string lang = name.substr(1, name.size() - 2).lower();
    if (lang != "c++") {
      warning(loc, llvm::Twine("Unknonw language ")
                       .concat(lang)
                       .concat(". Ignoring definition."));
      note(loc, "Valid values are: c++");
    } else
      variables.set(var::Language, llvm::StringRef(lang));
  }
}

void GrammarBuilder::define(const llvm::SMLoc loc, llvm::StringRef name,
                            llvm::StringRef value, var::VarType type) {
  if (type == var::Code || type == var::String)
    value = value.substr(1, value.size()-2);
  if (type == var::Code)
    value = value.trim();
  if (auto Err = variables.add(name, value, type)) {
    warning(loc, llvm::toString(std::move(Err)));
  }
}
