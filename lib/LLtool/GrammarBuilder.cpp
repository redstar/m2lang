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

#include "lltool/GrammarBuilder.h"

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

Nonterminal *GrammarBuilder::addSyntheticStart(Nonterminal *StartSymbol,
                                               Terminal *EoiTerminal) {
  // The following adds a synthetic rule "" = <Symbol> "_eof" .
  // Create start node. This is always the first node in array.
  Nonterminal *Start = nonterminal(llvm::SMLoc(), "");
  Node *N = symbol(llvm::SMLoc(), StartSymbol->name());
  N->Inner = StartSymbol;
  Start->Link = sequence(llvm::SMLoc());
  Start->Link->Inner = N;
  Start->Link->Back = Start;
  N->Next = symbol(llvm::SMLoc(), EoiTerminal->name());
  N->Next->Back = Start->Link;
  return Start;
}

Nonterminal *GrammarBuilder::findStartSymbol() {
  if (!StartName.empty()) {
    for (auto *N : llvm::make_filter_range(
             Nodes, [](Node *N) { return N->Kind == Node::NK_Nonterminal; })) {
      Nonterminal *NT = llvm::cast<Nonterminal>(N);
      if (NT->name() == StartName)
        return NT;
    }
    error(StartLoc,
          llvm::Twine("Start symbol ").concat(StartName).concat(" not found."));
  } else {
    // Return first non-terminal in vector.
    for (auto *N : llvm::make_filter_range(
             Nodes, [](Node *N) { return llvm::isa<Nonterminal>(N); })) {
      return llvm::cast<Nonterminal>(N);
    }
    error(llvm::SMLoc(), "No start symbol found.");
  }
  return nullptr;
}

void GrammarBuilder::resolve() {
  llvm::StringMap<Nonterminal *> NamesOfNonterminals;
  for (Node *N : llvm::make_filter_range(
           Nodes, [](Node *N) { return llvm::isa<Nonterminal>(N); })) {
    Nonterminal *NT = llvm::cast<Nonterminal>(N);
    Nonterminal *Other = NamesOfNonterminals.lookup(NT->name());
    if (Other) {
      error(NT->Loc, llvm::Twine("Duplicate nontermial ").concat(NT->name()));
      note(Other->Loc,
           llvm::Twine("First definition of nontermial ").concat(Other->name()));
    } else
      NamesOfNonterminals[NT->name()] = NT;
  }

  for (Node *N : llvm::make_filter_range(
           Nodes, [](Node *N) { return llvm::isa<SymbolRef>(N); })) {
    SymbolRef *Sym = llvm::cast<SymbolRef>(N);
    if (!N->Inner) {
      if (auto *V = NamesOfNonterminals.lookup(Sym->name()))
        N->Inner = V;
      else if (auto *T = Terminals.lookup(Sym->name()))
        N->Inner = T;
      else {
        error(Sym->Loc, llvm::Twine("Missing definition of nonterminal ")
                            .concat(Sym->name()));
        continue;
      }
    }

    // Link nonterminal node to chain of occurances.
    if (Nonterminal *NT = Sym->getNonterminal())
      NT->linkOccurance(Sym);
  }
}

Grammar GrammarBuilder::build() {
  if (Diag.errorsOccured()) // Bail out if there was a syntax error
    return Grammar();
  Terminal *EoiTerminal = terminal(llvm::SMLoc(), "_eoi", EoiName);
  Nonterminal *StartSymbol = findStartSymbol();
  Nonterminal *SyntheticStartSymbol =
      addSyntheticStart(StartSymbol, EoiTerminal);
  resolve();
  if (Diag.errorsOccured()) // Bail out if there was a syntax error
    return Grammar();
  llvm::IndexedMap<Terminal *> TerminalMap;
  TerminalMap.resize(NextTerminalNo + 1);
  for (auto *N : llvm::make_filter_range(
           Nodes, [](Node *N) { return llvm::isa<Terminal>(N); })) {
    Terminal *T = llvm::cast<Terminal>(N);
    TerminalMap[T->No] = T;
  }
  return Grammar(Nonterminals, StartSymbol, SyntheticStartSymbol, EoiTerminal,
                 Nodes, TerminalMap);
}

Nonterminal *GrammarBuilder::nonterminal(const llvm::SMLoc Loc,
                                         llvm::StringRef Name) {
  Nonterminal *N = new Nonterminal(Loc, Name);
  N->linkAtEnd(Nonterminals, LastNT);
  Nodes.push_back(N);
  return N;
}

Terminal *GrammarBuilder::terminal(const llvm::SMLoc Loc, llvm::StringRef Name,
                                   llvm::StringRef ExternalName) {
  if (Terminals.find(Name) != Terminals.end()) {
    error(Loc,
          llvm::Twine("Terminal ").concat(Name).concat(" already declared"));
    return nullptr;
  } else {
    Terminal *T = new Terminal(Loc, Name, ExternalName, NextTerminalNo++);
    Nodes.push_back(T);
    Terminals[Name] = T;
    return T;
  }

  return nullptr;
}

SymbolRef *GrammarBuilder::symbol(const llvm::SMLoc Loc, llvm::StringRef Name,
                                  bool IsTerminal) {
  SymbolRef *N = new SymbolRef(Loc, Name);
  Nodes.push_back(N);
  if (IsTerminal) {
    if (Node *T = Terminals.lookup(Name))
      N->Inner = T;
    else
      N->Inner = terminal(Loc, Name);
  }
  return N;
}

Code *GrammarBuilder::code(const llvm::SMLoc Loc, llvm::StringRef CodeStr) {
  // Drop { } or {. .} from string
  const size_t Offset = (CodeStr[1] == '.') ? 2 : 1;
  CodeStr = CodeStr.substr(Offset, CodeStr.size() - 2 * Offset).trim();
  Code *N = new Code(Loc, CodeStr);
  Nodes.push_back(N);
  return N;
}

Sequence *GrammarBuilder::sequence(const llvm::SMLoc Loc) {
  Sequence *N = new Sequence(Loc);
  Nodes.push_back(N);
  return N;
}

Group *GrammarBuilder::group(const llvm::SMLoc Loc,
                             Group::CardinalityKind Cardinality) {
  Group *N = new Group(Loc, Cardinality);
  Nodes.push_back(N);
  return N;
}

Alternative *GrammarBuilder::alternative(const llvm::SMLoc Loc, Node *Seq) {
  assert(Seq->Kind == Node::NK_Sequence && "Alternative needs sequence");
  Alternative *N = new Alternative(Loc);
  Nodes.push_back(N);
  N->Link = Seq;
  return N;
}

void GrammarBuilder::argument(Node *Node, llvm::StringRef Arg) {
  // Drop < > or <. .> from string
  const size_t Offset = Arg[1] == '.' ? 2 : 1;
  Arg = Arg.substr(Offset, Arg.size() - 2 * Offset).trim();
  if (auto *NT = llvm::dyn_cast<Nonterminal>(Node))
    NT->setFormalArgs(Arg) ;
  else if (auto *Sym = llvm::dyn_cast<SymbolRef>(Node))
    Sym->setActualArgs(Arg);
  else
    llvm_unreachable("Node neither Nonterminal not Symbol");
}

void GrammarBuilder::startSymbol(const llvm::SMLoc Loc, llvm::StringRef Name) {
  if (!StartName.empty()) {
    warning(Loc, "Start symbol is already defined. Ignoring new definition.");
  } else {
    StartLoc = Loc;
    StartName = Name;
  }
}

void GrammarBuilder::eoiSymbol(const llvm::SMLoc Loc, llvm::StringRef Name) {
  if (!EoiName.empty()) {
    warning(Loc,
            "End-of-input symbol is already defined. Ignoring new definition.");
  } else {
    EoiLoc = Loc;
    EoiName = Name;
  }
}

void GrammarBuilder::language(const llvm::SMLoc Loc, llvm::StringRef Name) {
  if (!LanguageName.empty()) {
    warning(Loc, "Language is already defined. Ignoring new definition.");
    note(LanguageLoc, "Previous definition");
  } else {
    std::string Lang = Name.substr(1, Name.size() - 2).lower();
    if (Lang != "c++") {
      warning(Loc, llvm::Twine("Unknonw language ")
                       .concat(Lang)
                       .concat(". Ignoring definition."));
      note(Loc, "Valid values are: c++");
    } else {
      LanguageName = "c++";
      LanguageLoc = Loc;
    }
  }
}

void GrammarBuilder::define(const llvm::SMLoc Loc, llvm::StringRef Name,
                            llvm::StringRef Value, var::VarType Type) {
  if (Type == var::Code || Type == var::String)
    Value = Value.substr(1, Value.size() - 2);
  if (Type == var::Code)
    Value = Value.trim();
  if (auto Err = Variables.add(Name, Value, Type)) {
    warning(Loc, llvm::toString(std::move(Err)));
  }
}
