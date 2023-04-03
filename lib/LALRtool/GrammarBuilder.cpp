//===--- GrammarBuilder.cpp - LALrtool ast and graph construction -*- C++ -*-=//
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

#include "lalrtool/GrammarBuilder.h"
#include "lalrtool/Diagnostic.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Casting.h"

using namespace lalrtool;

static constexpr llvm::StringRef Accept("$accept");
static constexpr llvm::StringRef End("$end");

GrammarBuilder::GrammarBuilder(Diagnostic &Diag)
    : Diag(Diag), NextRuleID(0), NextTerminalID(0), NextNonterminalID(0) {
  // The following code ensures thet the start symbol, the end-of-input symbol,
  // and the production all have the ID 0.
  Terminal *Eoi = new Terminal(llvm::SMLoc(), End, NextTerminalID++, "");
  Nonterminal *Start =
      new Nonterminal(llvm::SMLoc(), Accept, NextNonterminalID++);
  Rule *StartRule = new Rule(Start, NextRuleID++);
  StartRule->getRHS().push_back(nullptr);
  StartRule->getRHS().push_back(new TerminalRef(llvm::SMLoc(), Eoi));
  Start->setRule(StartRule);
  SymbolNames[End] = Eoi;
  SymbolNames[Accept] = Start;
}

void GrammarBuilder::error(llvm::SMLoc Loc, llvm::Twine Msg) {
  Diag.error(Loc, Msg);
}

void GrammarBuilder::warning(llvm::SMLoc Loc, llvm::Twine Msg) {
  Diag.warning(Loc, Msg);
}

void GrammarBuilder::note(llvm::SMLoc Loc, llvm::Twine Msg) {
  Diag.note(Loc, Msg);
}

void GrammarBuilder::findStartSymbol() {
  Rule *R = llvm::dyn_cast<Nonterminal>(SymbolNames[Accept])->getRule();
  assert(R->getID() == 0 && "Synthetic rule must have ID 0");

  if (!StartName.empty()) {
    Nonterminal *NT = llvm::dyn_cast<Nonterminal>(SymbolNames[StartName]);
    if (NT) {
      UnresolvedNonterminals.emplace_back<Unresolved>(
          {R, 0, NT->getLoc(), NT->getName()});
      return;
    }
    error(StartLoc,
          llvm::Twine("Start symbol ").concat(StartName).concat(" not found."));
  }

  auto Entry = llvm::find_if(SymbolNames,
                             [](const std::pair<llvm::StringRef, Symbol *> &E) {
                               return llvm::isa<Nonterminal>(E.second);
                             });
  if (Nonterminal *NT = llvm::dyn_cast<Nonterminal>(Entry->second)) {
    UnresolvedNonterminals.emplace_back<Unresolved>(
        {R, 0, NT->getLoc(), NT->getName()});
    return;
  }

  error(llvm::SMLoc(), "No start symbol found.");
  return;
}

void GrammarBuilder::resolve() {
  for (auto &U : UnresolvedNonterminals) {
    Symbol *Sym = SymbolNames[U.Name];
    if (Sym == nullptr)
      error(U.Loc, llvm::Twine("Nonterminal ")
                       .concat(U.Name)
                       .concat(" is not declared"));
    else if (!llvm::isa<Nonterminal>(Sym))
      error(U.Loc, llvm::Twine("Expected nonterminal ").concat(U.Name));
    else {
      Nonterminal *NT = llvm::dyn_cast<Nonterminal>(Sym);
      U.R->getRHS()[U.Index] = new NonterminalRef(U.Loc, NT);
    }
  }
}

Grammar GrammarBuilder::build() {
  if (Diag.errorsOccured()) // Bail out if there was a syntax error
    return Grammar();
  findStartSymbol();
  resolve();
  if (Diag.errorsOccured()) // Bail out if there was a syntax error
    return Grammar();
  llvm::SmallVector<Symbol *, 0> Symbols;
  Symbols.reserve(SymbolNames.size());
  llvm::transform(
      SymbolNames, std::back_inserter(Symbols),
      [](const std::pair<llvm::StringRef, Symbol *> &E) { return E.second; });
  Nonterminal *SyntheticStartSymbol =
      llvm::dyn_cast<Nonterminal>(SymbolNames[Accept]);
  Terminal *EoiTerminal = llvm::dyn_cast<Terminal>(SymbolNames[End]);
  Nonterminal *StartSymbol = llvm::dyn_cast<NonterminalRef>(
                                 SyntheticStartSymbol->getRule()->getRHS()[0])
                                 ->getNonterminal();
  return Grammar(StartSymbol, SyntheticStartSymbol, EoiTerminal, Symbols,
                 NextRuleID);
}

Nonterminal *GrammarBuilder::actOnNonterminal(const llvm::SMLoc Loc,
                                              llvm::StringRef Name) {
  Nonterminal *N = new Nonterminal(Loc, Name, NextNonterminalID++);
  SymbolNames[Name] = N;
  return N;
}

Terminal *GrammarBuilder::actOnTerminal(const llvm::SMLoc Loc,
                                        llvm::StringRef Name,
                                        llvm::StringRef ExternalName) {
  Terminal *T = nullptr;
  if (SymbolNames.find(Name) != SymbolNames.end()) {
    error(Loc,
          llvm::Twine("Terminal ").concat(Name).concat(" already declared"));
  } else {
    T = new Terminal(Loc, Name, NextTerminalID++, ExternalName);
    SymbolNames[Name] = T;
  }
  return T;
}

Rule *GrammarBuilder::actOnRule(Nonterminal *NT, Rule *PreviousRule) {
  Rule *R = new Rule(NT, NextRuleID++);
  if (NT->getRule() == nullptr)
    NT->setRule(R);
  if (PreviousRule)
    PreviousRule->setNext(R);
  return R;
}

void GrammarBuilder::actOnSymbolRef(Rule *R, const llvm::SMLoc Loc,
                                    llvm::StringRef Name, bool IsTerminal) {
  Symbol *Sym = nullptr;
  if (auto It = SymbolNames.find(Name); It != SymbolNames.end())
    Sym = It->second;
  if (llvm::isa_and_nonnull<Terminal>(Sym) || IsTerminal) {
    if (Sym == nullptr) {
      Sym = new Terminal(Loc, Name, NextTerminalID++, "");
      SymbolNames[Name] = Sym;
    }
    R->getRHS().push_back(new TerminalRef(Loc, llvm::cast<Terminal>(Sym)));
  } else if (Nonterminal *NT = llvm::dyn_cast_or_null<Nonterminal>(Sym)) {
    R->getRHS().push_back(new NonterminalRef(Loc, NT));
  } else {
    UnresolvedNonterminals.emplace_back<Unresolved>(
        {R, R->getRHS().size(), Loc, Name});
    R->getRHS().push_back(nullptr);
  }
}

void GrammarBuilder::actOnPredicate(Rule *R, const llvm::SMLoc Loc,
                                    llvm::StringRef CodeStr) {
  // Drop { } or {. .} from string
  const size_t Offset = (CodeStr[1] == '.') ? 2 : 1;
  CodeStr = CodeStr.substr(Offset, CodeStr.size() - 2 * Offset).trim();
  R->getRHS().push_back(new Predicate(Loc, CodeStr));
}

void GrammarBuilder::actOnAction(Rule *R, const llvm::SMLoc Loc,
                                 llvm::StringRef CodeStr) {
  // Drop { } or {. .} from string
  const size_t Offset = (CodeStr[1] == '.') ? 2 : 1;
  CodeStr = CodeStr.substr(Offset, CodeStr.size() - 2 * Offset).trim();
  R->getRHS().push_back(new Action(Loc, CodeStr));
}

void GrammarBuilder::actOnStartSymbol(const llvm::SMLoc Loc,
                                      llvm::StringRef Name) {
  if (!StartName.empty()) {
    warning(Loc, "Start symbol is already defined. Ignoring new definition.");
  } else {
    StartLoc = Loc;
    StartName = Name;
  }
}

void GrammarBuilder::actOnEoiSymbol(const llvm::SMLoc Loc,
                                    llvm::StringRef Name) {
  if (!EoiName.empty()) {
    warning(Loc,
            "End-of-input symbol is already defined. Ignoring new definition.");
  } else {
    EoiLoc = Loc;
    EoiName = Name;
  }
}

void GrammarBuilder::actOnLanguage(const llvm::SMLoc Loc,
                                   llvm::StringRef Name) {
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

void GrammarBuilder::actOnDefine(const llvm::SMLoc Loc, llvm::StringRef Name,
                                 llvm::StringRef Value, var::VarType Type) {
  if (Type == var::Code || Type == var::String)
    Value = Value.substr(1, Value.size() - 2);
  if (Type == var::Code)
    Value = Value.trim();
  if (auto Err = Variables.add(Name, Value, Type)) {
    warning(Loc, llvm::toString(std::move(Err)));
  }
}
