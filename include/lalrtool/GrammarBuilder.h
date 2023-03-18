//===--- GrammarBuilder.h - LALRtool ast and graph construction -*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the ast and graph construction grammar.
///
//===----------------------------------------------------------------------===//

#ifndef LALRTOOL_GRAMMARBUILDER_H
#define LALRTOOL_GRAMMARBUILDER_H

#include "lalrtool/Grammar.h"
#include "lalrtool/VarStore.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include <vector>

namespace lalrtool {
class Diagnostic;

class GrammarBuilder {
  Diagnostic &Diag;

  // Mapping from name to Terminal/Nonterminal.
  llvm::MapVector<llvm::StringRef, Symbol *> SymbolNames;

  // Remember forward references to patch them later.
  struct Unresolved {
    Rule *R;
    size_t Index;
    llvm::SMLoc Loc;
    llvm::StringRef Name;
  };
  llvm::SmallVector<Unresolved, 0> UnresolvedNonterminals;

  VarStore Variables;

  unsigned NextRuleID;
  unsigned NextTerminalID;

  llvm::StringRef StartName;
  llvm::SMLoc StartLoc;

  llvm::StringRef EoiName;
  llvm::SMLoc EoiLoc;

  llvm::StringRef LanguageName;
  llvm::SMLoc LanguageLoc;

  void error(llvm::SMLoc Loc, llvm::Twine Msg);
  void warning(llvm::SMLoc Loc, llvm::Twine Msg);
  void note(llvm::SMLoc Loc, llvm::Twine Msg);

  Nonterminal *addSyntheticStart(Nonterminal *StartSymbol,
                                 Terminal *EoiTerminal);
  Nonterminal *findStartSymbol();
  void resolve();

public:
  GrammarBuilder(Diagnostic &Diag)
      : Diag(Diag), NextRuleID(0), NextTerminalID(0) {}
  Grammar build();
  const VarStore &varStore() { return Variables; }

  Nonterminal *actOnNonterminal(const llvm::SMLoc Loc, llvm::StringRef Name);
  Terminal *actOnTerminal(const llvm::SMLoc Loc, llvm::StringRef Name,
                          llvm::StringRef ExternalName = "");
  Rule *actOnRule(Nonterminal *NT, Rule *PreviousRule = nullptr);
  void actOnSymbolRef(Rule *R, const llvm::SMLoc Loc, llvm::StringRef Name,
                      bool IsTerminal = false);
  void actOnPredicate(Rule *R, const llvm::SMLoc Loc, llvm::StringRef Code);
  void actOnAction(Rule *R, const llvm::SMLoc Loc, llvm::StringRef Code);
  void actOnStartSymbol(const llvm::SMLoc Loc, llvm::StringRef Name);
  void actOnEoiSymbol(const llvm::SMLoc Loc, llvm::StringRef Name);
  void actOnLanguage(const llvm::SMLoc Loc, llvm::StringRef Name);
  void actOnDefine(const llvm::SMLoc Loc, llvm::StringRef Name,
                   llvm::StringRef Value, var::VarType Type);
};
} // namespace lalrtool
#endif
