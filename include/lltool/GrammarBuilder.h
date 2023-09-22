//===--- GrammarBuilder.h - LLtool ast and graph construction ---*- C++ -*-===//
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

#ifndef LLTOOL_GRAMMARBUILDER_H
#define LLTOOL_GRAMMARBUILDER_H

#include "lltool/Diagnostic.h"
#include "lltool/Grammar.h"
#include "lltool/VarStore.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include <vector>

namespace lltool {

class GrammarBuilder {
  Diagnostic &Diag;
  std::vector<Node *> Nodes;
  llvm::StringMap<Terminal *> Terminals;
  VarStore Variables;

  // List of all nonterminal symbols, in same order as in source file.
  Nonterminal *Nonterminals;

  // Last inserted nonterminal symbol (for list construction).
  Nonterminal *LastNT;

  // Number of next terminal symbol.
  unsigned NextTerminalNo;

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
      : Diag(Diag), Nonterminals(nullptr), LastNT(nullptr), NextTerminalNo(0) {}
  Grammar build();
  const VarStore &varStore() { return Variables; }
  Nonterminal *nonterminal(const llvm::SMLoc Loc, llvm::StringRef Name);
  Terminal *terminal(const llvm::SMLoc Loc, llvm::StringRef Name,
                     llvm::StringRef ExternalName = "");
  SymbolRef *symbol(const llvm::SMLoc Loc, llvm::StringRef Name,
                    bool IsTerminal = false);
  Code *code(const llvm::SMLoc Loc, llvm::StringRef Code);
  Sequence *sequence(const llvm::SMLoc Loc);
  Group *group(const llvm::SMLoc Loc, Group::CardinalityKind Cardinality);
  Alternative *alternative(const llvm::SMLoc Loc, Node *Seq);
  void argument(Node *Node, llvm::StringRef Arg);
  void startSymbol(const llvm::SMLoc Loc, llvm::StringRef Name);
  void eoiSymbol(const llvm::SMLoc Loc, llvm::StringRef Name);
  void language(const llvm::SMLoc Loc, llvm::StringRef Name);
  void define(const llvm::SMLoc Loc, llvm::StringRef Name,
              llvm::StringRef Value, var::VarType Type);
};
} // namespace lltool
#endif
