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
#define LLTOOL_GRAMMARBUILD_H

#include "Diagnostic.h"
#include "Grammar.h"
#include "VarStore.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include <vector>

namespace lltool {

class GrammarBuilder {
  Diagnostic &Diag;
  std::vector<Node *> nodes;
  llvm::StringMap<Terminal *> terminals;
  VarStore variables;

  unsigned NextTerminalNo;

  llvm::StringRef startName;
  llvm::SMLoc startLoc;

  llvm::StringRef eoiName;
  llvm::SMLoc eoiLoc;

  void error(llvm::SMLoc loc, llvm::Twine msg);
  void warning(llvm::SMLoc loc, llvm::Twine msg);
  void note(llvm::SMLoc loc, llvm::Twine msg);

  Nonterminal *addSyntheticStart(Nonterminal *startSymbol,
                                 Terminal *eoiTerminal);
  Nonterminal *findStartSymbol();
  void resolve();

public:
  GrammarBuilder(Diagnostic &Diag) : Diag(Diag), NextTerminalNo(0) {}
  Grammar build();
  const VarStore &varStore() { return variables; }
  Nonterminal *nonterminal(const llvm::SMLoc loc, llvm::StringRef name);
  Terminal *terminal(const llvm::SMLoc loc, llvm::StringRef name,
                     llvm::StringRef externalName = "");
  Symbol *symbol(const llvm::SMLoc loc, llvm::StringRef name,
                 bool isTerminal = false);
  Code *code(const llvm::SMLoc loc, llvm::StringRef code);
  Sequence *sequence(const llvm::SMLoc loc);
  Group *group(const llvm::SMLoc loc, Group::CardinalityKind Cardinality);
  Alternative *alternative(const llvm::SMLoc loc, Node *seq);
  void argument(Node *Node, llvm::StringRef arg);
  void startSymbol(const llvm::SMLoc loc, llvm::StringRef name);
  void eoiSymbol(const llvm::SMLoc loc, llvm::StringRef name);
  void language(const llvm::SMLoc loc, llvm::StringRef name);
  void define(const llvm::SMLoc loc, llvm::StringRef name,
              llvm::StringRef value, var::VarType type);
};
} // namespace lltool
#endif
