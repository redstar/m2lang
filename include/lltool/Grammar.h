//===--- Grammar.h - LLtool grammar definition ------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the grammar.
///
//===----------------------------------------------------------------------===//

#ifndef LLTOOL_GRAMMAR_H
#define LLTOOL_GRAMMAR_H

#include "lltool/Node.h"
#include "llvm/ADT/IndexedMap.h"
#include "llvm/ADT/iterator_range.h"
#include <vector>

namespace lltool {

class Diagnostic;

class Grammar {
  Nonterminal *StartSymbol;
  Nonterminal *SyntheticStartSymbol;
  Terminal *EoiTerminal;
  std::vector<Node *> Nodes;
  llvm::IndexedMap<Terminal *> TerminalMap;

public:
  using range_type = llvm::iterator_range<std::vector<Node *>::iterator>;

  Grammar()
      : StartSymbol(nullptr), SyntheticStartSymbol(nullptr),
        EoiTerminal(nullptr), Nodes(), TerminalMap() {}
  Grammar(Nonterminal *StartSymbol, Nonterminal *SyntheticStartSymbol,
          Terminal *EoiTerminal, std::vector<Node *> &Nodes,
          llvm::IndexedMap<Terminal *> &TerminalMap)
      : StartSymbol(StartSymbol), SyntheticStartSymbol(SyntheticStartSymbol),
        EoiTerminal(EoiTerminal), Nodes(Nodes), TerminalMap(TerminalMap) {}

  Nonterminal *startSymbol() const { return StartSymbol; }
  Nonterminal *syntheticStartSymbol() const { return SyntheticStartSymbol; }
  Terminal *eoiTerminal() const { return EoiTerminal; }
  const std::vector<Node *> &nodes() const { return Nodes; }
  llvm::iterator_range<std::vector<Node *>::iterator> nodeRange() {
    return llvm::make_range(Nodes.begin(), Nodes.end());
  }

  Terminal *map(unsigned N) const{ return TerminalMap[N]; }
  unsigned numberOfTerminals() const {
    return static_cast<unsigned>(TerminalMap.size());
  }

  void performAnalysis(Diagnostic &Diag);
};
} // namespace lltool
#endif
