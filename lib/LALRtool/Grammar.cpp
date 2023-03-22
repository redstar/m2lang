//===--- Grammar.cpp - LALRtool grammar definition --------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
///
///
//===----------------------------------------------------------------------===//

#include "lalrtool/Grammar.h"
#include "lalrtool/Algo.h"
#include "lalrtool/Diagnostic.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/raw_ostream.h"

using namespace lalrtool;

void Grammar::performAnalysis(Diagnostic &Diag) {
  if (Diag.errorsOccured())
    return;
  calculateReachable(*this);
  calculateDerivesEpsilon(*this);
  calculateProductive(*this);
  if (Diag.errorsOccured())
    return;
  calculateFirstSets(*this);
  calculateFollowSets(*this);

  writeYAML(llvm::outs());
}

void Grammar::writeYAML(llvm::raw_ostream &OS) const {
  for (Nonterminal *NT : Nonterminals) {
    OS << NT->getName() << ":\n"
       << "  isReachable: " << (NT->isReachable() ? "true" : "false") << "\n"
       << "  isProductive: " << (NT->isProductive() ? "true" : "false") << "\n"
       << "  derivesEpsilon: " << (NT->isDerivesEpsilon() ? "true" : "false")
       << "\n";
    OS << "  first: [";
    bool First = true;
    for (int TID = NT->getFirstSet().find_first(); TID != -1;
         TID = NT->getFirstSet().find_next(TID)) {
      Terminal *T = TerminalMap[TID];
      OS << (First ? " " : ", ") << T->getName();
      First = false;
    }
    OS << " ]\n";
    OS << "  follow: [";
    First = true;
    for (int TID = NT->getFollowSet().find_first(); TID != -1;
         TID = NT->getFollowSet().find_next(TID)) {
      Terminal *T = TerminalMap[TID];
      OS << (First ? " " : ", ") << T->getName();
      First = false;
    }
    OS << " ]\n";
  }
}

#define AST_DEFINITION
#include "lalrtool/lalrtool.ast.inc"