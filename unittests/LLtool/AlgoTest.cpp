//===- unittests/tools/LLtool//FirstSetTest.cpp --- LLtool Algo tests -----===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "lltool/Algo.h"
#include "lltool/Diagnostic.h"
#include "lltool/Parser.h"
#include "lltool/VarStore.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "gtest/gtest.h"

namespace {

using namespace lltool;

TEST(AlgoTest, reachable1Test) {
  // Wilhelm, Maurer; p. 298
  // Nonterminals U, V are not reachable.
  llvm::StringRef Input = "%token a, b, c, d\n"
                          "%%\n"
                          "S : Y ;\n"
                          "Y : Y Z | Y a | b ;\n"
                          "U : V ;\n"
                          "X : c ;\n"
                          "V : V d | d ;\n"
                          "Z : Z X ;\n";

  llvm::SourceMgr SrcMgr;
  Diagnostic Diag(SrcMgr);
  std::unique_ptr<llvm::MemoryBuffer> Content =
      llvm::MemoryBuffer::getMemBuffer(Input);
  SrcMgr.AddNewSourceBuffer(std::move(Content), llvm::SMLoc());
  Grammar G;
  VarStore Vars;
  Parser TheParser(SrcMgr);
  TheParser.parse(G, Vars);
  calculateReachable(G);
  for (Node *N : G.nodes()) {
    if (auto *NT = llvm::dyn_cast<Nonterminal>(N)) {
      ASSERT_EQ(NT->isReachable(), NT->name() != "U" && NT->name() != "V");
    }
  }
}

TEST(AlgoTest, derivesEpsilon1Test) {
  // Wilhelm, Maurer; p. 311
  // Nonterminals Eq, Tq derives epsilon.
  llvm::StringRef Input = "%token id\n"
                          "%%\n"
                          "S : E ;\n"
                          "E : T Eq ;\n"
                          "Eq : ( '+' E )? ;\n"
                          "T : F Tq ;\n"
                          "Tq : ( '*' T )? ;\n"
                          "F : id | '(' E ')' ;\n";

  llvm::SourceMgr SrcMgr;
  Diagnostic Diag(SrcMgr);
  std::unique_ptr<llvm::MemoryBuffer> Content =
      llvm::MemoryBuffer::getMemBuffer(Input);
  SrcMgr.AddNewSourceBuffer(std::move(Content), llvm::SMLoc());
  Grammar G;
  VarStore Vars;
  Parser TheParser(SrcMgr);
  TheParser.parse(G, Vars);
  calculateDerivesEpsilon(G);
  for (Node *N : G.nodes()) {
    if (auto *NT = llvm::dyn_cast<Nonterminal>(N)) {
      ASSERT_EQ(NT->derivesEpsilon(), !(NT->name() != "Eq" && NT->name() != "Tq"));
    }
    if (auto *G = llvm::dyn_cast<Group>(N)) {
      ASSERT_EQ(G->derivesEpsilon(), G->isOptional());
    }
  }
}

TEST(AlgoTest, derivesEpsilon2Test) {
  // See https://www.codewars.com/kata/compute-nullable-non-terminals
  llvm::StringRef Input = "A : 'a' B | 'c' C ;\n"
                          "B : ( A B )? ;\n"
                          "C : 'b' 'c' ;\n"
                          "D : ( 'a' 'b' )? ;\n"
                          "E : B D ;\n";

  llvm::SourceMgr SrcMgr;
  Diagnostic Diag(SrcMgr);
  std::unique_ptr<llvm::MemoryBuffer> Content =
      llvm::MemoryBuffer::getMemBuffer(Input);
  SrcMgr.AddNewSourceBuffer(std::move(Content), llvm::SMLoc());
  Grammar G;
  VarStore Vars;
  Parser TheParser(SrcMgr);
  TheParser.parse(G, Vars);
  calculateDerivesEpsilon(G);
  for (Node *N : G.nodes()) {
    if (auto *NT = llvm::dyn_cast<Nonterminal>(N)) {
      ASSERT_EQ(NT->derivesEpsilon(),
                NT->name() == "B" || NT->name() == "D" || NT->name() == "E");
    }
    if (auto *G = llvm::dyn_cast<Group>(N)) {
      ASSERT_EQ(G->derivesEpsilon(), G->isOptional());
    }
  }
}

TEST(AlgoTest, derivesEpsilon3Test) {
  // See
  // https://mkaul.wordpress.com/2009/12/11/computing-nullable-first-and-follow-sets/
  llvm::StringRef Input = "Z : 'd' | X Y Z ;\n"
                          "Y : ( 'c' )? ;\n"
                          "X : Y | 'a' ;\n";

  llvm::SourceMgr SrcMgr;
  Diagnostic Diag(SrcMgr);
  std::unique_ptr<llvm::MemoryBuffer> Content =
      llvm::MemoryBuffer::getMemBuffer(Input);
  SrcMgr.AddNewSourceBuffer(std::move(Content), llvm::SMLoc());
  Grammar G;
  VarStore Vars;
  Parser TheParser(SrcMgr);
  TheParser.parse(G, Vars);
  calculateDerivesEpsilon(G);
  for (Node *N : G.nodes()) {
    if (auto *NT = llvm::dyn_cast<Nonterminal>(N)) {
      ASSERT_EQ(NT->derivesEpsilon(), NT->name() == "X" || NT->name() == "Y");
    }
    if (auto *G = llvm::dyn_cast<Group>(N)) {
      ASSERT_EQ(G->derivesEpsilon(), G->isOptional());
    }
  }
}

TEST(AlgoTest, productive1Test) {
  // Wilhelm, Maurer; p. 297
  // Nonterminal Z is not productive.
  llvm::StringRef Input = "%token a, b\n"
                          "%%\n"
                          "S : a X ;\n"
                          "X : b S | a Y b Y ;\n"
                          "Y : b a | a Z ;\n"
                          "Z : a Z X ;\n";

  llvm::SourceMgr SrcMgr;
  Diagnostic Diag(SrcMgr);
  std::unique_ptr<llvm::MemoryBuffer> Content =
      llvm::MemoryBuffer::getMemBuffer(Input);
  SrcMgr.AddNewSourceBuffer(std::move(Content), llvm::SMLoc());
  Grammar G;
  VarStore Vars;
  Parser TheParser(SrcMgr);
  TheParser.parse(G, Vars);
  calculateProductive(G);
  for (Node *N : G.nodes()) {
    if (auto *NT = llvm::dyn_cast<Nonterminal>(N)) {
      ASSERT_EQ(NT->isProductive(), NT->name() != "Z");
    }
  }
}

TEST(AlgoTest, productive2Test) {
  // Nonterminal S and therefore all nodes are productive.
  llvm::StringRef Input = "%%\n"
                          "S : ( S )? ;\n";

  llvm::SourceMgr SrcMgr;
  Diagnostic Diag(SrcMgr);
  std::unique_ptr<llvm::MemoryBuffer> Content =
      llvm::MemoryBuffer::getMemBuffer(Input);
  SrcMgr.AddNewSourceBuffer(std::move(Content), llvm::SMLoc());
  Grammar G;
  VarStore Vars;
  Parser TheParser(SrcMgr);
  TheParser.parse(G, Vars);
  calculateProductive(G);
  for (Node *N : G.nodes()) {
      ASSERT_EQ(N->isProductive(), true);
  }
}

TEST(AlgoTest, productive3Test) {
  // https://zerobone.net/blog/cs/non-productive-cfg-rules/
  // Nonterminal S and D are productive.
  llvm::StringRef Input = "%token a, b, c, d\n"
                          "%%\n"
                          "S : ( A c D )? ;"
                          "A : B b ;\n"
                          "B : C ;\n"
                          "C : a A ;\n"
                          "D : d ;\n";

  llvm::SourceMgr SrcMgr;
  Diagnostic Diag(SrcMgr);
  std::unique_ptr<llvm::MemoryBuffer> Content =
      llvm::MemoryBuffer::getMemBuffer(Input);
  SrcMgr.AddNewSourceBuffer(std::move(Content), llvm::SMLoc());
  Grammar G;
  VarStore Vars;
  Parser TheParser(SrcMgr);
  TheParser.parse(G, Vars);
  calculateProductive(G);
  for (Node *N : G.nodes()) {
    if (auto *NT = llvm::dyn_cast<Nonterminal>(N)) {
      ASSERT_EQ(NT->isProductive(),
                NT->name() == "S" || NT->name() == "D" || NT->name() == "");
    }
  }
}

TEST(AlgoTest, productive4Test) {
  // Nonterminal B is not productive.
  llvm::StringRef Input = "%token a, b, c, d\n"
                          "%%\n"
                          "S : A | B ;"
                          "A : a ;\n"
                          "B : b B ;\n";

  llvm::SourceMgr SrcMgr;
  Diagnostic Diag(SrcMgr);
  std::unique_ptr<llvm::MemoryBuffer> Content =
      llvm::MemoryBuffer::getMemBuffer(Input);
  SrcMgr.AddNewSourceBuffer(std::move(Content), llvm::SMLoc());
  Grammar G;
  VarStore Vars;
  Parser TheParser(SrcMgr);
  TheParser.parse(G, Vars);
  calculateProductive(G);
  for (Node *N : G.nodes()) {
    if (auto *NT = llvm::dyn_cast<Nonterminal>(N)) {
      ASSERT_EQ(NT->isProductive(), NT->name() != "B");
    }
  }
}

} // anonymous namespace
