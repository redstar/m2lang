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

namespace {
struct InitBitVector {
  template <typename... Ts>
  InitBitVector(unsigned NBits, Ts... BitsToSet) : Bits(NBits) {
    set(BitsToSet...);
  }

  template <typename T> void set(T Idx) { Bits.set(Idx); }

  template <typename T, typename... Ts> void set(T Idx, Ts... Idxs) {
    Bits.set(Idx);
    set(Idxs...);
  }

  llvm::BitVector operator()() { return Bits; }

private:
  llvm::BitVector Bits;
};

} // namespace

TEST(FirstFollowSetTest, firstSet1Test) {
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
  calculateReachable(G);
  calculateDerivesEpsilon(G);
  calculateFirstSets(G);
  llvm::StringMap<Nonterminal *> MapNT;
  llvm::StringMap<Terminal *> MapT;
  for (Node *N : G.nodes()) {
    if (auto *NT = llvm::dyn_cast<Nonterminal>(N)) {
      MapNT[NT->Name] = NT;
    } else if (auto *T = llvm::dyn_cast<Terminal>(N)) {
      MapT[T->Name] = T;
    }
  }

  const unsigned NumT = G.numberOfTerminals();
  ASSERT_EQ(MapNT["S"]->FirstSet,
            InitBitVector(NumT, MapT["id"]->No, MapT["'('"]->No)());
  ASSERT_FALSE(MapNT["S"]->derivesEpsilon());
  ASSERT_EQ(MapNT["E"]->FirstSet,
            InitBitVector(NumT, MapT["id"]->No, MapT["'('"]->No)());
  ASSERT_FALSE(MapNT["E"]->derivesEpsilon());
  ASSERT_EQ(MapNT["Eq"]->FirstSet, InitBitVector(NumT, MapT["'+'"]->No)());
  ASSERT_TRUE(MapNT["Eq"]->derivesEpsilon());
  ASSERT_EQ(MapNT["T"]->FirstSet,
            InitBitVector(NumT, MapT["id"]->No, MapT["'('"]->No)());
  ASSERT_FALSE(MapNT["T"]->derivesEpsilon());
  ASSERT_EQ(MapNT["Tq"]->FirstSet, InitBitVector(NumT, MapT["'*'"]->No)());
  ASSERT_TRUE(MapNT["Tq"]->derivesEpsilon());
  ASSERT_EQ(MapNT["F"]->FirstSet,
            InitBitVector(NumT, MapT["id"]->No, MapT["'('"]->No)());
  ASSERT_FALSE(MapNT["F"]->derivesEpsilon());
}

TEST(FirstFollowSetTest, followSet1Test) {
  // This is basically the same example as from Wilhelm, Maurer.
  // Please note the ";" at the end of production stmt.
  llvm::StringRef Input = "%token number\n"
                          "%%\n"
                          "stmt : expr ';' ;\n"
                          "expr : ( term exprq )? ;\n"
                          "exprq : ( '+' term exprq )? ;\n"
                          "term : factor termq ;\n"
                          "termq : ( '*' factor termq )? ;\n"
                          "factor : '(' expr ')' | number ;\n";

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
  calculateDerivesEpsilon(G);
  calculateFirstSets(G);
  calculateFollowSets(G);
  llvm::StringMap<Nonterminal *> MapNT;
  llvm::StringMap<Terminal *> MapT;
  for (Node *N : G.nodes()) {
    if (auto *NT = llvm::dyn_cast<Nonterminal>(N)) {
      MapNT[NT->Name] = NT;
    } else if (auto *T = llvm::dyn_cast<Terminal>(N)) {
      MapT[T->Name] = T;
    }
  }

  const unsigned NumT = G.numberOfTerminals();
  ASSERT_EQ(MapNT["stmt"]->FirstSet,
            InitBitVector(NumT, MapT["number"]->No, MapT["'('"]->No,
                          MapT["';'"]->No)());
  ASSERT_EQ(MapNT["stmt"]->Link->FollowSet,
            InitBitVector(NumT, MapT["_eoi"]->No)());
  ASSERT_FALSE(MapNT["stmt"]->derivesEpsilon());

  ASSERT_EQ(MapNT["expr"]->FirstSet,
            InitBitVector(NumT, MapT["number"]->No, MapT["'('"]->No)());
  ASSERT_EQ(MapNT["expr"]->Link->FollowSet,
            InitBitVector(NumT, MapT["';'"]->No, MapT["')'"]->No)());
  ASSERT_TRUE(MapNT["expr"]->derivesEpsilon());

  ASSERT_EQ(MapNT["exprq"]->FirstSet, InitBitVector(NumT, MapT["'+'"]->No)());
  ASSERT_EQ(MapNT["exprq"]->Link->FollowSet,
            InitBitVector(NumT, MapT["';'"]->No, MapT["')'"]->No)());
  ASSERT_TRUE(MapNT["exprq"]->derivesEpsilon());

  ASSERT_EQ(MapNT["term"]->FirstSet,
            InitBitVector(NumT, MapT["number"]->No, MapT["'('"]->No)());
  ASSERT_EQ(
      MapNT["term"]->Link->FollowSet,
      InitBitVector(NumT, MapT["'+'"]->No, MapT["';'"]->No, MapT["')'"]->No)());
  ASSERT_FALSE(MapNT["term"]->derivesEpsilon());

  ASSERT_EQ(MapNT["termq"]->FirstSet, InitBitVector(NumT, MapT["'*'"]->No)());
  ASSERT_EQ(
      MapNT["termq"]->Link->FollowSet,
      InitBitVector(NumT, MapT["'+'"]->No, MapT["';'"]->No, MapT["')'"]->No)());
  ASSERT_TRUE(MapNT["termq"]->derivesEpsilon());

  ASSERT_EQ(MapNT["factor"]->FirstSet,
            InitBitVector(NumT, MapT["number"]->No, MapT["'('"]->No)());
  ASSERT_EQ(MapNT["factor"]->Link->FollowSet,
            InitBitVector(NumT, MapT["'*'"]->No, MapT["'+'"]->No,
                          MapT["';'"]->No, MapT["')'"]->No)());
  ASSERT_FALSE(MapNT["factor"]->derivesEpsilon());
}

} // anonymous namespace
