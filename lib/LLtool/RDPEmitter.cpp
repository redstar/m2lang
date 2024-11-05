//===--- RDPEmitter.cpp - LLtool RDP emitter --------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the emitter for LLtool.
///
//===----------------------------------------------------------------------===//

#include "lltool/RDPEmitter.h"
#include "lltool/Grammar.h"
#include "lltool/Node.h"
#include "lltool/VarStore.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/TypeSwitch.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"
#include <cstring>

using namespace lltool;

namespace {
/* Additional attributes for code generation:
 * Global:
 *   - list/map of used follow sets
 * Nonterminal:
 *   - bool NeedsErrorHandling;
 *   - index of FollowSet
 * Alternative:
 *   - NeedsErrorAlternative
 *   - UseSwitch
 */
class PreProcess {
  Grammar &G;
  const bool PreferSwitch;

  llvm::StringMap<unsigned> UniqueFollow;

public:
  PreProcess(Grammar &G, const VarStore &V)
      : G(G), PreferSwitch(V.getFlag(var::CodePreferSwitch)) {}

  void run() {
    for (auto *NT : G.nonterminals())
      rule(NT);
  }

private:
  struct Context {
    Nonterminal *Rule;
    bool AtStart;
  };

  void dispatch(RightHandSide *N, Context &Ctx);
  void rule(Nonterminal *N);
  void group(Group *N, Context &Ctx);
  void alternative(Alternative *N, Context &Ctx);
  void sequence(Sequence *N, Context &Ctx);
  void symbol(SymbolRef *N, Context &Ctx);
  void code(Code *N, Context &Ctx);

  unsigned followSetIndex(const llvm::BitVector &BV);
};

class RDPEmitter {
  Grammar &G;

  llvm::StringMap<unsigned> UniqueFollow;

  // HACK Remember
  llvm::DenseSet<Code *> EmittedCode;

  // Text fragments
  std::string GuardDeclaration;
  std::string GuardDefinition;
  std::string ParserClass;
  std::string ParserClassWithOp;
  std::string TokenVarName;
  std::string TokenNamespace;
  std::string TokenNamespaceWithOp;
  std::string TokenKindAttr;
  std::string FollowSetType;
  std::string FollowSetArgName;
  std::string FollowSetLocalName;
  std::string FollowSetsName;
  std::string SkipUntilName;
  std::string ErrorHandlingLabel;
  std::string ErrorHandlingStmt;
  std::string Prefix;

  static const unsigned Inc = 2;

public:
  RDPEmitter(Grammar &G, const VarStore &V) : G(G) { initialize(V); };

  void run(llvm::raw_ostream &OS);

private:
  void initialize(const VarStore &V);
  void dispatch(llvm::raw_ostream &OS, Node *N, unsigned Indent);
  void emitRule(llvm::raw_ostream &OS, Nonterminal *N,
                bool OnlyPrototype = false);
  void emitFollowSets(llvm::raw_ostream &OS, bool OnlyPrototype = false);
  void emitSupportFunc(llvm::raw_ostream &OS, bool OnlyPrototype = false);
  void emitTokenSetType(llvm::raw_ostream &OS);
  void emitGroup(llvm::raw_ostream &OS, Group *N, unsigned Indent);
  void emitAlternative(llvm::raw_ostream &OS, Alternative *N, unsigned Indent);
  void emitSequence(llvm::raw_ostream &OS, Sequence *N, unsigned Indent);
  void emitSymbol(llvm::raw_ostream &OS, SymbolRef *N, unsigned Indent);
  void emitCode(llvm::raw_ostream &OS, Code *N, unsigned Indent);

  std::string condition(Node *N, bool UseFiFo);
  std::string condition(const llvm::BitVector &Set, const bool Negate);
  std::string functionName(Nonterminal *NT, bool WithClass = true);
  std::string tokenName(Terminal *T);
};
} // namespace

void PreProcess::dispatch(RightHandSide *N, Context &Ctx) {
  assert(N && "Node is null");
  llvm::TypeSwitch<RightHandSide *>(N)
      .Case([this, &Ctx](Alternative *N) { alternative(N, Ctx); })
      .Case([this, &Ctx](Code *N) { code(N, Ctx); })
      .Case([this, &Ctx](Group *N) { group(N, Ctx); })
      .Case([this, &Ctx](Sequence *N) { sequence(N, Ctx); })
      .Case([this, &Ctx](SymbolRef *N) { symbol(N, Ctx); });
}

void PreProcess::rule(Nonterminal *NT) {
  Context Ctx = {NT, false};
  dispatch(NT->getRHS(), Ctx);
  if (NT->GenAttr.NeedsErrorHandling) {
    // Record FollowSet
    if (NT != G.syntheticStartSymbol())
      NT->GenAttr.FollowSetIndex = followSetIndex(NT->Link->FollowSet);
    else
      NT->GenAttr.FollowSetIndex = static_cast<unsigned>(-1);
  }
}

void PreProcess::group(Group *N, Context &Ctx) { dispatch(N->element(), Ctx); }

void PreProcess::alternative(Alternative *Alt, Context &Ctx) {
  for (RightHandSide *N : Alt->alternatives())
    dispatch(N, Ctx);

  auto FirstChildOfOptGroup = [](Node *Root) {
    Node *N = Root;
    Node *P = Root->parent();
    while (P) {
      if (auto *G = llvm::dyn_cast<Group>(P)) {
        if (G->isOptional())
          return true;
      }
      if ((llvm::isa<Group>(P) &&
           llvm::cast<Group>(P)->isExactlyOne()) ||
          (llvm::isa<Sequence>(P) && P->Inner == N)) {
        N = P;
        P = P->parent();
        continue;
      }
      break;
    }
    return false;
  };

  bool CanUseSwitch = PreferSwitch;
  /* If the alternative is inside an optional group, e.g. ( A | B )?,
     then the condition of the group covers all tokens used in the
     alternative. Therefore an error check is not required. */
  bool NeedsErrorHandling = !FirstChildOfOptGroup(Alt);
  for (RightHandSide *N : Alt->alternatives()) {
    CanUseSwitch &= /*singleCondition(N) &*/ !N->HasConflict;
    NeedsErrorHandling &= !N->derivesEpsilon();
  }
  Alt->GenAttr.CanUseSwitch = CanUseSwitch;
  Alt->GenAttr.NeedsErrorBranch = NeedsErrorHandling;
  if (NeedsErrorHandling)
    Ctx.Rule->GenAttr.NeedsErrorHandling = true;
}

void PreProcess::sequence(Sequence *Seq, Context &Ctx) {
  /* If this sequence is at the start of an alternative or group, then
   * generation of expect()/consume() can be replaced with advance()
   * because the check already happened.
   */
  bool AtStart = false;
  if (!Seq->derivesEpsilon()) {
    if (auto *Alt = llvm::dyn_cast<Alternative>(Seq->Back)) {
      for (Node *N : Alt->alternatives())
        if (N == Seq) {
          AtStart = true;
          break;
        }
    } else if (auto *G = llvm::dyn_cast<Group>(Seq->Back)) {
      AtStart = G->Link == Seq;
    }
  }
  llvm::SaveAndRestore<bool> CtxAtStart(Ctx.AtStart, AtStart);
  for (RightHandSide *N : Seq->elements()) {
    dispatch(N, Ctx);
    // Clear AtStart flag if anything but Code was visited.
    if (Ctx.AtStart)
      Ctx.AtStart = llvm::isa<Code>(N);
  }
}

void PreProcess::symbol(SymbolRef *Sym, Context &Ctx) {
  if (llvm::isa<Nonterminal>(Sym->getSymbol()))
    Ctx.Rule->GenAttr.NeedsErrorHandling = true;
  else if (llvm::isa<Terminal>(Sym->getSymbol())) {
    // If the token is followed by code, then do not consume it now.
    Sym->GenAttr.UseExpect = llvm::isa_and_nonnull<Code>(Sym->Next);
    Sym->GenAttr.AtStart = Ctx.AtStart;
    if (!Ctx.AtStart)
      Ctx.Rule->GenAttr.NeedsErrorHandling = true;
  }
}

void PreProcess::code(Code *C, Context &Ctx) {
  if (C->Type == Code::Condition)
    Ctx.Rule->GenAttr.NeedsErrorHandling = true;
}

unsigned PreProcess::followSetIndex(const llvm::BitVector &BV) {
  // Turn BitVector into string.
  // Format is "Idx/Idx/Idx".
  // TODO Use a hash map.
  std::string Set;
  for (auto I = BV.set_bits_begin(), E = BV.set_bits_end(); I != E; ++I) {
    if (!Set.empty())
      Set.append("/");
    Set.append(llvm::itostr(*I));
  }
  auto I = UniqueFollow.find(Set);
  if (I != UniqueFollow.end())
    return I->getValue();
  const unsigned Idx = UniqueFollow.size();
  UniqueFollow.insert(std::pair<std::string, unsigned>(Set, Idx));
  return Idx;
}

void RDPEmitter::initialize(const VarStore &V) {
  ParserClass = V.getVar(var::ApiParserClass);
  ParserClassWithOp = ParserClass + "::";
  GuardDeclaration = ParserClass;
  std::transform(GuardDeclaration.begin(), GuardDeclaration.end(),
                 GuardDeclaration.begin(), llvm::toUpper);
  GuardDefinition = GuardDeclaration;
  GuardDeclaration.append("_DECLARATION");
  GuardDefinition.append("_DEFINITION");
  TokenVarName = V.getVar(var::ApiTokenName);
  TokenNamespace = V.getVar(var::ApiTokenNamespace);
  TokenNamespaceWithOp = TokenNamespace + "::";
  TokenKindAttr = TokenVarName + ".getKind()";
  Prefix = V.getVar(var::ApiPrefix);
  FollowSetType = Prefix + "TokenBitSet";
  FollowSetArgName = Prefix + "FollowSetCallers";
  FollowSetLocalName = Prefix + "FollowSet";
  FollowSetsName = Prefix + "FollowSets";
  SkipUntilName = Prefix + "skipUntil";
  ErrorHandlingLabel = Prefix + "errorhandler";
  ErrorHandlingStmt = "return " + ErrorHandlingLabel + "();";
}

void RDPEmitter::dispatch(llvm::raw_ostream &OS, Node *N, unsigned Indent) {
  assert(N && "Node is null");
  llvm::TypeSwitch<lltool::Node *>(N)
      .Case([this, &OS, Indent](Alternative *N) {
        emitAlternative(OS, N, Indent);
      })
      .Case([this, &OS, Indent](Code *N) { emitCode(OS, N, Indent); })
      .Case([this, &OS, Indent](Group *N) { emitGroup(OS, N, Indent); })
      .Case([this, &OS, Indent](Sequence *N) { emitSequence(OS, N, Indent); })
      .Case([this, &OS, Indent](SymbolRef *N) { emitSymbol(OS, N, Indent); })
      .Case<Nonterminal, Terminal>([](auto *) {});
}

void RDPEmitter::run(llvm::raw_ostream &OS) {
  OS << "#ifdef " << GuardDeclaration << "\n";
  emitTokenSetType(OS);
  emitFollowSets(OS, true);
  emitSupportFunc(OS, true);
  for (auto *NT : G.nonterminals()) {
    if (NT != G.syntheticStartSymbol())
      emitRule(OS, NT, true);
  }
  OS << "#endif\n";
  OS << "#ifdef " << GuardDefinition << "\n";
  emitFollowSets(OS);
  emitSupportFunc(OS);
  for (auto *NT : G.nonterminals()) {
    if (NT != G.syntheticStartSymbol())
      emitRule(OS, NT);
  }
  OS << "#endif\n";
}

void RDPEmitter::emitRule(llvm::raw_ostream &OS, Nonterminal *NT,
                          bool OnlyPrototype) {
  if (OnlyPrototype) {
    OS << "bool " << functionName(NT, false) << "(const " << FollowSetType
       << " &" << FollowSetArgName;
    if (!NT->formalArgs().empty())
      OS << ", " << NT->formalArgs();
    OS << ");\n";
    return;
  }
  OS << "bool " << functionName(NT) << "(const " << FollowSetType << " &"
     << FollowSetArgName;
  if (!NT->formalArgs().empty())
    OS << ", " << NT->formalArgs();
  OS << ") {\n";
  if (NT->GenAttr.NeedsErrorHandling) {
    OS << "  const " << FollowSetType << " " << FollowSetLocalName << " = "
       << FollowSetsName << "[" << NT->GenAttr.FollowSetIndex << "] | "
       << FollowSetArgName << ";\n";
    OS << "  auto " << ErrorHandlingLabel << " = [this, " << FollowSetLocalName
       << "] {\n";
    OS << "    return " << SkipUntilName << "(" << FollowSetLocalName << ", "
       << FollowSetsName << "[" << NT->GenAttr.FollowSetIndex << "]"
       << ");\n";
    OS << "  };\n";
  }

  dispatch(OS, NT->getRHS(), Inc);
  OS << "  return false;\n";
  OS << "}\n";
}

void RDPEmitter::emitTokenSetType(llvm::raw_ostream &OS) {
  OS << "template <unsigned NBits> struct BitSet {\n";
  OS << "  typedef uintptr_t BitWord;\n";
  OS << "\n";
  OS << "  enum { BITWORD_SIZE = (unsigned)sizeof(BitWord) * CHAR_BIT };\n";
  OS << "  enum { MEM_SIZE = (NBits + BITWORD_SIZE - 1) / BITWORD_SIZE };\n";
  OS << "\n";
  OS << "  BitWord Data[MEM_SIZE];\n";
  OS << "\n";
  OS << "  BitSet() { clear(); }\n";
  OS << "\n";
  OS << "  template <typename... Ts> BitSet(Ts... BitsToSet) {\n";
  OS << "    clear();\n";
  OS << "    set(BitsToSet...);\n";
  OS << "  }\n";
  OS << "\n";
  OS << "  void clear() { std::memset(Data, 0, sizeof(Data)); }\n";
  OS << "\n";
  OS << "  template <typename T> bool contains(T Idx) const {\n";
  OS << "    return (Data[Idx / BITWORD_SIZE] & BitWord(1) << (Idx % "
        "BITWORD_SIZE)) != 0;\n";
  OS << "  }\n";
  OS << "\n";
  OS << "  BitSet<NBits> &operator|=(const BitSet<NBits> &Other) {\n";
  OS << "    for (unsigned I = 0; I < MEM_SIZE; ++I)\n";
  OS << "      Data[I] |= Other.Data[I];\n";
  OS << "    return *this;\n";
  OS << "  }\n";
  OS << "\n";
  OS << "  BitSet<NBits> operator|(const BitSet<NBits> &Other) const {\n";
  OS << "    BitSet<NBits> Result;\n";
  OS << "    for (unsigned I = 0; I < MEM_SIZE; ++I)\n";
  OS << "      Result.Data[I] = Data[I] | Other.Data[I];\n";
  OS << "    return Result;\n";
  OS << "  }\n";
  OS << "\n";
  OS << "  template <typename T> void set(T Idx) {\n";
  OS << "    Data[Idx / BITWORD_SIZE] |= BitWord(1) << (Idx % BITWORD_SIZE);\n";
  OS << "  }\n";
  OS << "  template <typename T, typename... Ts> void set(T Idx, Ts... Idxs) "
        "{\n";
  OS << "    set(Idx);\n";
  OS << "    set(Idxs...);\n";
  OS << "  }\n";
  OS << "};\n";
  OS << "using " << FollowSetType << " = BitSet<" << TokenNamespaceWithOp
     << "NUM_TOKENS>;\n";
}

void RDPEmitter::emitFollowSets(llvm::raw_ostream &OS, bool OnlyPrototype) {
  if (OnlyPrototype) {
    OS << "static const " << FollowSetType << " " << FollowSetsName << "[];";
    return;
  }
  OS << "const " << ParserClassWithOp << FollowSetType << " "
     << ParserClassWithOp << FollowSetsName << "[] = {\n";
  unsigned Max = 0;
  for (auto *NT : G.nonterminals()) {
    if (NT == G.syntheticStartSymbol())
      continue;
    assert(Max >= NT->GenAttr.FollowSetIndex && "Wrong order");
    if (NT->GenAttr.FollowSetIndex < Max)
      continue;
    ++Max;
    OS << "  { ";
    bool IsFirst = true;
    for (auto I = NT->Link->FollowSet.set_bits_begin(),
              E = NT->Link->FollowSet.set_bits_end();
         I != E; ++I) {
      if (IsFirst)
        IsFirst = false;
      else
        OS << ", ";
      OS << tokenName(G.map(*I));
    }
    OS << " },\n";
  }
  OS << "};\n";
}

void RDPEmitter::emitSupportFunc(llvm::raw_ostream &OS, bool OnlyPrototype) {
  if (OnlyPrototype) {
    OS << "bool " << SkipUntilName << "(const " << FollowSetType
       << " &ActiveSets, const " << FollowSetType << " &CurrentSet);\n";
    return;
  }
  OS << "bool " << ParserClassWithOp << SkipUntilName << "(const "
     << ParserClassWithOp << FollowSetType << " &ActiveSets, const "
     << ParserClassWithOp << FollowSetType << " &CurrentSet) {\n";
  OS << "  " << ParserClassWithOp << FollowSetType
     << " StopSets = ActiveSets | " << tokenName(G.eoiTerminal()) << ";\n";
  OS << "  while (!StopSets.contains(" << TokenKindAttr << ")) {\n";
  OS << "    advance();\n";
  OS << "  }\n";
  OS << "  return CurrentSet.contains(" << TokenKindAttr << ");\n";
  OS << "}\n";
}

void RDPEmitter::emitGroup(llvm::raw_ostream &OS, Group *N, unsigned Indent) {
  switch (N->Cardinality) {
  case Group::One:
    dispatch(OS, N->Link, Indent);
    break;
  case Group::OneOrMore:
    OS.indent(Indent) << "do {\n";
    dispatch(OS, N->Link, Indent + Inc);
    OS.indent(Indent) << "} while (" << condition(N->Link, false) << ");\n";
    break;
  case Group::ZeroOrOne:
    OS.indent(Indent) << "if (" << condition(N->Link, false) << ") {\n";
    dispatch(OS, N->Link, Indent + Inc);
    OS.indent(Indent) << "}\n";
    break;
  case Group::ZeroOrMore:
    OS.indent(Indent) << "while (" << condition(N->Link, false) << ") {\n";
    dispatch(OS, N->Link, Indent + Inc);
    OS.indent(Indent) << "}\n";
    break;
  }
}

void RDPEmitter::emitAlternative(llvm::raw_ostream &OS, Alternative *Alt,
                                 unsigned Indent) {
  if (Alt->GenAttr.CanUseSwitch) {
    llvm_unreachable("Not yet implemented");
  } else {
    for (Node *N = Alt->Link; N; N = N->Link) {
      const char *Stmt = (N == Alt->Link) ? "if" : "else if";
      std::string Cond(condition(N, true));
      // Optimize alternative which derives epsilon.
      // If it is the last alternative in the list (but not the first!), then
      // replace the "else if" statement with "else" and do not emit the
      // condition. Otherwise, just make the condition always true.
      if (N->derivesEpsilon()) {
        if (!N->Link && N != Alt->Link) {
          bool UseElse = true;
          if (auto *C = llvm::dyn_cast_or_null<Code>(N->Inner)) {
            // TODO The case Code::Condition should not happen.
            if (C->Type == Code::Predicate || C->Type == Code::Resolver ||
                C->Type == Code::Condition) {
              Cond = std::string(C->Text);
              UseElse = false;
              EmittedCode.insert(C);
            }
          }
          if (UseElse) {
            Cond = "";
            Stmt = "else";
          }
        } else
          Cond.append("|| true");
      }
      OS.indent(Indent) << Stmt;
      if (Cond.size())
        OS << " (" << condition(N, true) << ")";
      OS << " {\n";
      dispatch(OS, N, Indent + Inc);
      OS.indent(Indent) << "}\n";
    }
    if (Alt->GenAttr.NeedsErrorBranch) {
      OS.indent(Indent) << "else {\n";
      OS.indent(Indent + Inc) << "error();\n";
      OS.indent(Indent + Inc) << ErrorHandlingStmt << "\n";
      OS.indent(Indent) << "}\n";
    }
  }
}

void RDPEmitter::emitSequence(llvm::raw_ostream &OS, Sequence *Seq,
                              unsigned Indent) {
  bool GenAdvance = false;
  for (RightHandSide *N : Seq->elements()) {
    if (GenAdvance && !llvm::isa<Code>(N)) {
      OS.indent(Indent) << "advance();\n";
      GenAdvance = false;
    }
    dispatch(OS, N, Indent);
    if (SymbolRef *Sym = llvm::dyn_cast<SymbolRef>(N)) {
      if (llvm::isa<Terminal>(Sym->Inner))
        GenAdvance = Sym->GenAttr.UseExpect || Sym->GenAttr.AtStart;
    }
  }
  if (GenAdvance)
    OS.indent(Indent) << "advance();\n";
}

void RDPEmitter::emitSymbol(llvm::raw_ostream &OS, SymbolRef *Sym,
                            unsigned Indent) {
  if (auto *NT = Sym->getNonterminal()) {
    OS.indent(Indent) << "if (" << functionName(NT) << "("
                      << FollowSetLocalName;
    if (!NT->formalArgs().empty())
      OS << ", " << Sym->actualArgs();
    OS << "))\n";
    OS.indent(Indent + Inc) << ErrorHandlingStmt << "\n";
  } else if (auto *T = Sym->getTerminal()) {
    if (!Sym->GenAttr.AtStart) {
      const llvm::StringRef Func = Sym->GenAttr.UseExpect ? "expect" : "consume";
      OS.indent(Indent) << "if (" << Func << "(" << tokenName(T) << "))\n";
      OS.indent(Indent + Inc) << ErrorHandlingStmt << "\n";
    }
  } else
    llvm_unreachable("Symbol points to neither non-terminal nor to terminal");
}

void RDPEmitter::emitCode(llvm::raw_ostream &OS, Code *N, unsigned Indent) {
  if (EmittedCode.contains(N))
    return;
  if (N->Type == Code::Normal) {
    OS.indent(Indent) << N->Text << "\n";
  } else if (N->Type == Code::Condition) {
    OS.indent(Indent) << "if (!(" << N->Text << ")) {\n";
    OS.indent(Indent + Inc) << "error();\n";
    OS.indent(Indent + Inc) << ErrorHandlingStmt << "\n";
    OS.indent(Indent) << "}\n";
  }
}

std::string RDPEmitter::condition(Node *N, bool UseFiFo) {
  llvm::BitVector Set(N->FirstSet);
  if (UseFiFo && N->derivesEpsilon())
    Set |= N->FollowSet;
  std::string Condition = condition(Set, false);
  if (auto *C = llvm::dyn_cast_or_null<Code>(N->Inner)) {
    // TODO The case Code::Condition should not happen.
    if (C->Type == Code::Predicate || C->Type == Code::Resolver ||
        C->Type == Code::Condition) {
      Condition.append(" && (").append(std::string(C->Text)).append(")");
      EmittedCode.insert(C);
    }
  }
  return Condition;
}

std::string RDPEmitter::condition(const llvm::BitVector &Set,
                                  const bool Negate) {
  if (Set.empty())
    return "false";
  if (Set.count() == 1) {
    const unsigned TokenIdx = Set.find_first();
    std::string Str(Negate ? "!" : "");
    Str.append(TokenVarName);
    Str.append(".is(");
    Str.append(tokenName(G.map(TokenIdx)));
    Str.append(")");
    return Str;
  }
  std::string Str(Negate ? "!" : "");
  Str.append("(").append(FollowSetType).append("{");
  for (int Idx = Set.find_first(); Idx != -1; Idx = Set.find_next(Idx)) {
    Str.append(tokenName(G.map(Idx)));
    Str.append(", ");
  }
  Str.append("}).contains(").append(TokenKindAttr).append(")");
  return Str;
}

std::string RDPEmitter::functionName(Nonterminal *NT, bool WithClass) {
  std::string FuncName(WithClass ? (ParserClassWithOp + "parse") : "parse");
  if (!NT->name().empty()) {
    const unsigned char Ch = llvm::toUpper(NT->name()[0]);
    FuncName.push_back(Ch);
    FuncName.append(std::string(NT->name().substr(1)));
  }
  return FuncName;
}

std::string RDPEmitter::tokenName(Terminal *T) {
  std::string TokenName(TokenNamespaceWithOp);
  if (!T->externalName().empty()) {
    TokenName.append(std::string(T->externalName()));
  } else if (T == G.eoiTerminal()) {
    TokenName.append("eoi");
  } else {
    if (T->name().starts_with("\"")) {
      // Eliminate "
      llvm::StringRef Str = T->name().substr(1, T->name().size() - 2);
      if (llvm::isAlpha(Str[0]))
        TokenName.append("kw_");
      for (auto I = Str.begin(), E = Str.end(); I != E; ++I) {
        switch (*I) {
#define CASE(Ch, Name)                                                         \
  case Ch:                                                                     \
    TokenName.append(Name);                                                    \
    break
          CASE('|', "pipe");
          CASE('=', "equal");
          CASE('(', "l_paren");
          CASE(')', "r_paren");
          CASE('[', "l_square");
          CASE(']', "r_square");
          CASE('{', "l_brace");
          CASE('}', "r_brace");
          CASE(',', "comma");
          CASE(';', "semi");
          CASE(':', "colon");
          CASE('?', "question");
          CASE('!', "exclaim");
          CASE('&', "amp");
          CASE('~', "tilde");
          CASE('+', "plus");
          CASE('*', "star");
          CASE('/', "slash");
          CASE('^', "caret");
          CASE('#', "hash");
          CASE('<', "less");
          CASE('>', "greater");
          CASE('%', "percent");
          CASE('@', "at");
          CASE('$', "dollar");
#undef CASE
        case '.':
          if (*(I + 1) == '.') {
            TokenName.append("ellipsis");
            ++I;
          } else
            TokenName.append("period");
          break;
        case '-':
          if (*(I + 1) == '>') {
            TokenName.append("arrow");
            ++I;
          } else
            TokenName.append("minus");
          break;
        default:
          TokenName.push_back(*I);
        }
      }
    } else
      TokenName.append(std::string(T->name()));
  }
  return TokenName;
}

#if 0
namespace {
template <unsigned NBits> struct BitSet {
  typedef uintptr_t BitWord;

  enum { BITWORD_SIZE = (unsigned)sizeof(BitWord) * CHAR_BIT };

  BitWord Data[(NBits + BITWORD_SIZE - 1) / BITWORD_SIZE];

  BitSet() { clear(); }

  template <typename... Ts> BitSet(Ts... BitsToSet) {
    clear();
    set(BitsToSet...);
  }

  void clear() { std::memset(Data, 0, sizeof(Data)); }

  template <typename T> bool contains(T Idx) const {
    return (Data[Idx / BITWORD_SIZE] & BitWord(1) << (Idx % BITWORD_SIZE)) != 0;
  }

  BitSet<NBits> &operator|=(const BitSet<NBits> &Other) {
    for (unsigned I = 0; I < sizeof(Data); ++I)
      Data[I] |= Other.Data[I];
    return *this;
  }

  template <typename T> void set(T Idx) {
    Data[Idx / BITWORD_SIZE] |= BitWord(1) << (Idx % BITWORD_SIZE);
  }
  template <typename T, typename... Ts> void set(T Idx, Ts... Idxs) {
    set(Idx);
    set(Idxs...);
  }
};

enum MyToken { One = 0, Two, Three, Four };
static BitSet<4> ALL[] = {{One, Three}, {One, Two}};
} // namespace

void RDPEmitter::calculateUniqueFollowSets(llvm::raw_ostream &OS) {
  unsigned No = 0;
  for (Node *N : G.nodes()) {
    if (auto *NT = llvm::dyn_cast<Nonterminal>(N)) {
      Node *N = NT->Link;
      // Turn BitVector into string
      std::string set;
      for (int Idx = N->FollowSet.find_first(); Idx != -1;
           Idx = N->FollowSet.find_next(Idx)) {
        set.append(tokenName(G.map(Idx)));
        set.append(", ");
      }
      set = set.substr(0, set.size() - 2);
      if (UniqueFollow.find(set) == UniqueFollow.end()) {
        UniqueFollow.insert(std::pair<std::string, unsigned>(set, No++));
      }
    }
  }
  for (std::string str : UniqueFollow.keys()) {
    OS << "BitSet<tok::NUM_TOKENS> Set(" << str << ")\n";
  }
  OS << "Unique follow sets: " << UniqueFollow.size() << "\n";
  unsigned NT = 0, T = 0;
  for (Node *N : G.nodes()) {
    if (llvm::isa<Nonterminal>(N))
      NT++;
    if (llvm::isa<Terminal>(N))
      T++;
  }
  OS << "Nonterminals: " << NT << "\n";
  OS << "Terminals: " << T << "\n";
  OS << "Bitset BITWORD: \n";
  OS << "BITWORD_SIZE: " << BitSet<4>::BITWORD_SIZE << "\n";
  OS << "Bitset test (One,Three): \n";
  OS << "One:   " << ALL[0].contains(One) << "\n";
  OS << "Two:   " << ALL[0].contains(Two) << "\n";
  OS << "Three: " << ALL[0].contains(Three) << "\n";
  OS << "Four:  " << ALL[0].contains(Four) << "\n";
  OS << "Bitset test (One,Two): \n";
  OS << "One:   " << ALL[1].contains(One) << "\n";
  OS << "Two:   " << ALL[1].contains(Two) << "\n";
  OS << "Three: " << ALL[1].contains(Three) << "\n";
  OS << "Four:  " << ALL[1].contains(Four) << "\n";
  OS << "Bitset test (Four): \n";
  OS << "Four:  " << (BitSet<4>{Four}).contains(Four) << "\n";
}
#endif

namespace lltool {
void emitRDP(Grammar &G, VarStore &Vars, llvm::raw_ostream &OS) {
  PreProcess(G, Vars).run();
  RDPEmitter(G, Vars).run(OS);
}
} // namespace lltool
