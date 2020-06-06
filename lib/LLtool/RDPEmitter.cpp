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
#include "lltool/VarStore.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/FormatVariadic.h"
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
    for (Node *N : G.nodes()) {
      if (auto NT = llvm::dyn_cast<Nonterminal>(N))
        rule(NT);
    }
  }

private:
  struct Context {
    Nonterminal *Rule;
    bool AtStart;
  };

  void dispatch(Node *N, Context &Ctx);
  void rule(Nonterminal *N);
  void group(Group *N, Context &Ctx);
  void alternative(Alternative *N, Context &Ctx);
  void sequence(Sequence *N, Context &Ctx);
  void symbol(Symbol *N, Context &Ctx);
  void code(Code *N, Context &Ctx);

  unsigned followSetIndex(const llvm::BitVector &BV);
};

class RDPEmitter {
  Grammar &G;

  llvm::StringMap<unsigned> UniqueFollow;

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
  std::string ErrorHandlingLabel;
  std::string ErrorHandlingStmt;

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
  void emitSymbol(llvm::raw_ostream &OS, Symbol *N, unsigned Indent);
  void emitCode(llvm::raw_ostream &OS, Code *N, unsigned Indent);

  std::string condition(Node *N, bool UseFiFo);
  std::string condition(const llvm::BitVector &Set, const bool Negate);
  std::string functionName(Nonterminal *NT, bool WithClass = true);
  std::string tokenName(Terminal *T);
};
} // namespace

void PreProcess::dispatch(Node *N, Context &Ctx) {
  assert(N && "Node is null");
  switch (N->Kind) {
  case Node::NK_Alternative:
    alternative(llvm::cast<Alternative>(N), Ctx);
    break;
  case Node::NK_Code:
    code(llvm::cast<Code>(N), Ctx);
    break;
  case Node::NK_Group:
    group(llvm::cast<Group>(N), Ctx);
    break;
    break;
  case Node::NK_Sequence:
    sequence(llvm::cast<Sequence>(N), Ctx);
    break;
  case Node::NK_Symbol:
    symbol(llvm::cast<Symbol>(N), Ctx);
    break;
  case Node::NK_Nonterminal:
  case Node::NK_Terminal:
    llvm_unreachable("Internal error");
  }
}

void PreProcess::rule(Nonterminal *NT) {
  Context Ctx = {NT, false};
  dispatch(NT->Link, Ctx);
  if (NT->GenAttr.NeedsErrorHandling) {
    // Record FollowSet
    if (NT != G.syntheticStartSymbol())
      NT->GenAttr.FollowSetIndex = followSetIndex(NT->Link->FollowSet);
    else
      NT->GenAttr.FollowSetIndex = static_cast<unsigned>(-1);
  }
}

void PreProcess::group(Group *N, Context &Ctx) { dispatch(N->Link, Ctx); }

void PreProcess::alternative(Alternative *Alt, Context &Ctx) {
  for (Node *N = Alt->Link; N; N = N->Link)
    dispatch(N, Ctx);

  auto firstChildOfOptGroup = [](Node *node) {
    Node *n = node;
    Node *p = node->parent();
    while (p) {
      if (auto *G = llvm::dyn_cast<Group>(p)) {
        if (G->isOptional())
          return true;
      }
      if ((llvm::isa<Group>(p) &&
           llvm::cast<Group>(p)->Cardinality == Group::One) ||
          (llvm::isa<Sequence>(p) && p->Inner == n)) {
        n = p;
        p = p->parent();
        continue;
      }
      break;
    }
    return false;
  };

  bool UseSwitch = PreferSwitch;
  /* If the alternative is inside an optional group, e.g. ( A | B )?,
     then the condition of the group covers all tokens used in the
     alternative. Therefore an error check is not required. */
  bool NeedErrorHandling = firstChildOfOptGroup(Alt);
  for (Node *N = Alt->Link; N; N = N->Link) {
    UseSwitch &= /*singleCondition(n) &*/ !N->HasConflict;
    NeedErrorHandling &= !N->DerivesEpsilon;
  }
  Alt->GenAttr.CanUseSwitch = UseSwitch;
  Alt->GenAttr.NeedsErrorBranch = NeedErrorHandling;
}

void PreProcess::sequence(Sequence *Seq, Context &Ctx) {
  /* If this sequence is at then start of an alternative or group, then
   * generation of expect()/consume() can be replaced with alternative()
   * because the check already happened.
   */
  bool AtStart = false;
  if (auto *Alt = llvm::dyn_cast<Alternative>(Seq->Back)) {
    for (Node *N = Alt->Link; N; N = N->Link)
      if (N == Seq) {
        AtStart = true;
        break;
      }
  } else if (auto *G = llvm::dyn_cast<Group>(Seq->Back)) {
    AtStart = G->Link == Seq;
  }
  llvm::SaveAndRestore<bool> CtxAtStart(Ctx.AtStart, AtStart);
  for (Node *N = Seq->Inner; N; N = N->Next) {
    dispatch(N, Ctx);
    // Clear AtStart flag if anything but Code was visited.
    if (Ctx.AtStart)
      Ctx.AtStart = llvm::isa<Code>(N);
  }
}

void PreProcess::symbol(Symbol *Sym, Context &Ctx) {
  if (llvm::isa<Nonterminal>(Sym->Inner))
    Ctx.Rule->GenAttr.NeedsErrorHandling = true;
  else if (llvm::isa<Terminal>(Sym->Inner)) {
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
  std::string set;
  for (auto I = BV.set_bits_begin(), E = BV.set_bits_end(); I != E; ++I) {
    if (!set.empty())
      set.append("/");
    set.append(llvm::itostr(*I));
  }
  auto I = UniqueFollow.find(set);
  if (I != UniqueFollow.end())
    return I->getValue();
  unsigned Idx = UniqueFollow.size();
  UniqueFollow.insert(std::pair<std::string, unsigned>(set, Idx));
  return Idx;
}

void RDPEmitter::initialize(const VarStore &V) {
  llvm::StringRef Str;
  Str = V.getVar(var::ApiParserClass);
  ParserClass = Str.empty() ? "Parser" : Str;
  ParserClassWithOp = ParserClass + "::";
  GuardDeclaration = ParserClass;
  std::transform(GuardDeclaration.begin(), GuardDeclaration.end(),
                 GuardDeclaration.begin(), llvm::toUpper);
  GuardDefinition = GuardDeclaration;
  GuardDeclaration.append("_DECLARATION");
  GuardDefinition.append("_DEFINITION");
  Str = V.getVar(var::ApiTokenNamespace);
  TokenVarName = "Tok";
  TokenNamespace = Str.empty() ? "tok" : Str;
  TokenNamespaceWithOp = TokenNamespace + "::";
  TokenKindAttr = TokenVarName + ".getKind()";
  FollowSetType = "__TokenBitSet";
  FollowSetArgName = "__FollowSetCallers";
  FollowSetLocalName = "__FollowSet";
  FollowSetsName = "__FollowSets";
  ErrorHandlingLabel = "__errorhandler";
  ErrorHandlingStmt = "return " + ErrorHandlingLabel + "();";
}

void RDPEmitter::dispatch(llvm::raw_ostream &OS, Node *N, unsigned Indent) {
  assert(N && "Node is null");
  switch (N->Kind) {
  case Node::NK_Alternative:
    emitAlternative(OS, llvm::cast<Alternative>(N), Indent);
    break;
  case Node::NK_Code:
    emitCode(OS, llvm::cast<Code>(N), Indent);
    break;
  case Node::NK_Group:
    emitGroup(OS, llvm::cast<Group>(N), Indent);
    break;
  case Node::NK_Nonterminal:
    break;
  case Node::NK_Sequence:
    emitSequence(OS, llvm::cast<Sequence>(N), Indent);
    break;
  case Node::NK_Symbol:
    emitSymbol(OS, llvm::cast<Symbol>(N), Indent);
    break;
  case Node::NK_Terminal:
    break;
  }
}

void RDPEmitter::run(llvm::raw_ostream &OS) {
  OS << "#ifdef " << GuardDeclaration << "\n";
  emitTokenSetType(OS);
  emitFollowSets(OS, true);
  emitSupportFunc(OS, true);
  for (Node *N : G.nodes()) {
    if (auto NT = llvm::dyn_cast<Nonterminal>(N))
      if (NT != G.syntheticStartSymbol())
        emitRule(OS, NT, true);
  }
  OS << "#endif\n";
  OS << "#ifdef " << GuardDefinition << "\n";
  emitFollowSets(OS);
  emitSupportFunc(OS);
  for (Node *N : G.nodes()) {
    if (auto NT = llvm::dyn_cast<Nonterminal>(N))
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
    if (!NT->FormalArgs.empty())
      OS << ", " << NT->FormalArgs;
    OS << ");\n";
    return;
  }
  OS << "bool " << functionName(NT) << "(const " << FollowSetType << " &"
     << FollowSetArgName;
  if (!NT->FormalArgs.empty())
    OS << ", " << NT->FormalArgs;
  OS << ") {\n";
  if (NT->GenAttr.NeedsErrorHandling) {
    OS << "  const " << FollowSetType << " " << FollowSetLocalName << " = "
       << FollowSetsName << "[" << NT->GenAttr.FollowSetIndex << "] | "
       << FollowSetArgName << ";\n";
    OS << "  auto " << ErrorHandlingLabel << " = [this, " << FollowSetLocalName
       << "] {\n";
    OS << "    return __skipUntil(" << FollowSetLocalName << ", "
       << FollowSetsName << "[" << NT->GenAttr.FollowSetIndex << "]"
       << ");\n";
    OS << "  };\n";
  }

  dispatch(OS, NT->Link, Inc);
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
  for (Node *N : G.nodes()) {
    if (auto *NT = llvm::dyn_cast<Nonterminal>(N)) {
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
  }
  OS << "};\n";
}

void RDPEmitter::emitSupportFunc(llvm::raw_ostream &OS, bool OnlyPrototype) {
  if (OnlyPrototype) {
    OS << "bool __skipUntil(const " << FollowSetType << " &ActiveSets, const "
       << FollowSetType << " &CurrentSet);\n";
    return;
  }
  OS << "bool " << ParserClassWithOp << "__skipUntil(const "
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
      OS.indent(Indent) << Stmt << " (" << condition(N, true) << ") {\n";
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
  for (Node *N = Seq->Inner; N; N = N->Next) {
    if (GenAdvance && !llvm::isa<Code>(N)) {
      OS.indent(Indent) << "advance();\n";
      GenAdvance = false;
    }
    dispatch(OS, N, Indent);
    if (Symbol *Sym = llvm::dyn_cast<Symbol>(N)) {
      if (llvm::isa<Terminal>(Sym->Inner))
        GenAdvance = Sym->GenAttr.UseExpect || Sym->GenAttr.AtStart;
    }
  }
  if (GenAdvance)
    OS.indent(Indent) << "advance();\n";
}

void RDPEmitter::emitSymbol(llvm::raw_ostream &OS, Symbol *Sym,
                            unsigned Indent) {
  if (auto *NT = llvm::dyn_cast<Nonterminal>(Sym->Inner)) {
    OS.indent(Indent) << "if (" << functionName(NT) << "("
                      << FollowSetLocalName;
    if (!NT->FormalArgs.empty())
      OS << ", " << Sym->ActualArgs;
    OS << "))\n";
    OS.indent(Indent + Inc) << ErrorHandlingStmt << "\n";
  } else if (auto *T = llvm::dyn_cast<Terminal>(Sym->Inner)) {
    if (!Sym->GenAttr.AtStart) {
      std::string func = Sym->GenAttr.UseExpect ? "expect" : "consume";
      OS.indent(Indent) << "if (" << func << "(" << tokenName(T) << "))\n";
      OS.indent(Indent + Inc) << ErrorHandlingStmt << "\n";
    }
  } else
    llvm_unreachable("Symbol points to neither non-terminal nor to terminal");
}

void RDPEmitter::emitCode(llvm::raw_ostream &OS, Code *N, unsigned Indent) {
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
  const llvm::BitVector &Set =
      (UseFiFo && N->FirstSet.empty()) ? N->FollowSet : N->FirstSet;
  std::string Condition = condition(Set, false);
  if (auto *C = llvm::dyn_cast_or_null<Code>(N->Inner)) {
    if (C->Type == Code::Predicate || C->Type == Code::Resolver)
      Condition.append(" && (").append(C->Text).append(")");
  }
  return Condition;
}

std::string RDPEmitter::condition(const llvm::BitVector &Set,
                                  const bool Negate) {
  if (Set.empty())
    return "false";
  if (Set.count() == 1) {
    unsigned TokenIdx = Set.find_first();
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
  if (!NT->Name.empty()) {
    unsigned char Ch = llvm::toUpper(NT->Name[0]);
    FuncName.push_back(Ch);
    FuncName.append(NT->Name.substr(1));
  }
  return FuncName;
}

std::string RDPEmitter::tokenName(Terminal *T) {
  std::string TokenName(TokenNamespaceWithOp);
  if (!T->ExternalName.empty()) {
    TokenName.append(T->ExternalName);
  } else if (T == G.eoiTerminal()) {
    TokenName.append("eoi");
  } else {
    if (T->Name.startswith("\"")) {
      // Eliminate "
      llvm::StringRef Str = T->Name.substr(1, T->Name.size() - 2);
      if (llvm::isAlpha(Str[0]))
        TokenName.append("kw_");
      for (auto I = Str.begin(), E = Str.end(); I != E; ++I) {
        switch (*I) {
#define CASE(Ch, Name)                                                         \
  case Ch:                                                                     \
    TokenName.append(Name);                                                    \
    break
          CASE('.', "period");
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
          CASE('-', "minus");
          CASE('*', "star");
          CASE('/', "slash");
          CASE('^', "caret");
          CASE('#', "hash");
          CASE('<', "less");
          CASE('>', "greater");
          CASE('%', "percent");
          CASE('@', "at");
// TODO "..": "ellipsis", "->": "arrow",
#undef CASE
        default:
          TokenName.push_back(*I);
        }
      }
    } else
      TokenName.append(T->Name);
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
void EmitRDP(Grammar &G, VarStore &Vars, llvm::raw_ostream &OS) {
  PreProcess(G, Vars).run();
  RDPEmitter(G, Vars).run(OS);
}
} // namespace lltool