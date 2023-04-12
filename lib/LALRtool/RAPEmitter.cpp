//===--- RAPEmitter.h - LALRtool RAP emitter --------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the recursive ascent parser emitter for LALRtool.
///
//===----------------------------------------------------------------------===//

#include "lalrtool/RAPEmitter.h"
#include "lalrtool/Grammar.h"
#include "lalrtool/LR0Automaton.h"
#include "lalrtool/VarStore.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"
#include <cstring>

using namespace lalrtool;

namespace {
class RAPEmitter {
  const Grammar &G;
  const LR0Automaton &LR0;

  // String-based stream for declarations.
  llvm::SmallString<4096> Declarations;
  llvm::raw_svector_ostream DeclOS;

  LR0ItemHelper ItemHelper;

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

public:
  RAPEmitter(const Grammar &G, const LR0Automaton &LR0, const VarStore &Vars)
      : G(G), LR0(LR0), DeclOS(Declarations) {
    initialize(Vars);
  }
  void run(llvm::raw_ostream &OS);

private:
  void initialize(const VarStore &V);
  void emitState(llvm::raw_ostream &OS, const LR0State &State);
  void emitCall(llvm::raw_ostream &OS, const LR0State &State, bool DoShift,
                unsigned Indent);
  void emitDataTypes(llvm::raw_ostream &OS);
  std::string setOfTokenNames(const lalrtool::FollowSetType &Set);
  std::string tokenName(Terminal *T);
};
} // namespace

void RAPEmitter::initialize(const VarStore &V) {
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

static void emitItem(llvm::raw_ostream &OS, const LR0Item &Item) {
  OS << Item << "\n";
}

/**
 * @brief Collected information for code generation
 *
 * The following information is stored for each state.
 * Multi-level return:
 * - The set of rules which could be recognized when this state
 *   is left.
 * - The set of possible return levels when this state is left.
 *
 * Example:
 * q1 = { [ E -> T . ], [E -> E . + T ] }
 * On return from state q1, 1 of 2 possible rules was matched.
 * The set of return levels is { 0 } aka length of the recognized
 * right side minus 1.
 */
class CodeInfo {
  llvm::DenseSet<std::pair<Nonterminal *, unsigned>> Returns;
  unsigned MinReturnLevel;
  unsigned MaxReturnLevel;

public:
  CodeInfo(const LR0State &State) {
    LR0ItemHelper ItemHelper;
    MinReturnLevel = static_cast<unsigned>(-1);
    MaxReturnLevel = 0;
    for (const LR0Item &Item : State.kernels()) {
      Returns.insert(std::pair<Nonterminal *, unsigned>(ItemHelper.getLHS(Item),
                                                        Item.getDot()));
      MinReturnLevel = std::min(MinReturnLevel, Item.getDot());
      MaxReturnLevel = std::max(MaxReturnLevel, Item.getDot());
    }
  }

  bool returnOneMoreLevel() { return MinReturnLevel > 1; }
  bool hasDifferentLevels() { return MinReturnLevel != MaxReturnLevel; }
};

/**
 * @brief Code generation graph.
 *
 * The kernel items of a LR(0) state define the parsing goal.
 * To reach this goal, subgoals must be parsed first.
 * The items in a state form a hierarchy. The goal is to
 * create this hierarchy to make code generation easier.
 *
 * Example from simple expression grammar:
 * --------
 * [ T -> T "*" . F ]
 * --------
 * [ F -> . id ]
 * [ F -> . "(" E ")" ]
 *
 * The goal of that state is to parse [ T -> T "*" . F ].
 * In order to recognize F either [ F -> . id ] or
 * [ F -> . "(" E ")" ] must be parsed first.
 *
 * This leads to the following parsing pseudo code:
 *
 * if (tok == id) {
 *   state([ F -> . id ])
 * } else if (tok == "(") {
 *   state([ F -> . "(" E ")" ])
 * } else
 *   error();
 * return state([ T -> T "*" . F ])
 *
 * The code generation graph is a data structure supporting
 * the generation of such code.
 *
 * Construction is as follows:
 * - An initial node is the root node.
 * - The initial node has vertices to all items shifting a
 *   terminal and all reduce items.
 * - For each of the nodes:
 *   - Take the nonterminal of the left hand side
 *   - Create a vertex to the item which has the dot in position 0,
 *     followed by that nonterminal.
 *     There can be several of those items
 *     - Connect first to a self-deriving item (the left side is the same
 *       nonterminal).
 *     - Create verices to all those items.
 * The construction stops after all items are added to the graph.
 *
 * If you view the closure() as producing a graph, then the code generation path
 * is that graph, with the direction of vertices reversed.
 *
 * How does this help with code generation?
 *
 */
struct CodeGenGraph {
  struct Node {};
  llvm::SmallVector<unsigned, 16> Level;
};

static std::string nonterminalID(Nonterminal *NT) {
  return llvm::Twine(NT->getID())
      .concat(" /* ")
      .concat(NT->getName())
      .concat(" */")
      .str();
}

void RAPEmitter::emitState(llvm::raw_ostream &OS, const LR0State &State) {
  // Emit declaration.
  DeclOS << "Configuration parseState" << State.getNo() << "();\n";

  // Emit definition.
  OS << "\n"
     << ParserClassWithOp << "Configuration " << ParserClassWithOp
     << "parseState" << State.getNo() << "() {\n";
  OS << "  PARSER_DEBUG(llvm::dbgs() << \"state " << State.getNo()
     << "\\n\");\n";
  OS << "/*\n";
  for (const LR0Item &Item : State.items()) {
    emitItem(OS, Item);
  }
  OS << "*/\n\n";

  // Sort items into categories.
  using LR0ItemSet = llvm::DenseSet<LR0Item>;
  llvm::DenseMap<Terminal *, LR0ItemSet> TerminalActions;
  llvm::DenseMap<Rule *, LR0ItemSet> ReduceActions;
  llvm::DenseMap<Rule *, LR0ItemSet> EmptyReduceActions;
  llvm::DenseMap<Nonterminal *, LR0ItemSet> NonterminalActions;
  for (const LR0Item &Item : State.items()) {
    if (Terminal *T = ItemHelper.getTerminalAfterDot(Item))
      TerminalActions[T].insert(Item);
    else if (Item.isReduceItem()) {
      if (Item.getRule()->getRHS().size())
        ReduceActions[Item.getRule()].insert(Item);
      else
        EmptyReduceActions[Item.getRule()].insert(Item);
    } else if (NonterminalRef *NTRef = llvm::dyn_cast<NonterminalRef>(
                   ItemHelper.getElementAfterDot(Item)))
      NonterminalActions[NTRef->getNonterminal()].insert(Item);
  }

  auto EmitDbgSet = [&OS](llvm::StringRef Name, const LR0ItemSet &ItemSet) {
    std::string Str;
    llvm::raw_string_ostream StrStream(Str);
    for (const LR0Item &Item : ItemSet)
      StrStream << Item << " ";
    size_t Pos = 0;
    while ((Pos = Str.find('"', Pos)) != std::string::npos) {
      Str.replace(Pos, 1, "\\\"");
      Pos += 2;
    }
    OS << "    PARSER_DEBUG(llvm::dbgs() << \"" << Name << " { " << Str
       << "}\\n\");\n";
  };

  OS << "  Configuration Cfg;\n  (void)Cfg;\n";

  // Handle reduce actions first.
  for (auto [R, ItemSet] : ReduceActions) {
    Nonterminal *NT = R->getNonterminal();
    OS << "  if (";
    OS << setOfTokenNames(NT->getFollowSet()) << ") {\n";
    EmitDbgSet("Reduce", ItemSet);
    OS << "    // Action $$ = ...\n";
    size_t Length = R->getRHS().size();
    if (Length)
      OS << "    return reduce<" << Length << ", " << nonterminalID(NT)
         << ">(/*$$=*/nullptr);\n";
    else
      OS << "    Cfg = reduce<" << nonterminalID(NT) << ">(/*$$=*/nullptr);\n";
    OS << "  }\n";
  }
  if (!ReduceActions.empty() && TerminalActions.empty()) {
    OS << "  error();\n  return Configuration{0, 0};\n";
  }

  bool First = true;
  for (auto [R, ItemSet] : EmptyReduceActions) {
    Nonterminal *NT = R->getNonterminal();
    OS << "  " << (First ? "if" : "else if") << " (";
    OS << setOfTokenNames(NT->getFollowSet()) << ") {\n";
    EmitDbgSet("Reduce", ItemSet);
    OS << "    // Action $$ = ...\n";
    OS << "    Cfg = reduce<" << nonterminalID(NT) << ">(/*$$=*/nullptr);\n";
    OS << "  }\n";
    First = false;
  }
  // Now handle the terminal actions.
  for (auto [T, ItemSet] : TerminalActions) {
    OS << "  " << (First ? "if" : "else if") << " (";
    OS << TokenVarName << ".is(" << tokenName(T) << ")) {\n";
    EmitDbgSet("Shift", ItemSet);
    const LR0State *Qnew = LR0.transition(&State, T);
    emitCall(OS, *Qnew, true, 4);
    OS << "  }\n";
    First = false;
  }
  if (!First) {
    OS << "  else {\n    error();\n    return Configuration{0, 0};\n  }\n";
  }

  if (!NonterminalActions.empty()) {
    OS << "  while (true) {\n";
    for (auto [NT, ItemSet] : NonterminalActions) {
      OS << "    if (Cfg.SymbolID == " << nonterminalID(NT) << ") {\n";
      EmitDbgSet("Call", ItemSet);
      const LR0State *Qnew = LR0.transition(&State, NT);
      emitCall(OS, *Qnew, false, 6);
      if (State.getNo() == 0) {
        // Break loop if reduced to $accept.
        auto It =
            std::find_if(ItemSet.begin(), ItemSet.end(), [](const LR0Item &I) {
              return I.getRule()->getNonterminal()->getID() == 0;
            });
        if (It != ItemSet.end())
          OS << "      if (Cfg.SymbolID == 0 /* $accept */)\n"
             << "        return Configuration{0 /* $accept */, 0};\n";
      }
      OS << "    }\n";
    }
    OS << "  }\n";
  }
  OS << "}\n";
}

void RAPEmitter::emitCall(llvm::raw_ostream &OS, const LR0State &State,
                          bool DoShift, unsigned Indent) {
  CodeInfo CInfo(State);
  llvm::Twine FuncName =
      llvm::Twine("parseState").concat(llvm::Twine(State.getNo()));
  std::string Call = DoShift ? llvm::Twine("shift<&")
                                   .concat(ParserClassWithOp)
                                   .concat(FuncName)
                                   .concat(">()")
                                   .str()
                             : FuncName.concat("()").str();
  if (CInfo.returnOneMoreLevel())
    OS.indent(Indent) << "return --" << Call << ";\n";
  else if (CInfo.hasDifferentLevels()) {
    OS.indent(Indent) << "if ((Cfg = " << Call << "))\n";
    OS.indent(Indent) << "  return --Cfg;\n";
  } else {
    OS.indent(Indent) << "Cfg = " << Call << ";\n";
  }
}

void RAPEmitter::emitDataTypes(llvm::raw_ostream &OS) {
  OS << "struct Configuration {\n"
     << "  unsigned SymbolID;\n"
     << "  unsigned ReturnLevel;\n\n"
     << "  operator bool() const { return ReturnLevel; }\n"
     << "  Configuration &operator--() {\n"
     << "    --ReturnLevel;\n"
     << "    return *this;\n"
     << "  }\n"
     << "};\n\n";
  OS << "union StackElement {\n"
     << "  StackElement(Token Tok) : Tok(Tok) {}\n"
     << "  StackElement(void *Ptr) : Ptr(Ptr) {}\n"
     << "  Token Tok;\n"
     << "  void *Ptr;\n"
     << "};\n\n";
  OS << "template <auto TransitionToState>\n"
     << "Configuration shift() {\n"
     << "  ParserStack.emplace_back(Tok);\n"
     << "  advance();\n"
     << "  return (this->*TransitionToState)();\n"
     << "}\n\n";
  OS << "template <unsigned Length, unsigned SymbolID>\n"
     << "Configuration reduce(StackElement Val) {\n"
     << "  ParserStack.pop_back_n(Length);\n"
     << "  ParserStack.push_back(Val);\n"
     << "  return Configuration{SymbolID, Length-1};\n"
     << "}\n\n";
  OS << "template <unsigned SymbolID>\n"
     << "Configuration reduce(StackElement Val) {\n"
     << "  ParserStack.push_back(Val);\n"
     << "  return Configuration{SymbolID, 0};\n"
     << "}\n\n";
}

void RAPEmitter::run(llvm::raw_ostream &OS) {
  OS << "/* All states\n";
  for (const LR0State &Q : LR0) {
    OS << "\nState " << Q.getNo() << "\n";
    OS << "--------\n";
    llvm::DenseSet<LR0Item> K;
    for (const LR0Item &Item : Q.kernels()) {
      OS << Item << "\n";
      K.insert(Item);
    }
    OS << "--------\n";
    for (const LR0Item &Item : Q.items()) {
      if (!K.contains(Item))
        OS << Item << "\n";
    }
    OS << "\n";
  }
  OS << "*/\n";
  OS << "#ifdef " << GuardDefinition << "\n";
  OS << "#if !defined(PARSER_DEBUG)\n"
     << "#define PARSER_DEBUG(x)\n"
     << "#endif\n";
  for (const LR0State &State : LR0) {
    emitState(OS, State);
  }
  OS << "#endif\n";
  OS << "#ifdef " << GuardDeclaration << "\n";
  emitDataTypes(OS);
  OS << "llvm::SmallVector<StackElement, 16> ParserStack;\n\n";
  OS << Declarations;
  OS << "#endif\n";
}

std::string RAPEmitter::setOfTokenNames(const lalrtool::FollowSetType &Set) {
  std::string TokenSetStr;
  size_t Count = 0;
  for (int ID = Set.find_first(); ID != -1; ID = Set.find_next(ID)) {
    if (Count)
      TokenSetStr.append(", ");
    TokenSetStr.append(tokenName(G.mapTerminal(ID)));
    ++Count;
  }
  if (Count > 1)
    return llvm::Twine(TokenVarName)
        .concat(".isOneOf(")
        .concat(TokenSetStr)
        .concat(")")
        .str();
  if (Count == 1)
    return llvm::Twine(TokenVarName)
        .concat(".is(")
        .concat(TokenSetStr)
        .concat(")")
        .str();
  return "true";
}

std::string RAPEmitter::tokenName(Terminal *T) {
  std::string TokenName(TokenNamespaceWithOp);
  if (!T->getExternalName().empty()) {
    TokenName.append(std::string(T->getExternalName()));
  } else if (T->getID() == 0) {
    TokenName.append("eoi");
  } else {
    if (T->getName().startswith("\"")) {
      // Eliminate "
      llvm::StringRef Str = T->getName().substr(1, T->getName().size() - 2);
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
      TokenName.append(std::string(T->getName()));
  }
  return TokenName;
}

#if 0
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
      if (auto *NT = llvm::dyn_cast<Nonterminal>(N))
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

  auto FirstChildOfOptGroup = [](Node *Root) {
    Node *N = Root;
    Node *P = Root->parent();
    while (P) {
      if (auto *G = llvm::dyn_cast<Group>(P)) {
        if (G->isOptional())
          return true;
      }
      if ((llvm::isa<Group>(P) &&
           llvm::cast<Group>(P)->Cardinality == Group::One) ||
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
  for (Node *N = Alt->Link; N; N = N->Link) {
    CanUseSwitch &= /*singleCondition(N) &*/ !N->HasConflict;
    NeedsErrorHandling &= !N->DerivesEpsilon;
  }
  Alt->GenAttr.CanUseSwitch = CanUseSwitch;
  Alt->GenAttr.NeedsErrorBranch = NeedsErrorHandling;
  if (NeedsErrorHandling)
    Ctx.Rule->GenAttr.NeedsErrorHandling = true;
}

void PreProcess::sequence(Sequence *Seq, Context &Ctx) {
  /* If this sequence is at then start of an alternative or group, then
   * generation of expect()/consume() can be replaced with advance()
   * because the check already happened.
   */
  bool AtStart = false;
  if (!Seq->DerivesEpsilon) {
    if (auto *Alt = llvm::dyn_cast<Alternative>(Seq->Back)) {
      for (Node *N = Alt->Link; N; N = N->Link)
        if (N == Seq) {
          AtStart = true;
          break;
        }
    } else if (auto *G = llvm::dyn_cast<Group>(Seq->Back)) {
      AtStart = G->Link == Seq;
    }
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
  std::string Set;
  for (auto I = BV.set_bits_begin(), E = BV.set_bits_end(); I != E; ++I) {
    if (!Set.empty())
      Set.append("/");
    Set.append(llvm::itostr(*I));
  }
  auto I = UniqueFollow.find(Set);
  if (I != UniqueFollow.end())
    return I->getValue();
  unsigned Idx = UniqueFollow.size();
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
    if (auto *NT = llvm::dyn_cast<Nonterminal>(N))
      if (NT != G.syntheticStartSymbol())
        emitRule(OS, NT, true);
  }
  OS << "#endif\n";
  OS << "#ifdef " << GuardDefinition << "\n";
  emitFollowSets(OS);
  emitSupportFunc(OS);
  for (Node *N : G.nodes()) {
    if (auto *NT = llvm::dyn_cast<Nonterminal>(N))
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
    OS << "    return " << SkipUntilName << "(" << FollowSetLocalName << ", "
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
      if (N->DerivesEpsilon) {
        if (!N->Link && N != Alt->Link) {
          bool UseElse = true;
          if (auto *C = llvm::dyn_cast_or_null<Code>(N->Inner)) {
            if (C->Type == Code::Predicate || C->Type == Code::Resolver) {
              Cond = std::string(C->Text);
              UseElse = false;
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
      std::string Func = Sym->GenAttr.UseExpect ? "expect" : "consume";
      OS.indent(Indent) << "if (" << Func << "(" << tokenName(T) << "))\n";
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
  llvm::BitVector Set(N->FirstSet);
  if (UseFiFo && N->DerivesEpsilon)
    Set |= N->FollowSet;
  std::string Condition = condition(Set, false);
  if (auto *C = llvm::dyn_cast_or_null<Code>(N->Inner)) {
    if (C->Type == Code::Predicate || C->Type == Code::Resolver)
      Condition.append(" && (").append(std::string(C->Text)).append(")");
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
    FuncName.append(std::string(NT->Name.substr(1)));
  }
  return FuncName;
}

std::string RDPEmitter::tokenName(Terminal *T) {
  std::string TokenName(TokenNamespaceWithOp);
  if (!T->ExternalName.empty()) {
    TokenName.append(std::string(T->ExternalName));
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
      TokenName.append(std::string(T->Name));
  }
  return TokenName;
}
#endif

namespace lalrtool {
void emitRAP(const Grammar &G, const LR0Automaton &LR0, VarStore &Vars,
             llvm::raw_ostream &OS) {
  // PreProcess(G, Vars).run();
  RAPEmitter(G, LR0, Vars).run(OS);
}
} // namespace lalrtool