set(LLVM_LINK_COMPONENTS support)

add_m2lang_library(lalrtool
Algo.cpp
Diagnostic.cpp
Grammar.cpp
GrammarBuilder.cpp
Lexer.cpp
LR0Automaton.cpp
Main.cpp
Parser.cpp
#RDPEmitter.cpp
VarStore.cpp
)
