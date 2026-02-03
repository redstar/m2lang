set(LLVM_LINK_COMPONENTS support)

add_m2lang_library(lltool
Algo.cpp
Diagnostic.cpp
Grammar.cpp
GrammarBuilder.cpp
Lexer.cpp
Main.cpp
Parser.cpp
RDPEmitter.cpp
VarStore.cpp
)
