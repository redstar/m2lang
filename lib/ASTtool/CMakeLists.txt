set(LLVM_LINK_COMPONENTS support)

add_m2lang_library(asttool
ClassBuilder.cpp
ClassEmitter.cpp
Diagnostic.cpp
Lexer.cpp
Main.cpp
Parser.cpp
VarStore.cpp
)
