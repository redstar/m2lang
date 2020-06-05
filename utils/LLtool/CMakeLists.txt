add_llvm_utility(LLtool
  Algo.cpp
  Diagnostic.cpp
  Grammar.cpp
  GrammarBuilder.cpp
  Lexer.cpp
  LLtool.cpp
  Parser.cpp
  RDPEmitter.cpp
  VarStore.cpp
  )

target_link_libraries(LLtool PRIVATE LLVMSupport)
