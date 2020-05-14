set(LLVM_LINK_COMPONENTS support)

add_m2lang_library(m2langParser
  Parser.cpp
  M2Parser.cpp

  LINK_LIBS
  m2langAST
  m2langLexer
  m2langSema
  )
