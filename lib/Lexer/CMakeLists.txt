set(LLVM_LINK_COMPONENTS support)

add_m2lang_library(m2langLexer
  Lexer.cpp
  Preprocessor.cpp

  LINK_LIBS
  m2langBasic
  )
