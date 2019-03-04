set(LLVM_LINK_COMPONENTS support)

add_m2lang_library(m2lang
  Lexer.cpp
  TokenKinds.cpp

  LINK_LIBS
  ""
  )
