set(LLVM_LINK_COMPONENTS support)

add_module_library(m2langLexer
  CXXMODULES
  Lexer.cppm
  Preprocessor.cppm
  Token.cppm
  Module.cppm

  LINK
  m2langBasic
  )
