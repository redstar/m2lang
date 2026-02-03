set(LLVM_LINK_COMPONENTS support)

add_module_library(m2langParser
  CXXMODULES
  Parser.cppm
  M2Parser.cppm

  LINK
  m2langAST
  m2langLexer
  m2langSema
  )
