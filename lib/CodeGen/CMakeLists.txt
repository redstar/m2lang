set(LLVM_LINK_COMPONENTS support)

add_m2lang_library(m2langCodeGen
  CodeGen.cpp

  LINK_LIBS
  m2langAST
  m2langSema
  )
