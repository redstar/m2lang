set(LLVM_LINK_COMPONENTS support)

add_m2lang_library(m2langCodeGen
  CGModule.cpp
  CGProcedure.cpp
  CGTBAA.cpp
  CGUtils.cpp
  CodeGenerator.cpp

  LINK_LIBS
  m2langAST
  m2langSema
  )
