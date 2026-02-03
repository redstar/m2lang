set(LLVM_LINK_COMPONENTS support)

add_module_library(m2langCodeGen
  CXXMODULES
  CGModule.cppm
  CGProcedure.cppm
  CGTBAA.cppm
  CGUnit.cppm
  CodeGenerator.cppm

  LINK
  m2langAST
  m2langSema
  )
