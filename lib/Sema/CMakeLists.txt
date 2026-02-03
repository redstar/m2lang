set(LLVM_LINK_COMPONENTS Core Support)

add_module_library(m2langSema PARTIAL_SOURCES_INTENDED
  CXXMODULES
  Sema.cppm

  LINK
  m2langAST
  )
