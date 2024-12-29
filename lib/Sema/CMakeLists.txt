set(LLVM_LINK_COMPONENTS Core Support)

add_m2lang_library(m2langSema PARTIAL_SOURCES_INTENDED
  Dummy.cpp
  LINK_LIBS
  m2langAST

  CXXMODULES
  Sema.cppm
  )
