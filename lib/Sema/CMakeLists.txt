set(LLVM_LINK_COMPONENTS Core Support)

add_m2lang_library(m2langSema
  Scope.cpp
  Sema.cpp

  LINK_LIBS
  m2langAST
  )
