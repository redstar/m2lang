set(LLVM_LINK_COMPONENTS Core Support)

add_m2lang_library(m2langAST
  AST.cpp
  ASTContext.cpp
  PervasiveTypeKinds.cpp
  Scope.cpp

  LINK_LIBS
  ""
  )
