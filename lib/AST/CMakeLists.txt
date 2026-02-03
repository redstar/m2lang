set(LLVM_LINK_COMPONENTS Core Support)

add_m2lang_library(m2langAST
  CXXMODULES
  AST.cppm
  ASTContext.cppm
  PervasiveKinds.cppm
  Module.cppm
)
