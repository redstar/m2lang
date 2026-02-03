set(LLVM_LINK_COMPONENTS support)

add_module_library(m2langBasic
  CXXMODULES
  Diagnostic.cppm
  TargetInfo.cppm
  TokenKinds.cppm
  )
