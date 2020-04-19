set( LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Analysis
  CodeGen
  Core
  IPO
  AggressiveInstCombine
  InstCombine
  Instrumentation
  MC
  MCParser
  ObjCARCOpts
  Option
  ScalarOpts
  Support
  TransformUtils
  Vectorize
  )

add_m2lang_tool(m2lang
  driver.cpp
  )

m2lang_target_link_libraries(m2lang
  PRIVATE
  m2langAST
  m2langBasic
  m2langLexer
  m2langParser
  m2langSema
  )