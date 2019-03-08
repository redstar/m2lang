set(LLVM_LINK_COMPONENTS
  Support
  )

add_m2lang_unittest(LexerTests
  LexerTest.cpp
  )

target_link_libraries(LexerTests
  PRIVATE
  m2langLexer
  m2langBasic
  )
