set(LLVM_LINK_COMPONENTS
  Support
  )

add_m2lang_unittest(LLtoolTests
  AlgoTest.cpp
  )

target_link_libraries(LLtoolTests
  PRIVATE
  lltool
  )
