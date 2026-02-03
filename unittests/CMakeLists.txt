add_custom_target(M2langUnitTests)
set_target_properties(M2langUnitTests PROPERTIES FOLDER "M2lang tests")

# add_m2lang_unittest(test_dirname file1.cpp file2.cpp)
#
# Will compile the list of files together and link against the m2lang
# Produces a binary named 'basename(test_dirname)'.
function(add_m2lang_unittest test_dirname)
  add_unittest(M2langUnitTests ${test_dirname} ${ARGN})
endfunction()

add_subdirectory(Lexer)
add_subdirectory(LLtool)
