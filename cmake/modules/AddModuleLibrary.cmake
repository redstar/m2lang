# Adds a library using C++20 modules.
# - FILE_SET is the name of the file set with the modules
# - CXXFILES are normal C++ files and module implementations
# - CXXMODULES are the module files
# - LINK adds private link dependencies
# The target name of the library is prefixed by lib.
function(add_module_library name)
  cmake_parse_arguments(ARG "" "FILE_SET" "CXXFILES;CXXMODULES;LINK" ${ARGN})

  if(NOT ARG_FILE_SET)
    set(ARG_FILE_SET "${name}_modules")
  endif()

  add_library(lib${name} ${ARG_UNPARSED})
  set_property(TARGET lib${name} PROPERTY OUTPUT_NAME ${name})
  if(ARG_LINK)
    target_link_libraries(lib${name} PRIVATE ${ARG_LINK})
  endif()
  target_compile_features(lib${name} PUBLIC cxx_std_20)
  target_sources(lib${name}
    PUBLIC
      ${ARG_CXXFILES}
    PUBLIC
      FILE_SET ${ARG_FILE_SET}
      TYPE CXX_MODULES
      FILES
        ${ARG_CXXMODULES}
  )
endfunction()
