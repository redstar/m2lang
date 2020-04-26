get_property(LLVM_LIT_CONFIG_MAP GLOBAL PROPERTY LLVM_LIT_CONFIG_MAP)

get_llvm_lit_path(LIT_BASE_DIR LIT_FILE_NAME)

set(LLVM_SOURCE_DIR ${M2LANG_SOURCE_DIR})
set(LLVM_BINARY_DIR ${M2LANG_BINARY_DIR})

if(NOT "${CMAKE_CFG_INTDIR}" STREQUAL ".")
  foreach(BUILD_MODE ${CMAKE_CONFIGURATION_TYPES})
    string(REPLACE ${CMAKE_CFG_INTDIR} ${BUILD_MODE} bi ${LIT_BASE_DIR})
    set(bi "${bi}/${LIT_FILE_NAME}")
    configure_file(
      llvm-lit.in
      ${bi}
      )
  endforeach()
else()
  set(BUILD_MODE .)
  configure_file(
    llvm-lit.in
    ${LIT_BASE_DIR}/${LIT_FILE_NAME}
    )
endif()
