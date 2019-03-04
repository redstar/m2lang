# Generate a list of CMake library targets so that other CMake projects can
# link against them. LLVM calls its version of this file LLVMExports.cmake, but
# the usual CMake convention seems to be ${Project}Targets.cmake.
set(M2LANG_INSTALL_PACKAGE_DIR lib${LLVM_LIBDIR_SUFFIX}/cmake/m2lang)
set(m2lang_cmake_builddir "${CMAKE_BINARY_DIR}/${M2LANG_INSTALL_PACKAGE_DIR}")

# Keep this in sync with llvm/cmake/CMakeLists.txt!
set(LLVM_INSTALL_PACKAGE_DIR lib${LLVM_LIBDIR_SUFFIX}/cmake/llvm)
set(llvm_cmake_builddir "${LLVM_BINARY_DIR}/${LLVM_INSTALL_PACKAGE_DIR}")

get_property(M2LANG_EXPORTS GLOBAL PROPERTY M2LANG_EXPORTS)
export(TARGETS ${M2LANG_EXPORTS} FILE ${m2lang_cmake_builddir}/M2langTargets.cmake)

# Generate M2langConfig.cmake for the build tree.
set(M2LANG_CONFIG_CMAKE_DIR "${m2lang_cmake_builddir}")
set(M2LANG_CONFIG_LLVM_CMAKE_DIR "${llvm_cmake_builddir}")
set(M2LANG_CONFIG_EXPORTS_FILE "${m2lang_cmake_builddir}/M2langTargets.cmake")
set(M2LANG_CONFIG_INCLUDE_DIRS
  "${M2LANG_SOURCE_DIR}/include"
  "${M2LANG_BINARY_DIR}/include"
  )
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/M2langConfig.cmake.in
  ${m2lang_cmake_builddir}/M2langConfig.cmake
  @ONLY)
set(M2LANG_CONFIG_CMAKE_DIR)
set(M2LANG_CONFIG_LLVM_CMAKE_DIR)
set(M2LANG_CONFIG_EXPORTS_FILE)

# Generate M2langConfig.cmake for the install tree.
set(M2LANG_CONFIG_CODE "
# Compute the installation prefix from this LLVMConfig.cmake file location.
get_filename_component(M2LANG_INSTALL_PREFIX \"\${CMAKE_CURRENT_LIST_FILE}\" PATH)")
# Construct the proper number of get_filename_component(... PATH)
# calls to compute the installation prefix.
string(REGEX REPLACE "/" ";" _count "${M2LANG_INSTALL_PACKAGE_DIR}")
foreach(p ${_count})
  set(M2LANG_CONFIG_CODE "${M2LANG_CONFIG_CODE}
get_filename_component(M2LANG_INSTALL_PREFIX \"\${M2LANG_INSTALL_PREFIX}\" PATH)")
endforeach(p)
set(M2LANG_CONFIG_CMAKE_DIR "\${M2LANG_INSTALL_PREFIX}/${M2LANG_INSTALL_PACKAGE_DIR}")
set(M2LANG_CONFIG_LLVM_CMAKE_DIR "\${M2LANG_INSTALL_PREFIX}/${LLVM_INSTALL_PACKAGE_DIR}")
set(M2LANG_CONFIG_EXPORTS_FILE "\${M2LANG_CMAKE_DIR}/M2langTargets.cmake")
set(M2LANG_CONFIG_INCLUDE_DIRS
  "\${M2LANG_INSTALL_PREFIX}/include"
  )
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/M2langConfig.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/M2langConfig.cmake
  @ONLY)
set(M2LANG_CONFIG_CODE)
set(M2LANG_CONFIG_CMAKE_DIR)
set(M2LANG_CONFIG_EXPORTS_FILE)

if (NOT LLVM_INSTALL_TOOLCHAIN_ONLY)
  get_property(m2lang_has_exports GLOBAL PROPERTY M2LANG_HAS_EXPORTS)
  if(m2lang_has_exports)
    install(EXPORT M2langTargets DESTINATION ${M2LANG_INSTALL_PACKAGE_DIR}
            COMPONENT m2lang-cmake-exports)
  endif()

  install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/M2langConfig.cmake
    DESTINATION ${M2LANG_INSTALL_PACKAGE_DIR}
    COMPONENT m2lang-cmake-exports)

  if(NOT LLVM_ENABLE_IDE)
    # Add a dummy target so this can be used with LLVM_DISTRIBUTION_COMPONENTS
    add_custom_target(m2lang-cmake-exports)
    add_llvm_install_targets(install-m2lang-cmake-exports
                             COMPONENT m2lang-cmake-exports)
  endif()
endif()
