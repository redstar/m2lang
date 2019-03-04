# Test runner infrastructure for M2lang. This configures the M2lang test trees
# for use by Lit, and delegates to LLVM's lit test handlers.

if (CMAKE_CFG_INTDIR STREQUAL ".")
  set(LLVM_BUILD_MODE ".")
else ()
  set(LLVM_BUILD_MODE "%(build_mode)s")
endif ()

string(REPLACE ${CMAKE_CFG_INTDIR} ${LLVM_BUILD_MODE} M2LANG_TOOLS_DIR ${LLVM_RUNTIME_OUTPUT_INTDIR})

if(M2LANG_BUILT_STANDALONE)
  # Set HAVE_LIBZ according to recorded LLVM_ENABLE_ZLIB value. This
  # value is forced to 0 if zlib was not found, so it is fine to use it
  # instead of HAVE_LIBZ (not recorded).
  if(LLVM_ENABLE_ZLIB)
    set(HAVE_LIBZ 1)
  endif()
endif()

llvm_canonicalize_cmake_booleans(
  ENABLE_BACKTRACES
  HAVE_LIBZ
  LLVM_ENABLE_PER_TARGET_RUNTIME_DIR)

configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.py.in
  ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg.py
  MAIN_CONFIG
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.cfg.py
  )

configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/Unit/lit.site.cfg.py.in
  ${CMAKE_CURRENT_BINARY_DIR}/Unit/lit.site.cfg.py
  MAIN_CONFIG
  ${CMAKE_CURRENT_SOURCE_DIR}/Unit/lit.cfg.py
  )

option(M2LANG_TEST_USE_VG "Run M2lang tests under Valgrind" OFF)
if(M2LANG_TEST_USE_VG)
  set(M2LANG_TEST_EXTRA_ARGS ${M2LANG_TEST_EXTRA_ARGS} "--vg")
endif ()

list(APPEND M2LANG_TEST_DEPS
#  m2lang m2lang-headers
#  m2lang-tblgen
  )

set(M2LANG_TEST_PARAMS
  m2lang_site_config=${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg
  )

if( NOT M2LANG_BUILT_STANDALONE )
  list(APPEND M2LANG_TEST_DEPS
    llvm-config
    FileCheck count not
    llc
    llvm-as
    llvm-bcanalyzer
    llvm-cat
    llvm-dis
    llvm-lto2
    llvm-modextract
    llvm-nm
    llvm-objdump
    llvm-profdata
    llvm-readobj
    llvm-symbolizer
    opt
    )

  if(TARGET llvm-lto)
    list(APPEND M2LANG_TEST_DEPS llvm-lto)
  endif()
endif()

add_custom_target(m2lang-test-depends DEPENDS ${M2LANG_TEST_DEPS})
set_target_properties(m2lang-test-depends PROPERTIES FOLDER "M2lang tests")

add_lit_testsuite(check-m2lang "Running the M2lang regression tests"
  ${CMAKE_CURRENT_BINARY_DIR}
  #LIT ${LLVM_LIT}
  PARAMS ${M2LANG_TEST_PARAMS}
  DEPENDS ${M2LANG_TEST_DEPS}
  ARGS ${M2LANG_TEST_EXTRA_ARGS}
  )
set_target_properties(check-m2lang PROPERTIES FOLDER "M2lang tests")

add_lit_testsuites(M2LANG ${CMAKE_CURRENT_SOURCE_DIR}
  PARAMS ${M2LANG_TEST_PARAMS}
  DEPENDS ${M2LANG_TEST_DEPS}
)

# Add a legacy target spelling: m2lang-test
add_custom_target(m2lang-test)
add_dependencies(m2lang-test check-m2lang)
set_target_properties(m2lang-test PROPERTIES FOLDER "M2lang tests")

# FIXME: This logic can be removed once all buildbots have moved
# debuginfo-test from m2lang/test to llvm/projects or monorepo.
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/debuginfo-tests)
  message(WARNING "Including debuginfo-tests in m2lang/test is deprecated.  Move to llvm/projects or use monorepo.")
  if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/debuginfo-tests/CMakeLists.txt)
    add_subdirectory(debuginfo-tests)
  endif()
endif()
