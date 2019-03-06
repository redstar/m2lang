cmake_minimum_required(VERSION 3.4.3)

if(POLICY CMP0075)
  cmake_policy(SET CMP0075 NEW)
endif()

# If we are not building as a part of LLVM, build M2lang as an
# standalone project, using LLVM as an external library:
if( CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR )
  project(M2lang)

  # Rely on llvm-config.
  set(CONFIG_OUTPUT)
  if(LLVM_CONFIG)
    set (LLVM_CONFIG_FOUND 1)
    message(STATUS "Found LLVM_CONFIG as ${LLVM_CONFIG}")
    message(DEPRECATION "Using llvm-config to detect the LLVM installation is \
          deprecated.  The installed cmake files should be used \
          instead.  CMake should be able to detect your LLVM install \
          automatically, but you can also use LLVM_DIR to specify \
          the path containing LLVMConfig.cmake.")
    set(CONFIG_COMMAND ${LLVM_CONFIG}
      "--assertion-mode"
      "--bindir"
      "--libdir"
      "--includedir"
      "--prefix"
      "--src-root"
      "--cmakedir")
    execute_process(
      COMMAND ${CONFIG_COMMAND}
      RESULT_VARIABLE HAD_ERROR
      OUTPUT_VARIABLE CONFIG_OUTPUT
    )
    if(NOT HAD_ERROR)
      string(REGEX REPLACE
        "[ \t]*[\r\n]+[ \t]*" ";"
        CONFIG_OUTPUT ${CONFIG_OUTPUT})
    else()
      string(REPLACE ";" " " CONFIG_COMMAND_STR "${CONFIG_COMMAND}")
      message(STATUS "${CONFIG_COMMAND_STR}")
      message(FATAL_ERROR "llvm-config failed with status ${HAD_ERROR}")
    endif()

    list(GET CONFIG_OUTPUT 0 ENABLE_ASSERTIONS)
    list(GET CONFIG_OUTPUT 1 TOOLS_BINARY_DIR)
    list(GET CONFIG_OUTPUT 2 LIBRARY_DIR)
    list(GET CONFIG_OUTPUT 3 INCLUDE_DIR)
    list(GET CONFIG_OUTPUT 4 LLVM_OBJ_ROOT)
    list(GET CONFIG_OUTPUT 5 MAIN_SRC_DIR)
    list(GET CONFIG_OUTPUT 6 LLVM_CONFIG_CMAKE_PATH)

    # Normalize LLVM_CMAKE_PATH. --cmakedir might contain backslashes.
    # CMake assumes slashes as PATH.
    file(TO_CMAKE_PATH ${LLVM_CONFIG_CMAKE_PATH} LLVM_CMAKE_PATH)
  endif()


  if(NOT MSVC_IDE)
    set(LLVM_ENABLE_ASSERTIONS ${ENABLE_ASSERTIONS}
      CACHE BOOL "Enable assertions")
    # Assertions should follow llvm-config's.
    mark_as_advanced(LLVM_ENABLE_ASSERTIONS)
  endif()

  find_package(LLVM REQUIRED HINTS "${LLVM_CMAKE_PATH}")
  list(APPEND CMAKE_MODULE_PATH ${LLVM_DIR})

  # We can't check LLVM_CONFIG here, because find_package(LLVM ...) also sets
  # LLVM_CONFIG.
  if (NOT LLVM_CONFIG_FOUND)
    # Pull values from LLVMConfig.cmake.  We can drop this once the llvm-config
    # path is removed.
    set(TOOLS_BINARY_DIR ${LLVM_TOOLS_BINARY_DIR})
    set(LIBRARY_DIR ${LLVM_LIBRARY_DIR})
    set(INCLUDE_DIR ${LLVM_INCLUDE_DIR})
    set(LLVM_OBJ_DIR ${LLVM_BINARY_DIR})
    # The LLVM_CMAKE_PATH variable is set when doing non-standalone builds and
    # used in this project, so we need to make sure we set this value.
    # FIXME: LLVM_CMAKE_DIR comes from LLVMConfig.cmake.  We should rename
    # LLVM_CMAKE_PATH to LLVM_CMAKE_DIR throughout the project.
    set(LLVM_CMAKE_PATH ${LLVM_CMAKE_DIR})
  endif()

  set(LLVM_TOOLS_BINARY_DIR ${TOOLS_BINARY_DIR} CACHE PATH "Path to llvm/bin")
  set(LLVM_LIBRARY_DIR ${LIBRARY_DIR} CACHE PATH "Path to llvm/lib")
  set(LLVM_MAIN_INCLUDE_DIR ${INCLUDE_DIR} CACHE PATH "Path to llvm/include")
  set(LLVM_BINARY_DIR ${LLVM_OBJ_ROOT} CACHE PATH "Path to LLVM build tree")
  set(LLVM_MAIN_SRC_DIR ${MAIN_SRC_DIR} CACHE PATH "Path to LLVM source tree")

  find_program(LLVM_TABLEGEN_EXE "llvm-tblgen" ${LLVM_TOOLS_BINARY_DIR}
    NO_DEFAULT_PATH)

  # They are used as destination of target generators.
  set(LLVM_RUNTIME_OUTPUT_INTDIR ${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bin)
  set(LLVM_LIBRARY_OUTPUT_INTDIR ${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib${LLVM_LIBDIR_SUFFIX})
  if(WIN32 OR CYGWIN)
    # DLL platform -- put DLLs into bin.
    set(LLVM_SHLIB_OUTPUT_INTDIR ${LLVM_RUNTIME_OUTPUT_INTDIR})
  else()
    set(LLVM_SHLIB_OUTPUT_INTDIR ${LLVM_LIBRARY_OUTPUT_INTDIR})
  endif()

  option(LLVM_ENABLE_WARNINGS "Enable compiler warnings." ON)
  option(LLVM_INSTALL_TOOLCHAIN_ONLY
    "Only include toolchain files in the 'install' target." OFF)

  option(LLVM_FORCE_USE_OLD_HOST_TOOLCHAIN
    "Set to ON to force using an old, unsupported host toolchain." OFF)

  include(AddLLVM)
  include(TableGen)
  include(HandleLLVMOptions)
  include(VersionFromVCS)

  set(PACKAGE_VERSION "${LLVM_PACKAGE_VERSION}")

  if (NOT DEFINED LLVM_INCLUDE_TESTS)
    set(LLVM_INCLUDE_TESTS ON)
  endif()

  include_directories("${LLVM_BINARY_DIR}/include" "${LLVM_MAIN_INCLUDE_DIR}")
  link_directories("${LLVM_LIBRARY_DIR}")

  set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
  set( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib${LLVM_LIBDIR_SUFFIX} )
  set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib${LLVM_LIBDIR_SUFFIX} )

  if(LLVM_INCLUDE_TESTS)
    set(Python_ADDITIONAL_VERSIONS 2.7)
    include(FindPythonInterp)
    if(NOT PYTHONINTERP_FOUND)
      message(FATAL_ERROR
"Unable to find Python interpreter, required for builds and testing.

Please install Python or specify the PYTHON_EXECUTABLE CMake variable.")
    endif()

    if( ${PYTHON_VERSION_STRING} VERSION_LESS 2.7 )
      message(FATAL_ERROR "Python 2.7 or newer is required")
    endif()

    # Check prebuilt llvm/utils.
    if(EXISTS ${LLVM_TOOLS_BINARY_DIR}/FileCheck${CMAKE_EXECUTABLE_SUFFIX}
        AND EXISTS ${LLVM_TOOLS_BINARY_DIR}/count${CMAKE_EXECUTABLE_SUFFIX}
        AND EXISTS ${LLVM_TOOLS_BINARY_DIR}/not${CMAKE_EXECUTABLE_SUFFIX})
      set(LLVM_UTILS_PROVIDED ON)
    endif()

    if(EXISTS ${LLVM_MAIN_SRC_DIR}/utils/lit/lit.py)
      # Note: path not really used, except for checking if lit was found
      set(LLVM_LIT ${LLVM_MAIN_SRC_DIR}/utils/lit/lit.py)
      if(EXISTS ${LLVM_MAIN_SRC_DIR}/utils/llvm-lit)
        add_subdirectory(${LLVM_MAIN_SRC_DIR}/utils/llvm-lit utils/llvm-lit)
      endif()
      if(NOT LLVM_UTILS_PROVIDED)
        add_subdirectory(${LLVM_MAIN_SRC_DIR}/utils/FileCheck utils/FileCheck)
        add_subdirectory(${LLVM_MAIN_SRC_DIR}/utils/count utils/count)
        add_subdirectory(${LLVM_MAIN_SRC_DIR}/utils/not utils/not)
        set(LLVM_UTILS_PROVIDED ON)
        set(CLANG_TEST_DEPS FileCheck count not)
      endif()
      set(UNITTEST_DIR ${LLVM_MAIN_SRC_DIR}/utils/unittest)
      if(EXISTS ${UNITTEST_DIR}/googletest/include/gtest/gtest.h
          AND NOT EXISTS ${LLVM_LIBRARY_DIR}/${CMAKE_STATIC_LIBRARY_PREFIX}gtest${CMAKE_STATIC_LIBRARY_SUFFIX}
          AND EXISTS ${UNITTEST_DIR}/CMakeLists.txt)
        add_subdirectory(${UNITTEST_DIR} utils/unittest)
      endif()
    else()
      # Seek installed Lit.
      find_program(LLVM_LIT
                   NAMES llvm-lit lit.py lit
                   PATHS "${LLVM_MAIN_SRC_DIR}/utils/lit"
                   DOC "Path to lit.py")
    endif()

    if(LLVM_LIT)
      # Define the default arguments to use with 'lit', and an option for the user
      # to override.
      set(LIT_ARGS_DEFAULT "-sv")
      if (MSVC OR XCODE)
        set(LIT_ARGS_DEFAULT "${LIT_ARGS_DEFAULT} --no-progress-bar")
      endif()
      set(LLVM_LIT_ARGS "${LIT_ARGS_DEFAULT}" CACHE STRING "Default options for lit")

      # On Win32 hosts, provide an option to specify the path to the GnuWin32 tools.
      if( WIN32 AND NOT CYGWIN )
        set(LLVM_LIT_TOOLS_DIR "" CACHE PATH "Path to GnuWin32 tools")
      endif()
    else()
      set(LLVM_INCLUDE_TESTS OFF)
    endif()
  endif()

  set( M2LANG_BUILT_STANDALONE 1 )
  set(BACKEND_PACKAGE_STRING "LLVM ${LLVM_PACKAGE_VERSION}")
else()
  set(BACKEND_PACKAGE_STRING "${PACKAGE_STRING}")
endif()

# Make sure that our source directory is on the current cmake module path so that
# we can include cmake files from this directory.
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

include(CheckIncludeFile)
check_include_file(sys/resource.h CLANG_HAVE_RLIMITS)

set(M2LANG_RESOURCE_DIR "" CACHE STRING
  "Relative directory from the M2lang binary to its resource files.")

set(M2_MODULE_DIRS "" CACHE STRING
  "Colon separated list of directories m2lang will search for definition modules.")

set(GCC_INSTALL_PREFIX "" CACHE PATH "Directory where gcc is installed." )
set(DEFAULT_SYSROOT "" CACHE PATH
  "Default <path> to all compiler invocations for --sysroot=<path>." )

set(ENABLE_LINKER_BUILD_ID OFF CACHE BOOL "pass --build-id to ld")

set(M2LANG_VENDOR ${PACKAGE_VENDOR} CACHE STRING
  "Vendor-specific text for showing with version information.")

if( M2LANG_VENDOR )
  add_definitions( -DM2LANG_VENDOR="${M2LANG_VENDOR} " )
endif()

set(M2LANG_REPOSITORY_STRING "" CACHE STRING
  "Vendor-specific text for showing the repository the source is taken from.")

if(M2LANG_REPOSITORY_STRING)
  add_definitions(-DM2LANG_REPOSITORY_STRING="${M2LANG_REPOSITORY_STRING}")
endif()

# The libdir suffix must exactly match whatever LLVM's configuration used.
set(M2LANG_LIBDIR_SUFFIX "${LLVM_LIBDIR_SUFFIX}")

set(M2LANG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(M2LANG_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

if( CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR AND NOT MSVC_IDE )
  message(FATAL_ERROR "In-source builds are not allowed. "
"Please create a directory and run cmake "
"from there, passing the path to this source directory as the last argument. "
"This process created the file `CMakeCache.txt' and the directory "
"`CMakeFiles'. Please delete them.")
endif()

# If M2LANG_VERSION_* is specified, use it, if not use LLVM_VERSION_*.
if(NOT DEFINED M2LANG_VERSION_MAJOR)
  set(M2LANG_VERSION_MAJOR ${LLVM_VERSION_MAJOR})
endif()
if(NOT DEFINED M2LANG_VERSION_MINOR)
  set(M2LANG_VERSION_MINOR ${LLVM_VERSION_MINOR})
endif()
if(NOT DEFINED M2LANG_VERSION_PATCHLEVEL)
  set(M2LANG_VERSION_PATCHLEVEL ${LLVM_VERSION_PATCH})
endif()
# Unlike PACKAGE_VERSION, CLANG_VERSION does not include LLVM_VERSION_SUFFIX.
set(M2LANG_VERSION "${M2LANG_VERSION_MAJOR}.${M2LANG_VERSION_MINOR}.${M2LANG_VERSION_PATCHLEVEL}")
message(STATUS "M2lang version: ${M2LANG_VERSION}")

# Configure the Version.inc file.
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/include/m2lang/Version.inc.in
  ${CMAKE_CURRENT_BINARY_DIR}/include/m2lang/Version.inc)

# Add appropriate flags for GCC
if (LLVM_COMPILER_IS_GCC_COMPATIBLE)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-common -Woverloaded-virtual")
  if (NOT "${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-strict-aliasing")
  endif ()

  # Enable -pedantic for Clang even if it's not enabled for LLVM.
  if (NOT LLVM_ENABLE_PEDANTIC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic -Wno-long-long")
  endif ()

  check_cxx_compiler_flag("-Werror -Wnested-anon-types" CXX_SUPPORTS_NO_NESTED_ANON_TYPES_FLAG)
  if( CXX_SUPPORTS_NO_NESTED_ANON_TYPES_FLAG )
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-nested-anon-types" )
  endif()
endif ()

# Determine HOST_LINK_VERSION on Darwin.
set(HOST_LINK_VERSION)
if (APPLE)
  set(LD_V_OUTPUT)
  execute_process(
    COMMAND sh -c "${CMAKE_LINKER} -v 2>&1 | head -1"
    RESULT_VARIABLE HAD_ERROR
    OUTPUT_VARIABLE LD_V_OUTPUT
  )
  if (NOT HAD_ERROR)
    if ("${LD_V_OUTPUT}" MATCHES ".*ld64-([0-9.]+).*")
      string(REGEX REPLACE ".*ld64-([0-9.]+).*" "\\1" HOST_LINK_VERSION ${LD_V_OUTPUT})
    elseif ("${LD_V_OUTPUT}" MATCHES "[^0-9]*([0-9.]+).*")
      string(REGEX REPLACE "[^0-9]*([0-9.]+).*" "\\1" HOST_LINK_VERSION ${LD_V_OUTPUT})
    endif()
  else()
    message(FATAL_ERROR "${CMAKE_LINKER} failed with status ${HAD_ERROR}")
  endif()
endif()

include(CMakeParseArguments)
include(AddM2lang)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

include_directories(BEFORE
  ${CMAKE_CURRENT_BINARY_DIR}/include
  ${CMAKE_CURRENT_SOURCE_DIR}/include
  )

add_definitions( -D_GNU_SOURCE )

# m2lang version information
set(M2LANG_EXECUTABLE_VERSION
    "${M2LANG_VERSION_MAJOR}" CACHE STRING
    "Major version number that will be appended to the m2lang executable name")
set(LIBM2LANG_LIBRARY_VERSION
    "${M2LANG_VERSION_MAJOR}" CACHE STRING
    "Major version number that will be appended to the libm2lang library")
mark_as_advanced(M2LANG_EXECUTABLE_VERSION LIBM2LANG_LIBRARY_VERSION)

option(M2LANG_INCLUDE_TESTS
       "Generate build targets for the M2lang unit tests."
       ${LLVM_INCLUDE_TESTS})

#add_subdirectory(utils/TableGen)

add_subdirectory(include)

# All targets below may depend on all tablegen'd files.
get_property(M2LANG_TABLEGEN_TARGETS GLOBAL PROPERTY M2LANG_TABLEGEN_TARGETS)
add_custom_target(m2lang-tablegen-targets DEPENDS ${M2LANG_TABLEGEN_TARGETS})
set_target_properties(m2lang-tablegen-targets PROPERTIES FOLDER "Misc")
list(APPEND LLVM_COMMON_DEPENDS m2lang-tablegen-targets)

# Force target to be built as soon as possible. M2lang modules builds depend
# header-wise on it as they ship all headers from the umbrella folders. Building
# an entire module might include header, which depends on intrinsics_gen.
if(LLVM_ENABLE_MODULES AND NOT M2LANG_BUILT_STANDALONE)
  list(APPEND LLVM_COMMON_DEPENDS intrinsics_gen)
endif()

add_subdirectory(lib)
#add_subdirectory(tools)
#add_subdirectory(runtime)

if(APPLE)
  # this line is needed as a cleanup to ensure that any CMakeCaches with the old
  # default value get updated to the new default.
  if(M2LANG_ORDER_FILE STREQUAL "")
    unset(M2LANG_ORDER_FILE CACHE)
    unset(M2LANG_ORDER_FILE)
  endif()


  set(M2LANG_ORDER_FILE ${CMAKE_CURRENT_BINARY_DIR}/m2lang.order CACHE FILEPATH
    "Order file to use when compiling m2lang in order to improve startup time (Darwin Only - requires ld64).")

  if(NOT EXISTS ${M2LANG_ORDER_FILE})
    string(FIND "${M2LANG_ORDER_FILE}" "${CMAKE_CURRENT_BINARY_DIR}" PATH_START)
    if(PATH_START EQUAL 0)
      file(WRITE ${M2LANG_ORDER_FILE} "\n")
    else()
      message(FATAL_ERROR "Specified order file '${M2LANG_ORDER_FILE}' does not exist.")
    endif()
  endif()
endif()

if( M2LANG_INCLUDE_TESTS )
  if(EXISTS ${LLVM_MAIN_SRC_DIR}/utils/unittest/googletest/include/gtest/gtest.h)
    add_subdirectory(unittests)
    list(APPEND M2LANG_TEST_DEPS M2langUnitTests)
    list(APPEND M2LANG_TEST_PARAMS
      m2lang_unit_site_config=${CMAKE_CURRENT_BINARY_DIR}/test/Unit/lit.site.cfg
      )
  else()
    # Use our own gtest. This requires an adjustment of function add_unittest.
    function(add_unittest test_suite test_name)
      set(LLVM_MAIN_SRC_DIR ${CMAKE_SOURCE_DIR})
      _add_unittest(${ARGV})
    endfunction()
    add_subdirectory(utils/unittest)
    add_subdirectory(unittests)
    list(APPEND M2LANG_TEST_DEPS M2langUnitTests)
    list(APPEND M2LANG_TEST_PARAMS
      m2lang_unit_site_config=${CMAKE_CURRENT_BINARY_DIR}/test/Unit/lit.site.cfg
      )
  endif()
  add_subdirectory(test)

  if(M2LANG_BUILT_STANDALONE)
    # Add a global check rule now that all subdirectories have been traversed
    # and we know the total set of lit testsuites.
    get_property(LLVM_LIT_TESTSUITES GLOBAL PROPERTY LLVM_LIT_TESTSUITES)
    get_property(LLVM_LIT_PARAMS GLOBAL PROPERTY LLVM_LIT_PARAMS)
    get_property(LLVM_LIT_DEPENDS GLOBAL PROPERTY LLVM_LIT_DEPENDS)
    get_property(LLVM_LIT_EXTRA_ARGS GLOBAL PROPERTY LLVM_LIT_EXTRA_ARGS)
    get_property(LLVM_ADDITIONAL_TEST_TARGETS
                 GLOBAL PROPERTY LLVM_ADDITIONAL_TEST_TARGETS)
    add_lit_target(check-all
      "Running all regression tests"
      ${LLVM_LIT_TESTSUITES}
      PARAMS ${LLVM_LIT_PARAMS}
      DEPENDS ${LLVM_LIT_DEPENDS} ${LLVM_ADDITIONAL_TEST_TARGETS}
      ARGS ${LLVM_LIT_EXTRA_ARGS}
      )
  endif()
endif()

option(M2LANG_INCLUDE_DOCS "Generate build targets for the M2lang docs."
  ${LLVM_INCLUDE_DOCS})
if( M2LANG_INCLUDE_DOCS )
  add_subdirectory(docs)
endif()

# Custom target to install all clang libraries.
add_custom_target(m2lang-libraries)
set_target_properties(m2lang-libraries PROPERTIES FOLDER "Misc")

if(NOT LLVM_ENABLE_IDE)
  add_llvm_install_targets(install-m2lang-libraries
                           DEPENDS m2lang-libraries
                           COMPONENT m2lang-libraries)
endif()

get_property(M2LANG_LIBS GLOBAL PROPERTY M2LANG_LIBS)
if(M2LANG_LIBS)
  list(REMOVE_DUPLICATES M2LANG_LIBS)
  foreach(lib ${M2LANG_LIBS})
    add_dependencies(m2lang-libraries ${lib})
    if(NOT LLVM_ENABLE_IDE)
      add_dependencies(install-m2lang-libraries install-${lib})
    endif()
  endforeach()
endif()

add_subdirectory(cmake/modules)
