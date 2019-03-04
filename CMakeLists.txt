cmake_minimum_required (VERSION 3.8)

if(POLICY CMP0075)
  cmake_policy(SET CMP0075 NEW)
endif()

project ("m2lang")

include(CheckIncludeFile)
include(CheckIncludeFileCXX)
include(CheckLibraryExists)
include(CheckCXXCompilerFlag)

# Some helper functions
function(append value)
  foreach(variable ${ARGN})
    set(${variable} "${${variable}} ${value}" PARENT_SCOPE)
  endforeach(variable)
endfunction()

# Locate LLVM
find_package(LLVM REQUIRED CONFIG)
message("Found LLVM ${LLVM_PACKAGE_VERSION}, build type ${LLVM_BUILD_TYPE}")

# Remove LTO from end of list of libraries
# Is this component available on Linux?
string(REGEX REPLACE ";LTO$" "" LLVM_AVAILABLE_LIBS "${LLVM_AVAILABLE_LIBS}")

# Information about which targets LLVM was built to target
foreach(LLVM_SUPPORTED_TARGET ${LLVM_TARGETS_TO_BUILD})
    add_definitions("-DM2LANG_LLVM_SUPPORTED_TARGET_${LLVM_SUPPORTED_TARGET}=1")
endforeach()
add_definitions("-DM2LANG_LLVM_TARGET_TRIPLE=\"${TARGET_TRIPLE}\"")

set(LIB_SUFFIX "" CACHE STRING "Appended to the library installation directory. Set to '64' to install libraries into ${PREFIX}/lib64.")

include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
include_directories(${PROJECT_SOURCE_DIR}/include)

llvm_map_components_to_libnames(llvm_libs all)

file(GLOB_RECURSE M2CC_SRC ${PROJECT_SOURCE_DIR}/lib/**.cpp)
file(GLOB_RECURSE M2CC_HDR_H ${PROJECT_SOURCE_DIR}/include/**.h)
file(GLOB_RECURSE M2CC_HDR_DEF ${PROJECT_SOURCE_DIR}/include/**.def)
set(M2CC_HDR ${M2CC_HDR_H} ${M2CC_HDR_DEF})
message("Source: ${M2CC_SRC}")
message("Headers: ${M2CC_HDR}")

source_group("Source Files\\m2cc" FILES ${M2CC_SRC})
source_group("Header Files\\m2cc" FILES ${M2CC_HDR})

if(MSVC)
  if (LLVM_BUILD_TYPE STREQUAL "Debug")
    append("/MTd" CMAKE_C_FLAGS CMAKE_CXX_FLAGS)
  endif()
  append("/Zc:inline" CMAKE_C_FLAGS CMAKE_CXX_FLAGS)

  # "Generate Intrinsic Functions".
  append("/Oi" CMAKE_C_FLAGS CMAKE_CXX_FLAGS)

  # "Enforce type conversion rules".
  append("/Zc:rvalueCast" CMAKE_CXX_FLAGS)
else()
    check_cxx_compiler_flag("-std=c++11" CXX_SUPPORTS_CXX11)
    if (CXX_SUPPORTS_CXX11)
      if (CYGWIN OR MINGW)
        # MinGW and Cygwin are a bit stricter and lack things like
        # 'strdup', 'stricmp', etc in c++11 mode.
        append("-std=gnu++11" CMAKE_CXX_FLAGS)
      else()
        append("-std=c++11" CMAKE_CXX_FLAGS)
      endif()
    else()
      message(FATAL_ERROR "LLVM requires C++11 support but the '-std=c++11' flag isn't supported.")
    endif()
endif()

set(M2CC_LIB m2cc)
add_library(${M2CC_LIB} STATIC ${M2CC_SRC} ${M2CC_HDR})
set_target_properties(
    ${M2CC_LIB} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin
    LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib${LIB_SUFFIX}
    ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib${LIB_SUFFIX}
    ARCHIVE_OUTPUT_NAME ${M2CC_LIB}
    LIBRARY_OUTPUT_NAME ${M2CC_LIB}
    RUNTIME_OUTPUT_NAME ${M2CC_LIB}
    COMPILE_FLAGS "${LLVM_CXXFLAGS} ${LLVM_DEFINITIONS}"
    LINK_FLAGS ""
)
