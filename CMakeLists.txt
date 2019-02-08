cmake_minimum_required (VERSION 3.8)

project ("m2lang")

include(CheckIncludeFile)
include(CheckIncludeFileCXX)
include(CheckLibraryExists)
include(CheckCXXCompilerFlag)

# Locate LLVM
set(LLVM_DIR "D:/My Program Files/LLVM-x64/lib/cmake/llvm")
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
    add_definitions("/MTd")
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
