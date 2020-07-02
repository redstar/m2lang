//===--- CGUtils.h - Code Generator Utility Functions -----------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef M2LANG_CODEGEN_CGUTILS_H
#define M2LANG_CODEGEN_CGUTILS_H

#include <string>

namespace m2lang {

class Declaration;

namespace utils {

std::string mangleName(Declaration *Decl);

}
} // namespace m2lang
#endif