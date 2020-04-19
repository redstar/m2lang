//===--- SourceLocation.h - Defines locaction in source ---------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the type of a source location.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_BASIC_SOURCELOCATION_H
#define M2LANG_BASIC_SOURCELOCATION_H

namespace m2lang {

// A source location is just an offset from the beginning.
using SourceLocation = size_t;

} // namespace m2lang

#endif