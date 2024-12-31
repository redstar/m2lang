//===--- Version.cppm - M2lang version numbder ----------------------------===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the m2lang version numbers.
///
//===----------------------------------------------------------------------===//

module;

#include "llvm/ADT/StringRef.h"

export module m2lang.basic:Version;

#include "generated/Basic/Version.inc"

export namespace m2lang {
  /// Retrieves a string representing the complete m2lang version,
  /// which includes the m2lang version number, and the repository version.
  llvm::StringRef getM2langFullVersion() {
    return M2LANG_VERSION;
  }
}
