//===- Version.h - M2lang Version Number -------------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines version macros and version-related utility functions
/// for M2lang.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_M2LANG_BASIC_VERSION_H
#define LLVM_M2LANG_BASIC_VERSION_H

#include "m2lang/Basic/Version.inc"
#include "llvm/ADT/StringRef.h"

namespace m2lang {
  /// Retrieves the repository path (e.g., Subversion path) that
  /// identifies the particular M2lang branch, tag, or trunk from which this
  /// M2lang was built.
  std::string getM2langRepositoryPath();

  /// Retrieves the repository path from which LLVM was built.
  ///
  /// This supports LLVM residing in a separate repository from clang.
  std::string getLLVMRepositoryPath();

  /// Retrieves the repository revision number (or identifier) from which
  /// this M2lang was built.
  std::string getM2langRevision();

  /// Retrieves the repository revision number (or identifier) from which
  /// LLVM was built.
  ///
  /// If M2lang and LLVM are in the same repository, this returns the same
  /// string as getM2langRevision.
  std::string getLLVMRevision();

  /// Retrieves the full repository version that is an amalgamation of
  /// the information in getM2langRepositoryPath() and getM2langRevision().
  std::string getM2langFullRepositoryVersion();

  /// Retrieves a string representing the complete clang version,
  /// which includes the clang version number, the repository version,
  /// and the vendor tag.
  std::string getM2langFullVersion();

  /// Like getM2langFullVersion(), but with a custom tool name.
  std::string getM2langToolFullVersion(llvm::StringRef ToolName);

  /// Retrieves a string representing the complete clang version suitable
  /// for use in the CPP __VERSION__ macro, which includes the clang version
  /// number, the repository version, and the vendor tag.
  std::string getM2langFullCPPVersion();
}

#endif // LLVM_M2LANG_BASIC_VERSION_H
