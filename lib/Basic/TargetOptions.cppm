//===--- TargetOptions.cppm - M2 Language Family Target Options -----------===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the m2lang::TargetOptions class.
///
//===----------------------------------------------------------------------===//

module;

#include <string>
#include <vector>

export module m2lang.basic:TargetOptions;

namespace m2lang {
/// Options for controlling the target.
export class TargetOptions {
public:
  /// The name of the target triple to compile for.
  std::string Triple;

  /// When compiling for the device side, contains the triple used to compile
  /// for the host.
  std::string HostTriple;

  /// If given, the name of the target CPU to generate code for.
  std::string CPU;

  /// The list of target specific features to enable or disable, as written on the command line.
  std::vector<std::string> FeaturesAsWritten;

  /// The list of target specific features to enable or disable -- this should
  /// be a list of strings starting with by '+' or '-'.
  std::vector<std::string> Features;

  // The code model to be used as specified by the user. Corresponds to
  // CodeModel::Model enum defined in include/llvm/Support/CodeGen.h, plus
  // "default" for the case when the user has not explicitly specified a
  // code model.
  std::string CodeModel;
};

} // namespace m2lang
