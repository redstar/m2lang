//===--- TargetInfo.h - Target-specific information -------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines target-specific information.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_BASIC_TARGETINFO_H
#define M2LANG_BASIC_TARGETINFO_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/TargetParser/Triple.h"
#include <string>

namespace m2lang {

class TargetInfo {
  llvm::Triple Triple;

protected:
  std::string DataLayout;
  bool IsBigEndian;

  TargetInfo(llvm::Triple Triple) : Triple(Triple) {}

public:
  virtual ~TargetInfo();

  static TargetInfo *get(llvm::Triple);

  const llvm::Triple &getTriple() const { return Triple; }
  const std::string &getDataLayout() const { return DataLayout; }
  virtual const llvm::ArrayRef<std::pair<llvm::StringRef, llvm::StringRef>>
  getTargetDefines() const = 0;
  bool isLittleEndian() const { return !IsBigEndian; }
  bool isBigEndian() const { return IsBigEndian; }
};

} // namespace m2lang

#endif
