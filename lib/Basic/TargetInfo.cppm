//===--- TargetInfo.cppm - Target-specific information --------------------===//
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

module;

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/TargetParser/Triple.h"
#include <string>

export module m2lang.basic:TargetInfo;

import :Config;

namespace m2lang {

export class TargetInfo {
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

using namespace m2lang;

namespace {
class AArch64TargetInfo : public TargetInfo {
public:
  AArch64TargetInfo(llvm::Triple Triple) : TargetInfo(Triple) {
    DataLayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i8:8:32-i16:16:32-"
                 "i64:64-i128:128-n32:64-S128-Fn32";
    IsBigEndian = false;
  }

  const llvm::ArrayRef<std::pair<llvm::StringRef, llvm::StringRef>>
  getTargetDefines() const override {
    return {{"ARCH", "aarch64"}};
  }
};

class ARMTargetInfo : public TargetInfo {
public:
  ARMTargetInfo(llvm::Triple Triple) : TargetInfo(Triple) {
    DataLayout = "e-m:e-p:32:32-Fi8-i64:64-v128:64:128-a:0:32-n32-S64";
    IsBigEndian = false;
  }

  const llvm::ArrayRef<std::pair<llvm::StringRef, llvm::StringRef>>
  getTargetDefines() const override {
    return {{"ARCH", "arm"}};
  }
};

class PowerPCTargetInfo : public TargetInfo {
public:
  PowerPCTargetInfo(llvm::Triple Triple) : TargetInfo(Triple) {
    // ppc64le.
    DataLayout = "e-m:e-Fn32-i64:64-i128:128-n32:64";
    IsBigEndian = false;
  }

  const llvm::ArrayRef<std::pair<llvm::StringRef, llvm::StringRef>>
  getTargetDefines() const override {
    return {{"ARCH", "ppc64le"}};
  }
};

class SystemZTargetInfo : public TargetInfo {
public:
  SystemZTargetInfo(llvm::Triple Triple) : TargetInfo(Triple) {
    DataLayout = "E-m:e-i1:8:16-i8:8:16-i64:64-f128:64"
                 "-v128:64-a:8:16-n32:64";
    IsBigEndian = true;
  }

  const llvm::ArrayRef<std::pair<llvm::StringRef, llvm::StringRef>>
  getTargetDefines() const override {
    return {{"ARCH", "systemz"}};
  }
};

class X86TargetInfo : public TargetInfo {
public:
  X86TargetInfo(llvm::Triple Triple) : TargetInfo(Triple) {
    DataLayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:"
                 "64-i128:128-f80:128-n8:16:32:64-S128";
    IsBigEndian = false;
  }

  const llvm::ArrayRef<std::pair<llvm::StringRef, llvm::StringRef>>
  getTargetDefines() const override {
    return {{"ARCH", "x86_64"}};
  }
};
} // namespace

TargetInfo::~TargetInfo() = default;

TargetInfo *TargetInfo::get(llvm::Triple Triple) {
  switch (Triple.getArch()) {
  case llvm::Triple::aarch64:
    if constexpr (cfg::hasAArch64Target) {
      if (Triple.isOSLinux())
        return new AArch64TargetInfo(Triple);
    }
    break;
  case llvm::Triple::arm:
    if constexpr (cfg::hasARMTarget) {
      if (Triple.isOSLinux())
        return new ARMTargetInfo(Triple);
    }
    break;
  case llvm::Triple::ppc64le:
    if constexpr (cfg::hasPowerPCTarget) {
      if (Triple.isOSLinux())
        return new PowerPCTargetInfo(Triple);
    }
    break;
  case llvm::Triple::systemz:
    if constexpr (cfg::hasSystemZTarget) {
      if (Triple.isOSLinux())
        return new SystemZTargetInfo(Triple);
    }
    break;
  case llvm::Triple::x86_64:
    if constexpr (cfg::hasX86Target) {
      if (Triple.isOSLinux())
        return new X86TargetInfo(Triple);
    }
    break;
  default:
    break;
  }
  return nullptr;
}
