//===--- Config.h - Global configuration ------------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines configuration flags.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_BASIC_CONFIG_H
#define M2LANG_BASIC_CONFIG_H

#include "llvm/Support/Compiler.h"

namespace m2lang {
    namespace cfg {
    constexpr bool hasAArch64Target = LLVM_HAS_AARCH64_TARGET;
    constexpr bool hasARMTarget = LLVM_HAS_ARM_TARGET;
    constexpr bool hasPowerPCTarget = LLVM_HAS_POWERPC_TARGET;
    constexpr bool hasSystemZTarget = LLVM_HAS_SYSTEMZ_TARGET;
    constexpr bool hasX86Target = LLVM_HAS_X86_TARGET;
    } // namespace cfg
} // namespace m2lang

#endif
