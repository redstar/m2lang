//===--- Config.cppm - Global configuration -------------------------------===//
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

module;

#include "llvm/Support/Compiler.h"

export module m2lang.basic:Config;

export namespace m2lang {
    namespace cfg {
    constexpr bool hasAArch64Target = LLVM_HAS_AARCH64_TARGET;
    constexpr bool hasARMTarget = LLVM_HAS_ARM_TARGET;
    constexpr bool hasPowerPCTarget = LLVM_HAS_POWERPC_TARGET;
    constexpr bool hasSystemZTarget = LLVM_HAS_SYSTEMZ_TARGET;
    constexpr bool hasX86Target = LLVM_HAS_X86_TARGET;
    } // namespace cfg
} // namespace m2lang
