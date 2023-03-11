//===--- LALRtool.cpp - LALRtool driver -------------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements driver of LALRtool.
///
//===----------------------------------------------------------------------===//

#include "lalrtool/Main.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"

int main(int argc, const char **argv) {
  llvm::InitLLVM X(argc, argv);
  llvm::cl::ParseCommandLineOptions(
      argc, argv, "LALRtool - recursive descent parser generator\n");

  return lalrtool::runLALRtoolMain(argv[0]);
}