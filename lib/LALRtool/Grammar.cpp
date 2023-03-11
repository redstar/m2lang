//===--- Grammar.cpp - LALRtool grammar definition --------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
///
///
//===----------------------------------------------------------------------===//

#include "lalrtool/Grammar.h"
#include "lltool/Diagnostic.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/raw_ostream.h"

using namespace lalrtool;

void Grammar::performAnalysis(Diagnostic &Diag) {
}

#define AST_DEFINITION
#include "lalrtool/lalrtool.ast.inc"