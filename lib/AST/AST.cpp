//===--- AST.cpp - Modula-2 Abstract Syntax Tree ----------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the AST implementation.
///
//===----------------------------------------------------------------------===//

#include "m2lang/AST/AST.h"

using namespace m2lang;

Module *Module::create() {
  return nullptr;
}

IfStmt *IfStmt::create() {
  return nullptr;
}

CaseStmt *CaseStmt::create() {
  return nullptr;
}

WhileStmt *WhileStmt::create() {
  return nullptr;
}

RepeatStmt *RepeatStmt::create() {
  return nullptr;
}

ForStmt *ForStmt::create() {
  return nullptr;
}

LoopStmt *LoopStmt::create() {
  return nullptr;
}

WithStmt *WithStmt::create() {
  return nullptr;
}