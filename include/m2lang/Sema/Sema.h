//===--- Sema.h - M2 Language Family Semantic Analyzer ----------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the semantic analyzer classes for Modula-2.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_SEMA_SEMA_H
#define M2LANG_SEMA_SEMA_H

namespace m2lang {

class Sema final {
public:
  void actOnProcedureDecl();
  void actOnTypeDecl();
  void actOnConstantDecl();
  void actOnVariableDecl();
  void actOnIfStmt();
  void actOnCaseStmt();
  void actOnWhileStmt();
  void actOnRepeatStmt();
  void actOnLoopStmt();
  void actOnForStmt();
  void actOnWithStmt();
  void actOnExitStmt();
  void actOnReturnStmt();
};

} // namespace m2lang

#endif