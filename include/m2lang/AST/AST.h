//===--- AST.h - M2 Language Family Abstract Syntax Tree --------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the abstract syntax tree classes.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_AST_AST_H
#define M2LANG_AST_AST_H

#include <string>

namespace m2lang {

class Module {
  std::string Name;
  bool IsGeneric;
  int Priority;
public:
  static Module *create();
};

class Expr {
};

class Stmt {
};

class IfStmt : public Stmt {
public:
  static IfStmt *create();
};

class CaseStmt : public Stmt {
public:
  static CaseStmt *create();
};

class WhileStmt : public Stmt {
public:
  static WhileStmt *create();
};

class RepeatStmt : public Stmt {
public:
  static RepeatStmt *create();
};

class ForStmt : public Stmt {
public:
  static ForStmt *create();
};

class LoopStmt : public Stmt {
public:
  static LoopStmt *create();
};

class WithStmt : public Stmt {
public:
  static WithStmt *create();
};

class ExitStmt : public Stmt {
public:
  static ExitStmt *create();
};

class ReturnStmt : public Stmt {
public:
  static ReturnStmt *create();
};

} // namespace m2lang

#endif