//===--- LR0Automaton.h - LR(0) automaton used by LALRtool ------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the LR(0) automaton.
///
//===----------------------------------------------------------------------===//

#ifndef LALRTOOL_LR0AUTOMATON_H
#define LALRTOOL_LR0AUTOMATON_H

#include "lalrtool/Grammar.h"

namespace lalrtool {

class LR0Item {
  Rule *R;
  unsigned Dot;

public:
  LR0Item(Rule *R, unsigned Dot) : R(R), Dot(Dot) {}

  Rule *getRule() const { return R; }
  unsigned getDot() const { return Dot; }
};
/*
 * I need:
 * - set of LR0Items
 * - transistions between those sets (marked by terminals?)
 */
class LR0Automaton {};

} // namespace lalrtool
#endif
