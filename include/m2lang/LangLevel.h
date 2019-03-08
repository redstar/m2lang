//===--- LangLevel.h - Modula-2 Language Level ------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the BSD 3-Clause License.
// See the LICENSE file for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the language level.
///
/// Three language variants are supported: PIM4, ISO and R10.
///
//===----------------------------------------------------------------------===//

#ifndef M2LANG_LANGLEVEL_H
#define M2LANG_LANGLEVEL_H

#include "llvm/Support/Compiler.h"

namespace m2lang {

  enum LangLevel {
    PIM4,
    ISO,
    R10
  };

  namespace opts {
    // Return the language variant currently used
    LangLevel langLevel();
  } // end namespace opts

  LLVM_READNONE inline bool isPIM4Lang() {
    return opts::langLevel() == PIM4;
  }

  LLVM_READNONE inline bool isISOLang() {
    return opts::langLevel() == ISO;
  }

  LLVM_READNONE inline bool isR10Lang() {
    return opts::langLevel() == R10;
  }

} // end namespace m2lang
#endif
