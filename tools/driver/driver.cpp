//===--- driver.cpp - m2lang compiler driver --------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the m2lang compiler driver.
///
//===----------------------------------------------------------------------===//

#include "m2lang/Lexer/Lexer.h"
#include "m2lang/Parser/Parser.h"
#include "m2lang/Sema/Sema.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/TargetSelect.h"

using namespace m2lang;

int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);
  llvm::SmallVector<const char *, 256> argv(argv_ + 1, argv_ + argc_);

  llvm::InitializeAllTargets();

  LangOptions langOpts;
  langOpts.PIM = 1;

  for (const char *F : argv) {
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr =
        llvm::MemoryBuffer::getFile(F);
    if (std::error_code BufferError = FileOrErr.getError()) {
      llvm::errs() << "Error reading " << F << ": " << BufferError.message()
                   << "\n";
    }

    llvm::SourceMgr SrcMgr;
    DiagnosticsEngine Diags(SrcMgr);

    // Tell SrcMgr about this buffer, which is what the
    // parser will pick up.
    SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

    auto lexer = Lexer(SrcMgr, Diags, langOpts);
    auto sema = Sema(Diags);
    auto parser = Parser(lexer, sema);
    parser.parse();
  }
}