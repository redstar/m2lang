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
#include "llvm/CodeGen/CommandFlags.inc"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"

using namespace m2lang;

static llvm::cl::list<std::string> InputFiles(cl::Positional,
                                              cl::desc("<input-files>"));

static const char *Head = "m2lang - Modula-2 language compiler\n";

void PrintVersion(llvm::raw_ostream &OS) {
  OS << Head;
  OS << "  Default target: " << llvm::sys::getDefaultTargetTriple() << "\n";
  std::string CPU(llvm::sys::getHostCPUName());
  OS << "  Host CPU: " << CPU << "\n";
  OS << "  https://github.com/redstar/m2lang\n";
  OS << "\n";
  OS.flush();
  llvm::TargetRegistry::printRegisteredTargetsForVersion(OS);
  exit(EXIT_SUCCESS);
}

int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);
  llvm::SmallVector<const char *, 256> Argv(argv_ + 1, argv_ + argc_);

  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();

  llvm::cl::SetVersionPrinter(&PrintVersion);
  llvm::cl::ParseCommandLineOptions(argc_, argv_, Head);

  if (MCPU == "help" ||
      std::any_of(MAttrs.begin(), MAttrs.end(),
                  [](const std::string &a) { return a == "help"; })) {
    auto Triple = llvm::Triple(LLVM_DEFAULT_TARGET_TRIPLE);
    std::string ErrMsg;
    if (auto target = llvm::TargetRegistry::lookupTarget(Triple.getTriple(), ErrMsg)) {
      llvm::errs() << "Targeting " << target->getName() << ". ";
      // this prints the available CPUs and features of the target to stderr...
      target->createMCSubtargetInfo(Triple.getTriple(), getCPUStr(), getFeaturesStr());
    } else {
      llvm::errs() << ErrMsg << "\n";
      exit(EXIT_FAILURE);
    }
    exit(EXIT_SUCCESS);
  }

  LangOptions langOpts;
  langOpts.PIM = 1;

  for (const auto F : InputFiles) {
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