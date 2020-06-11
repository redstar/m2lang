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

#include "m2lang/CodeGen/CodeGenerator.h"
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
#include "llvm/Support/WithColor.h"

using namespace m2lang;

static llvm::cl::list<std::string> InputFiles(cl::Positional,
                                              cl::desc("<input-files>"));

static cl::opt<std::string>
    MTriple("mtriple", cl::desc("Override target triple for module"));

static const char *Head = "m2lang - Modula-2 language compiler\n";

void printVersion(llvm::raw_ostream &OS) {
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

llvm::TargetMachine *createTargetMachine(const char *Argv0) {
  llvm::TargetOptions TargetOptions = InitTargetOptionsFromCodeGenFlags();
  std::string CPUStr = getCPUStr();
  std::string FeatureStr = getFeaturesStr();

  llvm::Triple Triple =
      llvm::Triple(!MTriple.empty() ? llvm::Triple::normalize(MTriple)
                                    : llvm::sys::getDefaultTargetTriple());

  std::string Error;
  const llvm::Target *Target =
      llvm::TargetRegistry::lookupTarget(MArch, Triple, Error);

  if (!Target) {
    llvm::WithColor::error(errs(), Argv0) << Error;
    return nullptr;
  }

  llvm::TargetMachine *TM = Target->createTargetMachine(
      Triple.getTriple(), CPUStr, FeatureStr, TargetOptions,
      llvm::Optional<llvm::Reloc::Model>(RelocModel));
  return TM;
}

int main(int Argc, const char **Argv) {
  llvm::InitLLVM X(Argc, Argv);

  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();

  llvm::cl::SetVersionPrinter(&printVersion);
  llvm::cl::ParseCommandLineOptions(Argc, Argv, Head);

  if (MCPU == "help" ||
      std::any_of(MAttrs.begin(), MAttrs.end(),
                  [](const std::string &a) { return a == "help"; })) {
    auto Triple = llvm::Triple(LLVM_DEFAULT_TARGET_TRIPLE);
    std::string ErrMsg;
    if (auto target =
            llvm::TargetRegistry::lookupTarget(Triple.getTriple(), ErrMsg)) {
      llvm::errs() << "Targeting " << target->getName() << ". ";
      // this prints the available CPUs and features of the target to stderr...
      target->createMCSubtargetInfo(Triple.getTriple(), getCPUStr(),
                                    getFeaturesStr());
    } else {
      llvm::errs() << ErrMsg << "\n";
      exit(EXIT_FAILURE);
    }
    exit(EXIT_SUCCESS);
  }

  llvm::TargetMachine *TM = createTargetMachine(Argv[0]);
  if (!TM)
    exit(EXIT_FAILURE);

  LangOptions LangOpts;
  LangOpts.PIM = 1;

  for (const auto F : InputFiles) {
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr =
        llvm::MemoryBuffer::getFile(F);
    if (std::error_code BufferError = FileOrErr.getError()) {
      llvm::WithColor::error(errs(), Argv[0])
          << "Error reading " << F << ": " << BufferError.message() << "\n";
    }

    llvm::SourceMgr SrcMgr;
    DiagnosticsEngine Diags(SrcMgr);

    // Tell SrcMgr about this buffer, which is what the
    // parser will pick up.
    SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

    auto lexer = Lexer(SrcMgr, Diags, LangOpts);
    auto sema = Sema(Diags);
    auto parser = Parser(lexer, sema);
    auto *CM = parser.parse();
    if (CM /*&& !Diags.getNumErrors()*/) {
      if (CodeGenerator *CG = CodeGenerator::create(TM)) {
        CG->run(CM);
        delete CG;
      }
    }
  }
}