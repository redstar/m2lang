//===--- driver.cpp - m2lang compiler driver ------------------------------===//
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

#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/InitializePasses.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/WithColor.h"
#include "llvm/TargetParser/Host.h"
#include <cstdlib>

import m2lang.ast;
import m2lang.basic;
import m2lang.codegen;
import m2lang.lexer;
import m2lang.parser;
import m2lang.sema;

using namespace m2lang;

static llvm::codegen::RegisterCodeGenFlags CGF;

static llvm::cl::list<std::string> InputFiles(llvm::cl::Positional,
                                              llvm::cl::desc("<input-files>"));

static llvm::cl::opt<std::string>
    OutputFilename("o", llvm::cl::desc("Output filename"),
                   llvm::cl::value_desc("filename"));

static llvm::cl::opt<char>
    OptLevel("O",
             llvm::cl::desc("Optimization level. [-O0, -O1, -O2, or -O3] "
                            "(default = '-O2')"),
             llvm::cl::Prefix, llvm::cl::ZeroOrMore, llvm::cl::init(' '));

static llvm::cl::opt<std::string>
    MTriple("mtriple", llvm::cl::desc("Override target triple for module"));

static llvm::cl::opt<bool>
    EmitLLVM("emit-llvm", llvm::cl::desc("Emit IR code instead of assembler"),
             llvm::cl::init(false));

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

llvm::TargetMachine *createTargetMachine(const char *Argv0,
                                         llvm::Triple Triple) {
  llvm::TargetOptions TargetOptions =
      llvm::codegen::InitTargetOptionsFromCodeGenFlags(Triple);
  std::string CPUStr = llvm::codegen::getCPUStr();
  std::string FeatureStr = llvm::codegen::getFeaturesStr();

  std::string Error;
  const llvm::Target *Target = llvm::TargetRegistry::lookupTarget(
      llvm::codegen::getMArch(), Triple, Error);

  if (!Target) {
    llvm::WithColor::error(llvm::errs(), Argv0) << Error;
    return nullptr;
  }

  llvm::CodeGenOptLevel OLvl = llvm::CodeGenOptLevel::Default;
  switch (OptLevel) {
  default:
    llvm::WithColor::error(llvm::errs(), Argv0)
        << "invalid optimization level.\n";
    return nullptr;
  case ' ':
    break;
  case '0':
    OLvl = llvm::CodeGenOptLevel::None;
    break;
  case '1':
    OLvl = llvm::CodeGenOptLevel::Less;
    break;
  case '2':
    OLvl = llvm::CodeGenOptLevel::Default;
    break;
  case '3':
    OLvl = llvm::CodeGenOptLevel::Aggressive;
    break;
  }

  llvm::TargetMachine *TM =
      Target->createTargetMachine(Triple.getTriple(), CPUStr, FeatureStr,
                                  TargetOptions, llvm::codegen::getRelocModel(),
                                  llvm::codegen::getExplicitCodeModel(), OLvl);
  return TM;
}

bool emit(llvm::StringRef Argv0, llvm::Module *M, llvm::TargetMachine *TM,
          llvm::StringRef InputFilename) {
  llvm::CodeGenFileType FileType = llvm::codegen::getFileType();
  if (OutputFilename.empty()) {
    if (InputFilename == "-") {
      OutputFilename = "-";
    } else {
      if (InputFilename.ends_with(".mod") || InputFilename.ends_with(".mod"))
        OutputFilename = InputFilename.drop_back(4).str();
      else
        OutputFilename = InputFilename.str();
      switch (FileType) {
      case llvm::CodeGenFileType::AssemblyFile:
        OutputFilename.append(EmitLLVM ? ".ll" : ".s");
        break;
      case llvm::CodeGenFileType::ObjectFile:
        OutputFilename.append(".o");
        break;
      case llvm::CodeGenFileType::Null:
        OutputFilename.append(".null");
        break;
      }
    }
  }

  // Open the file.
  std::error_code EC;
  llvm::sys::fs::OpenFlags OpenFlags = llvm::sys::fs::OF_None;
  if (FileType == llvm::CodeGenFileType::AssemblyFile)
    OpenFlags |= llvm::sys::fs::OF_Text;
  auto Out =
      std::make_unique<llvm::ToolOutputFile>(OutputFilename, EC, OpenFlags);
  if (EC) {
    llvm::WithColor::error(llvm::errs(), Argv0) << EC.message() << '\n';
    return false;
  }

  llvm::legacy::PassManager PM;
  if (FileType == llvm::CodeGenFileType::AssemblyFile && EmitLLVM) {
    PM.add(createPrintModulePass(Out->os()));
  } else {
    if (TM->addPassesToEmitFile(PM, Out->os(), nullptr, FileType)) {
      llvm::WithColor::error(llvm::errs(), Argv0)
          << "TheTargetMachine can't emit a file of this type\n";
      return false;
    }
  }
  PM.run(*M);
  Out->keep();
  return true;
}

int main(int Argc, const char **Argv) {
  llvm::InitLLVM X(Argc, Argv);

  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();
  llvm::InitializeAllAsmParsers();

  llvm::PassRegistry *Registry = llvm::PassRegistry::getPassRegistry();
  llvm::initializeCore(*Registry);
  llvm::initializeCodeGen(*Registry);

  llvm::cl::SetVersionPrinter(&printVersion);
  llvm::cl::ParseCommandLineOptions(Argc, Argv, Head);

  if (llvm::codegen::getMCPU() == "help" ||
      (!llvm::codegen::getMAttrs().empty() &&
       llvm::codegen::getMAttrs().front() == "help")) {
    auto Triple = llvm::Triple(LLVM_DEFAULT_TARGET_TRIPLE);
    std::string ErrMsg;
    if (auto target =
            llvm::TargetRegistry::lookupTarget(Triple.getTriple(), ErrMsg)) {
      llvm::errs() << "Targeting " << target->getName() << ". ";
      // this prints the available CPUs and features of the target to stderr...
      target->createMCSubtargetInfo(Triple.getTriple(),
                                    llvm::codegen::getCPUStr(),
                                    llvm::codegen::getFeaturesStr());
    } else {
      llvm::errs() << ErrMsg << "\n";
      exit(EXIT_FAILURE);
    }
    exit(EXIT_SUCCESS);
  }

  TargetInfo *TI = TargetInfo::get(llvm::Triple(!MTriple.empty() ? llvm::Triple::normalize(MTriple)
                                    : llvm::sys::getDefaultTargetTriple()));
  if (!TI) {
    llvm::WithColor::error(llvm::errs(), Argv[0])
          << "The requested architecture/platform is not supported\n";
    exit(EXIT_FAILURE);
  }
  llvm::TargetMachine *TM = createTargetMachine(Argv[0], TI->getTriple());
  if (!TM)
    exit(EXIT_FAILURE);

  LangOptions LangOpts;
  LangOpts.ISO = 1;

  for (const auto &F : InputFiles) {
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr =
        llvm::MemoryBuffer::getFile(F);
    if (std::error_code BufferError = FileOrErr.getError()) {
      llvm::WithColor::error(llvm::errs(), Argv[0])
          << "Error reading " << F << ": " << BufferError.message() << "\n";
    }

    llvm::SourceMgr SrcMgr;
    DiagnosticsEngine Diags(SrcMgr);

    // Tell SrcMgr about this buffer, which is what the
    // parser will pick up.
    SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

    auto lexer = Lexer(SrcMgr, Diags, LangOpts);
    auto pp = Preprocessor(lexer, *TI);
    auto ASTCtx = ASTContext(LangOpts, SrcMgr);
    auto sema = Sema(ASTCtx, Diags);
    auto parser = Parser(pp, sema);
    auto *CM = parser.parse();
    if (CM /*&& !Diags.getNumErrors()*/) {
      llvm::LLVMContext Ctx;
      if (CodeGenerator *CG = CodeGenerator::create(Ctx, ASTCtx, TM)) {
        std::unique_ptr<llvm::Module> M = CG->run(CM, F);
        if (!emit(Argv[0], M.get(), TM, F)) {
          llvm::WithColor::error(llvm::errs(), Argv[0])
              << "Error writing output\n";
        }
        delete CG;
      }
    }
  }
}
