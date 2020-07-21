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

#include "m2lang/AST/ASTContext.h"
#include "m2lang/CodeGen/CodeGenerator.h"
#include "m2lang/Lexer/Lexer.h"
#include "m2lang/Lexer/Preprocessor.h"
#include "m2lang/Parser/Parser.h"
#include "m2lang/Sema/Sema.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/CommandFlags.inc"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/InitializePasses.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/WithColor.h"

using namespace m2lang;

static llvm::cl::list<std::string> InputFiles(cl::Positional,
                                              cl::desc("<input-files>"));

static cl::opt<std::string> OutputFilename("o", cl::desc("Output filename"),
                                           cl::value_desc("filename"));

static cl::opt<char>
    OptLevel("O",
             cl::desc("Optimization level. [-O0, -O1, -O2, or -O3] "
                      "(default = '-O2')"),
             cl::Prefix, cl::ZeroOrMore, cl::init(' '));

static cl::opt<std::string>
    MTriple("mtriple", cl::desc("Override target triple for module"));

static cl::opt<bool> EmitLLVM("emit-llvm",
                              cl::desc("Emit IR code instead of assembler"),
                              cl::init(false));

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

  CodeGenOpt::Level OLvl = CodeGenOpt::Default;
  switch (OptLevel) {
  default:
    WithColor::error(errs(), Argv0) << "invalid optimization level.\n";
    return nullptr;
  case ' ':
    break;
  case '0':
    OLvl = CodeGenOpt::None;
    break;
  case '1':
    OLvl = CodeGenOpt::Less;
    break;
  case '2':
    OLvl = CodeGenOpt::Default;
    break;
  case '3':
    OLvl = CodeGenOpt::Aggressive;
    break;
  }

  llvm::TargetMachine *TM = Target->createTargetMachine(
      Triple.getTriple(), CPUStr, FeatureStr, TargetOptions, getRelocModel(),
      getCodeModel(), OLvl);
  return TM;
}

bool emit(StringRef Argv0, llvm::Module *M, llvm::TargetMachine *TM,
          StringRef InputFilename) {
  if (OutputFilename.empty()) {
    if (InputFilename == "-") {
      OutputFilename = "-";
    } else {
      if (InputFilename.endswith(".mod") || InputFilename.endswith(".mod"))
        OutputFilename = InputFilename.drop_back(4);
      else
        OutputFilename = InputFilename;
      switch (FileType) {
      case CodeGenFileType::CGFT_AssemblyFile:
        OutputFilename.append(EmitLLVM ? ".ll" : ".s");
        break;
      case CodeGenFileType::CGFT_ObjectFile:
        OutputFilename.append(".o");
        break;
      case CodeGenFileType::CGFT_Null:
        OutputFilename.append(".null");
        break;
      }
    }
  }

  // Open the file.
  std::error_code EC;
  sys::fs::OpenFlags OpenFlags = sys::fs::OF_None;
  if (FileType == CGFT_AssemblyFile)
    OpenFlags |= sys::fs::OF_Text;
  auto Out =
      std::make_unique<llvm::ToolOutputFile>(OutputFilename, EC, OpenFlags);
  if (EC) {
    WithColor::error(errs(), Argv0) << EC.message() << '\n';
    return false;
  }

  legacy::PassManager PM;
  if (FileType == CGFT_AssemblyFile && EmitLLVM) {
    PM.add(createPrintModulePass(Out->os()));
  } else {
    if (TM->addPassesToEmitFile(PM, Out->os(), nullptr, FileType)) {
      WithColor::error() << "TheTargetMachine can't emit a file of this type\n";
      return false;
    }
  }
  PM.run(*M);
  Out->keep();
  return true;
}

int main(int Argc, const char **Argv) {
  llvm::InitLLVM X(Argc, Argv);

  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();

  PassRegistry *Registry = PassRegistry::getPassRegistry();
  initializeCore(*Registry);
  initializeCodeGen(*Registry);

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
  LangOpts.ISO = 1;

  for (const auto &F : InputFiles) {
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
    auto pp = Preprocessor(lexer);
    auto ASTCtx = ASTContext(LangOpts);
    auto sema = Sema(ASTCtx, Diags);
    auto parser = Parser(pp, sema);
    auto *CM = parser.parse();
    if (CM /*&& !Diags.getNumErrors()*/) {
      llvm::LLVMContext Ctx;
      if (CodeGenerator *CG = CodeGenerator::create(Ctx, ASTCtx, TM)) {
        std::unique_ptr<llvm::Module> M = CG->run(CM, F);
        if (!emit(Argv[0], M.get(), TM, F)) {
          llvm::WithColor::error(errs(), Argv[0]) << "Error writing output\n";
        }
        delete CG;
      }
    }
  }
}