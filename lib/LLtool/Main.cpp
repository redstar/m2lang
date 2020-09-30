//===--- Main.cpp - LLtool main entry point ---------------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the main entry point for LLtool.
///
//===----------------------------------------------------------------------===//

#include "lltool/Main.h"
#include "lltool/Algo.h"
#include "lltool/Diagnostic.h"
#include "lltool/Parser.h"
#include "lltool/RDPEmitter.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"

using namespace lltool;

static llvm::cl::opt<std::string>
    OutputFilename("o", llvm::cl::desc("Output filename"),
                   llvm::cl::value_desc("filename"), llvm::cl::init("-"));

static llvm::cl::opt<std::string> InputFilename(llvm::cl::Positional,
                                                llvm::cl::desc("<input file>"),
                                                llvm::cl::init("-"));

static llvm::cl::opt<bool>
    WriteIfChanged("write-if-changed",
                   llvm::cl::desc("Only write output if it changed"));

static int reportError(const char *ProgName, llvm::Twine Msg) {
  llvm::errs() << ProgName << ": " << Msg;
  llvm::errs().flush();
  return 1;
}

int lltool::LLtoolMain(const char *Argv0) {
  // Read the input file.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr =
      llvm::MemoryBuffer::getFileOrSTDIN(InputFilename);
  if (std::error_code EC = FileOrErr.getError())
    return reportError(Argv0, "Could not open input file '" + InputFilename +
                                  "': " + EC.message() + "\n");

  llvm::SourceMgr SrcMgr;

  // Tell SrcMgr about this buffer, which is what the parser will pick up.
  SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

  // Parser the grammar and calculate all LL(1) values.
  Grammar grammar;
  VarStore Vars;
  Parser TheParser(SrcMgr);
  TheParser.parse(grammar, Vars);
  grammar.performAnalysis(TheParser.getDiag());

  // Do not generate output, if syntactically or semantically errors occured.
  if (TheParser.getDiag().errorsOccured())
    return reportError(Argv0, llvm::Twine(TheParser.getDiag().errorsPrinted()) +
                                  " errors.\n");

  // Write output to memory.
  std::string OutString;
  llvm::raw_string_ostream Out(OutString);
  EmitRDP(grammar, Vars, Out);

  if (WriteIfChanged) {
    // Only updates the real output file if there are any differences.
    // This prevents recompilation of all the files depending on it if there
    // aren't any.
    if (auto ExistingOrErr = llvm::MemoryBuffer::getFile(OutputFilename))
      if (std::move(ExistingOrErr.get())->getBuffer() == Out.str())
        return 0;
  }

  std::error_code EC;
  llvm::ToolOutputFile OutFile(OutputFilename, EC, llvm::sys::fs::OF_None);
  if (EC)
    return reportError(Argv0, "error opening " + OutputFilename + ":" +
                                  EC.message() + "\n");
  OutFile.os() << Out.str();

  // Declare success.
  OutFile.keep();
  return 0;
}