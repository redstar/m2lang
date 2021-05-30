m2lang - The LLVM-based Modula-2 compiler
=========================================

[![Build Status](https://img.shields.io/travis/com/redstar/m2lang/master.svg?logo=travis&label=Travis%20CI)][1]
[![Build Status](https://img.shields.io/cirrus/github/redstar/m2lang/master?logo=Cirrus%20CI&label=Cirrus%20CI)][2]

Introduction
------------

See this [FOSDEM 2019 talk](https://fosdem.org/2019/schedule/event/llvm_irgen/) for some details.

Implementation decisions
------------------------

 - LLVM is only external dependency
 - CMake scripts derived from clang
 - ISO Modula-2 language first
 - Use "declare-before-use" (single-pass) model
 - Goal is target several platforms from the beginning

Current status
--------------

- The lexer is done.
- The preprocessor (for handling of directives) is is based on the draft
  technical report ["Interfacing Modula-2 to C", Annex B](http://www.zi.biologie.uni-muenchen.de/~enger/SC22WG13/im2c-981130.html#TR-AXI-PRAGMAS)
  and aims to be compatible to the [Macintosh p1 compiler](https://modula2.awiedemann.de/manual/comp4.html#L4_2).
  Parsing of directives is implemented, with mostly no functionality.
- The parser is based on ISO Modula-2 with generic and OO additions.
  Some LL(1) conflicts are still left in the grammar, so not every source is parsed correctly.
- The parser is generated by [LLtool](https://github.com/redstar/LLtool). A C++
  port of LLtool is now integrated into this project, but not yet enabled.
- Error recovery in the parser is based on "panic mode", using the follow sets
  of the current and the active callers.
- Error messages are outputted using llvm::SourceMgr for nice presentation.
- Only a dummy driver exists to see the parser in action.
- A couple of IR statements is emitted to the console after succesful parsing.

[1]: https://travis-ci.org/redstar/m2lang "Travis CI Build Status"
[2]: https://cirrus-ci.com/github/redstar/m2lang "Cirrus CI Build Status"
