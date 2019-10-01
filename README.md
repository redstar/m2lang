m2lang - The LLVM-based Modula-2 compiler
=========================================

[![Build Status](https://travis-ci.org/redstar/m2lang.png?branch=master)][1]


Introduction
------------

See this [FOSDEM 2019 talk](https://fosdem.org/2019/schedule/event/llvm_irgen/) for some details.

Implementation decisions
------------------------

 - LLVM is only external dependency
 - CMake scripts derived from clang
 - PIM4 language first

Current status
--------------

- The lexer is almost done.
- The parser is a complete PIM4 parser with some additions for ISO.
- Diagnostics and error recovery is mostly missing.
- Only a dummy driver exists to see the parser in action.

[1]: https://travis-ci.org/redstar/m2lang "Travis CI Build Status"