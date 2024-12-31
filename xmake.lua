set_project("m2lang")
set_xmakever("2.9.6")

set_version("0.0.1")
set_configvar("APP", "m2lang")

add_rules("mode.release", "mode.debug")

-- The project uses C++ 20 modules.
set_languages("c++20")

-- Use local repository for package LLVM.
add_repositories("local-repo xmake/local-repo")
add_requires("llvm >= 19.1.1", {system = true})

add_includedirs("include")

includes("lib/LLtool")
includes("utils/LLtool")
includes("lib/ASTtool")
includes("utils/ASTtool")

includes("lib/Basic")
includes("lib/Lexer")
includes("lib/AST")
includes("lib/Sema")
includes("lib/Parser")
includes("lib/CodeGen")
includes("tools/driver")

