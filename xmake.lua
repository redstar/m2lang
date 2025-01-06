set_project("m2lang")
set_xmakever("2.9.4")

set_version("0.0.1")
set_configvar("CONFIG_APP_NAME", "m2lang")

add_rules("mode.release", "mode.debug")

-- Hard-code the toolchain.
--set_toolchains("clang")

-- The project uses C++ 20 modules.
set_languages("c++20")

-- Trigger update of compile_commands.json in the build directory.
add_rules("plugin.compile_commands.autoupdate", {outputdir = "$(buildir)", lsp = "clangd"})

-- Use local repository for package LLVM.
add_repositories("local-repo xmake/local-repo")
add_requires("llvm >= 19", {system = true, configs = {shared = not is_plat("macosx")}})

-- Include custom rules.
includes("xmake/rules")

add_includedirs("include")
add_includedirs("$(buildir)/$(plat)/$(arch)/$(mode)")
-- These should be added by the rules. Looks like a bug in xmake.
add_includedirs("$(buildir)/.gens/lexer/$(plat)/$(arch)/$(mode)/rules/lltool")
add_includedirs("$(buildir)/.gens/parser/$(plat)/$(arch)/$(mode)/rules/lltool")

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

