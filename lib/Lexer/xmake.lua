target("lexer")
    add_packages("llvm", {components = "base"})
    set_kind("static")
    add_deps("LLtool", "basic")
    add_rules("lltool")
    add_files("DirectiveParser.g")
    add_files("*.cppm", {public = true})