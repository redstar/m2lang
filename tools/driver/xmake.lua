target("driver")
    add_packages("llvm")
    set_kind("binary")
    add_deps("basic", "ast", "lexer", "sema", "parser", "codegen")
    add_files("driver.cpp")
    set_basename("m2lang")