target("ASTtool")
    add_packages("llvm")
    set_kind("binary")
    add_deps("asttool")
    add_files("ASTtool.cpp")
