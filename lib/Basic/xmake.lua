target("basic")
    add_packages("llvm")
    set_kind("static")
    add_files("*.cppm", {public = true})
