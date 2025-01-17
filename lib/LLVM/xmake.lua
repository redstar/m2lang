target("llvm-modules")
    add_packages("llvm")
    set_kind("moduleonly")
    add_files("*.cppm", {public = true})
