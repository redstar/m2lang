target("basic")
    add_packages("llvm")
    set_kind("static")
    -- Adding a local include dir seems not to be honored by scan-deps.
    set_configdir("$(buildir)/$(plat)/$(arch)/$(mode)/generated/Basic")
    add_configfiles("$(projectdir)/lib/Basic/Version.inc.in")
    add_files("*.cppm", {public = true})
