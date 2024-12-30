package("llvm")
    set_homepage("https://llvm.org/")
    set_description("The LLVM Compiler Infrastructure")
    set_kind("library")

    on_fetch(function (package, opt)
        if opt.system then
            local llvm_config = "llvm-config"
            local version = try {function() return os.iorunv(llvm_config, {"--version"}) end}
            if version then
                version = version:trim()
                -- Do not use llvm-config --includedir because property includedirs is mapped
                -- to option -isystem, which results in compile errors. The solution is to
                -- set the include directory as part of the cxxflags.
                local libs = try {function() return os.iorunv(llvm_config, {"--libs"}) end}
                local linkdir = try {function() return os.iorunv(llvm_config, {"--libdir"}) end}
                local cxxflags = try {function() return os.iorunv(llvm_config, {"--cxxflags"}) end}
                local result = {}
                result.version = version
                if libs then
                    links = {}
                    for _, item in ipairs(libs:trim():split(" ")) do
                        table.insert(links, item:sub(3)) -- Remove -l prefix
                    end
                    result.links = table.unwrap(links)
                end
                if linkdir then
                    result.linkdirs = linkdir:trim()
                end
                if cxxflags then
                    flags = {}
                    defines = {}
                    for _, item in ipairs(cxxflags:trim():split(" ")) do
                        -- Filter out option--std=, and put macro definitions in
                        -- separate table.
                        if not item:startswith("-std=") then
                            if item:startswith("-D") then
                                table.insert(defines, item:sub(3))
                            else
                                table.insert(flags, item)
                            end
                        end
                    end
                    result.cxxflags = table.unwrap(flags)
                    result.defines = table.unwrap(defines)
                end
                -- print("Result: ", result)
                return result
            end
        end
    end)