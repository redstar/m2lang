package("llvm")
    set_homepage("https://llvm.org/")
    set_description("The LLVM Compiler Infrastructure")
    set_kind("library")
    set_policy("package.fetch_only", true)
    set_policy("package.include_external_headers", false)

    on_fetch(function (package, opt)
        if opt.system then
            local llvm_config = "llvm-config"
            local version = try {function() return os.iorunv(llvm_config, {"--version"}) end}
            if version then
                version = version:trim()
                local libopt
                if package:config("shared") then
                    libopt = "--link-shared"
                else
                    libopt = "--link-static"
                end
                local includedir = try {function() return os.iorunv(llvm_config, {libopt, "--includedir"}) end}
                local libs = try {function() return os.iorunv(llvm_config, {libopt, "--libs"}) end}
                local linkdir = try {function() return os.iorunv(llvm_config, {libopt, "--libdir"}) end}
                local cxxflags = try {function() return os.iorunv(llvm_config, {libopt, "--cxxflags"}) end}
                local ldflags = try {function() return os.iorunv(llvm_config, {libopt, "--ldflags"}) end}
                local result = {}
                result.version = version
                if includedir then
                    result.includedirs = includedir:trim()
                end
                if libs then
                    local links = {}
                    for _, item in ipairs(libs:trim():split(" ")) do
                        table.insert(links, item:sub(3)) -- Remove -l prefix
                    end
                    if not package:config("shared") then
                        table.insert(links, "zstd")
                        table.insert(links, "z")
                    end
                    result.links = table.unwrap(links)
                end
                if linkdir then
                    result.linkdirs = { linkdir:trim(), "/opt/local/lib" } -- FIXME macosx specific
                end
                if cxxflags then
                    local flags = {}
                    local defines = {}
                    for _, item in ipairs(cxxflags:trim():split(" ")) do
                        -- Filter out options --std= and -I, and put macro
                        -- definitions in separate table.
                        if not (item:startswith("-std=") or item:startswith("-I")) then
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
                if ldflags then
                    local flags = {}
                    for _, item in ipairs(ldflags:trim():split(" ")) do
                        -- Filter out options -L.
                        print("Item", item)
                        if not item:startswith("-L") then
                            table.insert(flags, item)
                        end
                    end
                    result.ldflags = table.unwrap(flags)
                end
                -- print("Result: ", result)
                return result
            end
        end
    end)