rule("lltool")
    add_deps("c++")
    set_extensions(".g")

    on_config(function (target)
        -- Create empty file for each grammar file. Set time to 0 to trigger
        -- build first time.
        local includes
        for _, file in pairs(target:sourcefiles()) do
            local extension = path.extension(file)
            if extension == ".g" then
                local gendir = path.join(target:autogendir(), "rules", 'lltool')
                print("gendir: %s", gendir)
                local fragmentfile = path.join(gendir, path.basename(file) .. ".g.inc")
                if not os.exists(fragmentfile) then
                    io.writefile(fragmentfile, "")
                    os.touch(fragmentfile, {atime = 1, mtime = 1})
                    includes = gendir
                    cprint("${dim}touching %s", fragmentfile)
                    table.insert(target:headerfiles(), fragmentfile)
                end
            end
        end
        if includes then
            target:add("includedirs", includes)
        end
    end)

    before_build(function (target, opt)
        import("lib.detect.find_tool")
        import("utils.progress")
        local lltool = assert(find_tool("LLtool", {paths = {"$(buildir)/$(plat)/$(arch)/$(mode)"}}), "LLtool not found!")

        local includes
        for _, file in pairs(target:sourcefiles()) do
            local extension = path.extension(file)
            if extension == ".g" then
                local gendir = path.join(target:autogendir(), "rules", 'lltool')
                local fragmentfile = path.join(gendir, path.basename(file) .. ".g.inc")
                local params = {"-o", path(fragmentfile), path(file)}
                progress.show(opt.progress, "${color.build.target}<%s> ${clear}${color.build.object}generating.lltool %s", target:name(), file)
                local ok = try
                {
                    function ()
                        os.vrunv(lltool.program, params)
                        return true
                    end
                }
                if not ok then
                    local command = lltool.program .. " " ..os.args(params)
                    cprint("\r${bright color.error}error: ${clear}run `%s` failed", command)
                end
            end
        end
    end)

    -- This does not work. It seems that the dependency between the include in
    -- the C++ file and the generated fragment is missing.
    -- Same with before_build_file.
    -- before_buildcmd_file(function (target, batchcmds, sourcefile_lltool, opt)
    --     -- Get path to LLtool.
    --     import("lib.detect.find_tool")
    --     local lltool = assert(find_tool("LLtool", {paths = {"$(buildir)/$(plat)/$(arch)/$(mode)"}}), "LLtool not found!")

    --     -- Get path of fragment source file.
    --     local gendir = path.join(target:autogendir(), "rules", 'lltool')
    --     local fragmentfile = path.join(gendir, path.basename(file) .. ".g.inc")
    --     target:add("includedirs", includes)

    --     -- Add fragment.
    --     table.insert(target:headerfiles(), fragmentfile)

    --     -- Add commands.
    --     batchcmds:show_progress(opt.progress, "${color.build.target}<%s> ${clear}${color.build.object}generating.lltool 2 %s", target:name(), sourcefile_lltool)
    --     batchcmds:mkdir(path.directory(fragmentfile))
    --     batchcmds:vrunv(lltool.program, {"-o", path(fragmentfile), path(sourcefile_lltool)})

    --     -- Add dependencies.
    --     batchcmds:add_depfiles(sourcefile_lltool)
    --     batchcmds:set_depmtime(os.mtime(fragmentfile))
    --     batchcmds:set_depcache(target:dependfile(fragmentfile))
    -- end)
rule_end()
