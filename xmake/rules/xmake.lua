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
                local fragmentfile = path.join(gendir, path.basename(file) .. ".g.inc")
                io.writefile(fragmentfile, "")
                os.touch(fragmentfile, {atime = 1, mtime = 1})
                includes = gendir
            end
        end
        if includes then
            target:add("includedirs", includes)
        end
    end)

    before_buildcmd_file(function (target, batchcmds, sourcefile_lltool, opt)

        -- Get path to LLtool.
        import("lib.detect.find_tool")
        local lltool = assert(find_tool("LLtool", {paths = {"$(buildir)/$(plat)/$(arch)/$(mode)"}}), "LLtool not found!")

        -- Get path of fragment source file.
        local fragmentfile = path.join(target:autogendir(), "rules", 'lltool', path.basename(sourcefile_lltool) .. ".g.inc")

        -- Add fragment.
        table.insert(target:headerfiles(), fragmentfile)

        -- Add commands.
        batchcmds:show_progress(opt.progress, "${color.build.object}compiling.lltool %s", sourcefile_lltool)
        batchcmds:mkdir(path.directory(fragmentfile))
        batchcmds:vrunv(lltool.program, {"-o", path(fragmentfile), path(sourcefile_lltool)})

        -- Add dependencies.
        batchcmds:add_depfiles(sourcefile_lltool)
        batchcmds:set_depmtime(os.mtime(fragmentfile))
        batchcmds:set_depcache(target:dependfile(fragmentfile))
    end)

