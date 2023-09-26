namespace Interlude.Tools.Features

open Percyqaz.Shell
open Interlude.Tools.Utils

module Releases =

    open System.IO
    open System.IO.Compression

    let mutable current_version =
        let file = Path.Combine(INTERLUDE_SOURCE_PATH, "Interlude.fsproj")
        let f = File.ReadAllText file

        let i = f.IndexOf "<AssemblyVersion>"
        let j = f.IndexOf "</AssemblyVersion>"

        f.Substring(i, j - i).Substring("<AssemblyVersion>".Length)

    let publish() =

        let changelog = Path.Combine(YAVSRG_PATH, "Interlude", "docs", "changelog.md")
        let logtxt = File.ReadAllText(changelog)
        let latest = logtxt.Split(current_version + "\r\n" + "====", 2).[0]
        if latest.Trim() = "" then failwithf "No changelog for new version. Create this first"
        let v = latest.Split("====", 2).[0].Trim()
        File.WriteAllText(Path.Combine(YAVSRG_PATH, "Interlude", "docs", "changelog-latest.md"), latest)

        printfn "Version: %s -> %s" current_version v
        printfn "%s" latest
        let file = Path.Combine(INTERLUDE_SOURCE_PATH, "Interlude.fsproj")
        let mutable f = File.ReadAllText file

        do
            let i = f.IndexOf "<AssemblyVersion>"
            let j = f.IndexOf "</AssemblyVersion>"

            f <- f.Substring(0, i) + "<AssemblyVersion>" + v + f.Substring(j)

        do
            let i = f.IndexOf "<FileVersion>"
            let j = f.IndexOf "</FileVersion>"

            f <- f.Substring(0, i) + "<FileVersion>" + v + f.Substring(j)

        File.WriteAllText(file, f)

        current_version <- v

        printfn "Creating git commit"

        exec "git" (sprintf "commit -a -m \"🏷️ Version %s\"" v)

    type BuildPlatformInfo =
        {
            Name: string
            RuntimeId: string
            BassLibraryFile: string
            GLFWLibraryFile: string
            ExecutableFile: string
        }

    let build_platform (info: BuildPlatformInfo) =
        
        Directory.SetCurrentDirectory(INTERLUDE_SOURCE_PATH)

        let build_dir = Path.Combine(INTERLUDE_SOURCE_PATH, "bin", "Release", "net7.0", info.RuntimeId)
        let clean_dir = Path.Combine(YAVSRG_PATH, "Interlude", "releases", $"Interlude-{info.Name}")

        try Directory.Delete(build_dir, true) with _ -> ()
        try Directory.Delete(clean_dir, true) with _ -> ()

        exec "dotnet" $"publish --configuration Release -r {info.RuntimeId} -p:PublishSingleFile=True --self-contained true"

        Directory.CreateDirectory clean_dir |> ignore

        let rec copy source target =
            Directory.CreateDirectory target |> ignore
            for file in Directory.GetFiles source do
                match Path.GetExtension(file).ToLower() with
                | ".dll"
                | ".so"
                | ".dylib"
                | ".txt" -> File.Copy(file, Path.Combine(target, Path.GetFileName file))
                | _ -> ()

        File.Copy(Path.Combine(YAVSRG_PATH, "Percyqaz.Flux", "lib", info.RuntimeId, info.BassLibraryFile), Path.Combine(clean_dir, info.BassLibraryFile))
        File.Copy(Path.Combine(build_dir, "publish", info.ExecutableFile), Path.Combine(clean_dir, info.ExecutableFile))
        File.Copy(Path.Combine(build_dir, "publish", info.GLFWLibraryFile), Path.Combine(clean_dir, info.GLFWLibraryFile))
        copy (Path.Combine(build_dir, "Locale")) (Path.Combine(clean_dir, "Locale"))

        printfn "Outputted to: %s" clean_dir
        if File.Exists(clean_dir + ".zip") then File.Delete(clean_dir + ".zip")
        ZipFile.CreateFromDirectory(clean_dir, clean_dir + ".zip")
        printfn "Zipped to: %s.zip" clean_dir
    
    let build_osx_arm64() = build_platform { Name = "osx-arm64"; RuntimeId = "osx-arm64"; BassLibraryFile = "libbass.dylib"; GLFWLibraryFile = "libglfw.3.dylib"; ExecutableFile = "Interlude" }
    let build_win64() = build_platform { Name = "win64"; RuntimeId = "win-x64"; BassLibraryFile = "bass.dll"; GLFWLibraryFile = "glfw3.dll"; ExecutableFile = "Interlude.exe" }

    let register(ctx: ShellContext) : ShellContext =
        ctx.WithCommand(
            "version",
            "Displays the current version of Interlude",
            fun () -> current_version
        ).WithCommand(
            "publish",
            "Publishes a new version of Interlude",
            publish
        ).WithCommand(
            "release_win64",
            "Build an Interlude release and zip it for upload",
            build_win64
        ).WithCommand(
            "release_osx_arm64",
            "Build an Interlude release and zip it for upload",
            build_osx_arm64
        )