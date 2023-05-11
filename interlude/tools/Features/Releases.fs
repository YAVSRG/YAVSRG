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

        exec "git" "add ."
        exec "git" (sprintf "commit -m \"🏷️ Version %s\"" v)

    let build_win64() =
        
        Directory.SetCurrentDirectory(INTERLUDE_SOURCE_PATH)

        let build_dir = Path.Combine(INTERLUDE_SOURCE_PATH, "bin", "Release", "net7.0", "win-x64")
        let clean_dir = Path.Combine(YAVSRG_PATH, "Interlude", "releases", "Interlude-win64")

        try Directory.Delete(build_dir, true) with _ -> ()
        try Directory.Delete(clean_dir, true) with _ -> ()

        exec "dotnet" "publish --configuration Release -r win-x64 -p:PublishSingleFile=True --self-contained true"

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

        File.Copy(Path.Combine(YAVSRG_PATH, "Percyqaz.Flux", "lib", "win-x64", "bass.dll"), Path.Combine(clean_dir, "bass.dll"))
        File.Copy(Path.Combine(build_dir, "publish", "Interlude.exe"), Path.Combine(clean_dir, "Interlude.exe"))
        File.Copy(Path.Combine(build_dir, "publish", "glfw3.dll"), Path.Combine(clean_dir, "glfw3.dll"))
        copy (Path.Combine(build_dir, "Locale")) (Path.Combine(clean_dir, "Locale"))

        printfn "Outputted to: %s" clean_dir
        if File.Exists(clean_dir + ".zip") then File.Delete(clean_dir + ".zip")
        ZipFile.CreateFromDirectory(clean_dir, clean_dir + ".zip")
        printfn "Zipped to: %s.zip" clean_dir

    let register(ctx: Context) : Context =
        ctx.WithCommand(
            "version",
            Command.create "Displays the current version of Interlude" [] (Impl.Create ((fun () -> current_version), Types.str))
        ).WithCommand(
            "publish",
            Command.create "Publishes a new version of Interlude" [] (Impl.Create publish)
        ).WithCommand(
            "release_win64",
            Command.create "Build an Interlude release and zip it for upload" [] (Impl.Create build_win64)
        )