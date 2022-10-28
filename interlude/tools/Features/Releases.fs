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

    let change_version (v: string) =

        let changelog = Path.Combine(YAVSRG_PATH, "Interlude", "docs", "changelog.md")
        let logtxt = File.ReadAllText(changelog)
        let latest = logtxt.Split(current_version + "\r\n" + "====", 2)[0]
        if latest.Trim() = "" then failwithf "No changelog for new version: %s! Create this first" v
        File.WriteAllText(Path.Combine(YAVSRG_PATH, "Interlude", "docs", "changelog-latest.md"), latest)

        printfn "Version: %s -> %s" current_version v
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

    let build_win64() =
        
        Directory.SetCurrentDirectory(INTERLUDE_SOURCE_PATH)
        exec "dotnet" "clean --configuration Release /p:Platform=x64"
        exec "dotnet" "build --configuration Release /p:Platform=x64"

        let build_dir = Path.Combine(INTERLUDE_SOURCE_PATH, "bin", "x64", "Release", "netcoreapp3.1")
        let clean_dir = Path.Combine(YAVSRG_PATH, "Interlude", "releases", sprintf "Interlude.%s-win64" current_version)
        try Directory.Delete(clean_dir, true) with _ -> ()
        Directory.CreateDirectory(clean_dir) |> ignore

        let rec copy source target =
            Directory.CreateDirectory target |> ignore
            for file in Directory.GetFiles source do
                match Path.GetExtension(file).ToLower() with
                | ".dll"
                | ".so"
                | ".dylib"
                | ".txt"
                | ".exe" -> File.Copy(file, Path.Combine(target, Path.GetFileName file))
                | _ -> ()

        File.Copy(Path.Combine(YAVSRG_PATH, "Percyqaz.Flux", "lib", "win-x64", "bass.dll"), Path.Combine(clean_dir, "bass.dll"))
        File.Copy(Path.Combine(build_dir, "Interlude.deps.json"), Path.Combine(clean_dir, "Interlude.deps.json"))
        File.Copy(Path.Combine(build_dir, "Interlude.runtimeconfig.json"), Path.Combine(clean_dir, "Interlude.runtimeconfig.json"))
        copy build_dir clean_dir
        copy (Path.Combine(build_dir, "Locale")) (Path.Combine(clean_dir, "Locale"))
        copy (Path.Combine(build_dir, "runtimes", "win-x64", "native")) (Path.Combine(clean_dir, "runtimes", "win-x64", "native"))

        printfn "Outputted to: %s" clean_dir
        ZipFile.CreateFromDirectory(clean_dir, clean_dir + ".zip")
        printfn "Zipped to: %s.zip" clean_dir

    let register(ctx: Context) : Context =
        ctx.WithCommand(
            "version",
            Command.create "Displays the current version of Interlude" [] (Impl.Create (fun () -> printfn "%s" current_version))
        ).WithCommand(
            "publish_version",
            Command.create "Publishes a new version of Interlude" ["new_version"] (Impl.Create (Types.str, change_version))
        ).WithCommand(
            "release_win64",
            Command.create "Build an Interlude release and zip it for upload" [] (Impl.Create build_win64)
        )