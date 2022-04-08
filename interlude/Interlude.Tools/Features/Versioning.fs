namespace Interlude.Tools.Features

open Percyqaz.Shell
open Interlude.Tools

module Versioning =

    open System.IO
    open System.Diagnostics

    let mutable current_version =
        let file = Path.Combine(Utils.INTERLUDE, "Interlude.fsproj")
        let f = File.ReadAllText file

        let i = f.IndexOf "<AssemblyVersion>"
        let j = f.IndexOf "</AssemblyVersion>"

        f.Substring(i, j - i).Substring("<AssemblyVersion>".Length)

    let version (v: string) =
        let file = Path.Combine(Utils.INTERLUDE, "Interlude.fsproj")
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

        printfn "Version: %s -> %s" current_version v
        current_version <- v

    let build() =
        Process.Start("Features/build.bat", current_version).WaitForExit()

    let register(ctx: Context) : Context =
        ctx.WithCommand(
            "version",
            Command.create "Renames Interlude's version" ["new_version"] (Impl.Create (Types.str, version))
        ).WithCommand(
            "build",
            Command.create "Build an Interlude release and zip it for upload" [] (Impl.Create build)
        )