namespace Interlude.Tools.Features

open Percyqaz.Shell
open Interlude.Tools

module Releases =

    open System.IO
    open System.Diagnostics

    let mutable current_version =
        let file = Path.Combine(Utils.INTERLUDE, "Interlude.fsproj")
        let f = File.ReadAllText file

        let i = f.IndexOf "<AssemblyVersion>"
        let j = f.IndexOf "</AssemblyVersion>"

        f.Substring(i, j - i).Substring("<AssemblyVersion>".Length)

    let version (v: string) =
        printfn "Version: %s -> %s" current_version v
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

        current_version <- v

    let build() =
        Process.Start("Features/build.bat", current_version).WaitForExit()

    let debug() =
        Process.Start("Features/debug.bat").WaitForExit()

    let register(ctx: Context) : Context =
        ctx.WithCommand(
            "version",
            Command.create "Renames Interlude's version" ["new_version"] (Impl.Create (Types.str, version))
        ).WithCommand(
            "release",
            Command.create "Build an Interlude release and zip it for upload" [] (Impl.Create build)
        ).WithCommand(
            "debug",
            Command.create "Build & debug Interlude" [] (Impl.Create debug)
        )