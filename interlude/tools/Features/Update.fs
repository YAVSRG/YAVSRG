namespace Interlude.Tools.Features

open System.IO
open Interlude.Tools.Utils
open Percyqaz.Shell

module Update =

    let update() =
        Directory.SetCurrentDirectory(TOOLS_PATH)        
        exec "dotnet" "pack"
        exec "dotnet" "tool update --add-source ./nupkg -g Interlude.Tools"

    let register(ctx: Context) : Context =
        ctx.WithCommand(
            "update",
            Command.create "Update Interlude.Tools as a command-line tool" [] (Impl.Create update)
        )