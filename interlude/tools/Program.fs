open Percyqaz.Shell
open Percyqaz.Shell.Shell
open Interlude.Tools
open Interlude.Tools.Features

let ctx =
    ShellContext.Empty |> Check.register |> Assets.register |> Releases.register

[<EntryPoint>]
let main argv =
    let io = IOContext.Console

    if argv.Length > 0 then
        ctx.Evaluate io (String.concat " " argv)
    else
        printfn "== Interlude Tools CLI =="
        repl io ctx

    0
