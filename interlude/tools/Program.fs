open Percyqaz.Shell
open Percyqaz.Shell.Shell
open Interlude.Tools
open Interlude.Tools.Features

let ctx =
    ShellContext.Empty
    |> Check.register
    |> Assets.register
    |> Releases.register

[<EntryPoint>]
let main argv =
    if argv.Length > 0 then ctx.Evaluate(String.concat " " argv)
    else
        printfn "== Interlude Tools CLI =="
        repl ctx
    0
