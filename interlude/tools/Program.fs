open Percyqaz.Shell
open Interlude.Tools
open Interlude.Tools.Features

let ctx =
    Context.Empty
    |> Assets.register
    |> Releases.register

[<EntryPoint>]
let main argv =
    printfn "Path: %s" Utils.YAVSRG_PATH
    Shell.basic_repl ctx
    0
