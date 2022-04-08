open Percyqaz.Shell
open Interlude.Tools
open Interlude.Tools.Features

let ctx =
    Context.Empty
    |> Asset_Script.register
    |> Bundle_Assets.register
    |> Versioning.register

[<EntryPoint>]
let main argv =
    printfn "%s" Utils.YAVSRG_PATH
    Shell.basic_repl ctx
    0
