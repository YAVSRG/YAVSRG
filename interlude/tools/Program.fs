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
    if argv.Length > 0 then
        match ctx.Interpret(String.concat " " argv) with
        | Ok _ -> ()
        | ParseFail err -> printfn "%A" err
        | RunFail err -> raise err
    else
        Shell.basic_repl ctx
    0
