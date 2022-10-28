open Percyqaz.Shell
open Interlude.Tools
open Interlude.Tools.Features

let ctx =
    Context.Empty
    |> Check.register
    |> Assets.register
    |> Releases.register

[<EntryPoint>]
let main argv =
    if argv.Length > 0 then
        match ctx.Interpret(String.concat " " argv) with
        | Ok _ -> ()
        | ParseFail err -> printfn "%A" err
        | RunFail err -> raise err
    else
        printfn "== Interlude Tools CLI =="
        Shell.basic_repl ctx
    0
