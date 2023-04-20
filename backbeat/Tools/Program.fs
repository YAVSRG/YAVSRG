open Percyqaz.Shell

let ctx =
    Context.Empty
    |> Backbeat.Features.Tables.register
    |> Backbeat.Features.Archive.register

[<EntryPoint>]
let main argv =
    Backbeat.Utils.init()
    if argv.Length > 0 then
        match ctx.Interpret(String.concat " " argv) with
        | Ok _ -> ()
        | ParseFail err -> printfn "%A" err
        | RunFail err -> raise err
    else
        printfn "== Backbeat CLI =="
        Shell.basic_repl ctx
    0
