open Percyqaz.Shell

let ctx =
    Context.Empty
    |> Interlude.Charts.Features.register

[<EntryPoint>]
let main argv =
    Interlude.Charts.Utils.init()
    if argv.Length > 0 then
        match ctx.Interpret(String.concat " " argv) with
        | Ok _ -> ()
        | ParseFail err -> printfn "%A" err
        | RunFail err -> raise err
    else
        printfn "== Interlude Charts CLI =="
        Shell.basic_repl ctx
    0
