open Percyqaz.Shell
open Backbeat.Features
open Backbeat.Features.Archive

let ctx =
    Context.Empty
    |> Tables.register
    |> Archive.register
    |> Rulesets.register

[<EntryPoint>]
let main argv =
    use logging = Backbeat.Utils.init()
    if argv.Length > 0 then
        match ctx.Interpret(String.concat " " argv) with
        | Ok _ -> ()
        | ParseFail err -> printfn "%A" err
        | RunFail err -> raise err
    else
        printfn "== Backbeat CLI =="
        Shell.basic_repl ctx
    0
