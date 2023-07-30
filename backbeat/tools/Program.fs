open Percyqaz.Shell
open Percyqaz.Shell.Shell
open Backbeat.Features
open Backbeat.Features.Archive

let ctx =
    ShellContext.Empty
    |> Tables.register
    |> Archive.register
    |> Rulesets.register
    
[<EntryPoint>]
let main argv =
    printfn "%i" Upload.existing_files.Count
    use logging = Backbeat.Utils.init()
    if argv.Length > 0 then ctx.Evaluate(String.concat " " argv)
    else
        printfn "== Backbeat CLI =="
        repl ctx
    0
