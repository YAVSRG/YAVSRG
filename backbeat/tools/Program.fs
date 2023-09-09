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
    use logging = Backbeat.Utils.init()
    let io = IOContext.Console
    if argv.Length > 0 then ctx.Evaluate io (String.concat " " argv)
    else
        printfn "== Backbeat CLI =="
        repl io ctx
    0
