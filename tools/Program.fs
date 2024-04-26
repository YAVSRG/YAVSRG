open Percyqaz.Shell
open Percyqaz.Shell.Shell
open Interlude.Tools

let ctx =
    ShellContext.Empty 
    |> Commands.register
    |> fun ctx -> ctx.WithCommand("exit", "Closes the YAVSRG command line", fun () -> System.Environment.Exit(0))

[<EntryPoint>]
let main argv =
    let io = IOContext.Console

    if argv.Length > 0 then
        ctx.Evaluate io (String.concat " " argv)
    else
        printfn "== YAVSRG CLI Tools =="
        repl io ctx

    0
