open Percyqaz.Common
open Percyqaz.Shell
open Percyqaz.Shell.Shell
open YAVSRG.CLI

let ctx = ShellContext.Empty |> Commands.register

[<EntryPoint>]
let main argv =
    let io = IOContext.Console

    Logging.Verbosity <- LoggingLevel.DEBUG

    if argv.Length > 0 then
        ctx.Evaluate io (String.concat " " argv)
    else
        System.Console.Clear()
        printfn "== YAVSRG CLI Tools =="
        printfn "type 'help' for a list of commands, 'help <command>' for details, 'exit' to exit"
        repl io ctx

    0