open System.IO
open Percyqaz.Common
open Percyqaz.Shell
open Percyqaz.Shell.Shell
open YAVSRG.CLI

let ctx = ShellContext.Empty |> Commands.register

[<EntryPoint>]
let main argv =
    let io = IOContext.Console

    let net8_folder = Path.Combine(Utils.INTERLUDE_SOURCE_PATH, "bin", "Debug", "net8.0")
    let net9_folder = Path.Combine(Utils.INTERLUDE_SOURCE_PATH, "bin", "Debug", "net9.0")

    if ((Directory.Exists net8_folder) && not (Directory.Exists net9_folder)) then
        Directory.Move(net8_folder, net9_folder)

    Logging.Verbosity <- LoggingLevel.DEBUG

    if argv.Length > 0 then
        ctx.Evaluate io (String.concat " " argv)
    else
        System.Console.Clear()
        printfn "== YAVSRG CLI Tools =="
        printfn "type 'help' for a list of commands, 'help <command>' for details, 'exit' to exit"
        repl io ctx

    0