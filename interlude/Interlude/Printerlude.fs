namespace Interlude

open System.IO
open Percyqaz.Shell
open Percyqaz.Shell.Shell
open Interlude.UI.Toolbar

module Printerlude =

    let synchronise (f: unit -> unit) = UI.Screen.globalAnimation.Add(UI.Animation.AnimationAction(f))

    let mutable private ctx : Context = Unchecked.defaultof<_>

    module Tables = 
        
        let register_commands ctx = ctx

    module Utils =

        let show_version() =
            ctx.WriteLine(sprintf "You are running %s" Utils.version)
            ctx.WriteLine(sprintf "The latest version online is %s" Utils.AutoUpdate.latestVersionName)
        
        let register_commands (ctx: Context) = 
            ctx.WithCommand("version", Command.create "Shows info about the current game version" [] (Impl.Create show_version))
                .WithCommand("exit", Command.create "Exits the game" [] (Impl.Create (fun () -> UI.Screen.exit <- true)))
                .WithCommand("clear", Command.create "Clears the terminal" [] (Impl.Create Terminal.Log.clear))

    let ms = new MemoryStream()
    let context_output = new StreamReader(ms)
    let context_writer = new StreamWriter(ms)

    ctx <-
        { Context.Empty with IO = { In = stdin; Out = context_writer } }
        |> Tables.register_commands
        |> Utils.register_commands

    let exec(s: string) =
        let msPos = ms.Position
        match ctx.Interpret s with
        | Ok new_ctx -> 
            ctx <- new_ctx
            context_writer.Flush()
            ms.Position <- msPos
            Terminal.add_message (context_output.ReadToEnd())
        | ParseFail err -> Terminal.add_message (err.ToString())
        | RunFail err -> Terminal.add_message err.Message

    let init() =
        Terminal.exec_command <- exec