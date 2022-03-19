namespace Interlude

open System.IO
open Percyqaz.Shell.Tree
open Percyqaz.Shell.Library
open Interlude.UI.Toolbar

module Printerlude =

    let synchronise (f: unit -> unit) = UI.Screen.globalAnimation.Add(UI.Animation.AnimationAction(f))

    let mutable private context = Unchecked.defaultof<_>

    type Commands =
        static member Chart() : string =
            match Gameplay.Chart.cacheInfo with
            | Some c -> c.Hash
            | None -> failwith "No current chart."

        static member Exit() = UI.Screen.exit <- true

    let ms = new MemoryStream()
    let context_output = new StreamReader(ms)
    let context_writer = new StreamWriter(ms)

    context <- 
        { Context.Create<Commands>() with
            IO = {
                In = stdin // not used
                Out = context_writer
            }
        }

    let exec(s: string) =
        let msPos = ms.Position
        match context.Interpret s with
        | Ok ctx -> 
            context <- ctx
            context_writer.Flush()
            ms.Position <- msPos
            Terminal.add_message (context_output.ReadToEnd())
        | ParseFail err -> Terminal.add_message err
        | TypeFail errs -> for l in (Percyqaz.Shell.Check.Err.format errs) do Terminal.add_message l
        | RunFail err -> Terminal.add_message err.Message

    let init() =
        Terminal.exec_command <- exec