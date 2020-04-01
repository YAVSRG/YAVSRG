open System
open System.Collections.Generic
open Prelude.Common
open FParsec
open Prelude.Data.ChartManager

(*
    Prelude features required to build Interlude:
        Gameplay/score prep logic
        Theme management
    Prelude features that would otherwise be useful:
        Pattern generator
        File format for chart editing containing extra data/layers of notes
        SV Filters
*)

[<EntryPoint>]
let main argv =
    Console.BufferHeight <- 32766
    Logging.Subscribe(fun (_, _, s) -> if s.Length > 0 then printfn "    %s\n" s)
    let c = Cache()
    @"C:\Users\percy\AppData\Local\osu!\Songs\588322 Snails - Funk With Me (ft Big Gigantic)"
    |> fun p -> TaskManager.AddTask("Test", (c.ConvertSongFolder p "osu!"), (fun b -> TaskManager.AddTask("Test2", (c.RebuildCache), ignore, true)), true)
    TaskManager.Wait
    0