open System
open System.Collections.Generic
open Prelude.Common
open FParsec
open Prelude.Data.ChartManager

(*
    Prelude features required to build Interlude:
        Theme management
        Profiles
        Proper json writing and reading
    Prelude features that would otherwise be useful:
        Pattern generator
        File format for chart editing containing extra data/layers of notes
        SV Filters
*)

[<EntryPoint>]
let main argv =
    Console.BufferHeight <- 32766
    printf "%A" (Prelude.Json.JsonHelper.load "{}": Prelude.Charts.Interlude.ChartHeader)
    printf "%A" (Prelude.Json.JsonHelper.load "{}": Prelude.Data.Profiles.Profile).Name
    Logging.Subscribe(fun (_, _, s) -> if s.Length > 0 then printfn "    %s\n" s)
    let c = Cache()
    @"C:\Users\percy\AppData\Local\osu!\Songs\588322 Snails - Funk With Me (ft Big Gigantic)"
    |> fun p -> TaskManager.AddTask("A", (c.ConvertSongFolder p "osu!"), (fun b -> TaskManager.AddTask("B", (c.RebuildCache), ignore, true)), true)
    TaskManager.Wait
    0