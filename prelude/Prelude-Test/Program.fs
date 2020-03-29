open System
open System.Collections.Generic
open Prelude.Common
open FParsec

(*
    Prelude features required to build Interlude:
        HP system/life calculation
        Gameplay/score prep logic
        Chart modifiers
        Chart coloring systems
        Chart caching tools
    Prelude features that would otherwise be useful:
        Pattern generator
        File format for chart editing containing extra data/layers of notes
        SV Filters
*)

[<EntryPoint>]
let main argv =
    Console.BufferHeight <- 32766
    @"C:\Users\percy\AppData\Local\osu!\Songs\1122880 WJSN (Cosmic Girls) - Miracle\WJSN (Cosmic Girls) - Miracle (Percyqaz) [Uncut Ver.].osu"
    |> System.IO.Path.GetDirectoryName
    |> System.IO.Path.GetDirectoryName
    |> printfn "%A"
    (*try
        Collide.collide
    with
    | e ->
        Logging.Error (e.ToString()) ""*)
    0