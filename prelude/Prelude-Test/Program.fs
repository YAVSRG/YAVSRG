open System
open Prelude.Common;
open Prelude.Charts.ChartConversions
open Prelude.Gameplay.Difficulty
open Collatz

(*
    Prelude features to tackle next:
        Finish .osu storyboard parsing and .osb support
        Finish sm <> interlude conversion
        HP system/life calculation
        Chart modifiers
        Chart coloring systems
        Difficulty rating calculation
        Chart caching tools
*)

[<EntryPoint>]
let main argv =
    (*
    let chart =
        @"C:\Users\Blake\AppData\Local\osu!\Songs\911479 HO-KAGO TEA TIME - Utauyo!! MIRACLE (TV size) [no video]\HO-KAGO TEA TIME - Utauyo!! MIRACLE (TV size) (Kawawa) [BMS LV.11 wawather [Stream -Another-]].osu"
        |> loadAndConvertFile |> List.head
    let r = RatingReport(chart.Notes, 0.9, Layout.LeftOne, chart.Keys)
    printf "%A, %A" r.Physical r.Technical*)
    Collatz.main
    0 