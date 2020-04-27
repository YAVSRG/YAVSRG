open System
open System.Collections.Generic
open Prelude.Common
open Prelude.Json
open Prelude.Data.ChartManager

(*
    Prelude features required to build Interlude:
        Theme management
    Prelude features that would otherwise be useful:
        Pattern generator
        File format for chart editing containing extra data/layers of notes
        SV Filters
*)

[<EntryPoint>]
let main argv =
    Console.BufferHeight <- 32766
    "Game Time.sm"
    |> Prelude.Charts.StepMania.loadStepmaniaFile
    |> fun sm -> sm; Prelude.Charts.StepMania.calculateEtternaHash (List.head sm.Charts).NOTES sm.BPMS
    |> printfn "%A"
    0