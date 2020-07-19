open System
open System.IO
open Prelude.Common
open Prelude.Charts.Interlude

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
    let c1 = loadChartFile("original.yav").Value
    let c2 = loadChartFile("new.yav").Value

    Seq.zip
        (c1.Notes.Data |> Seq.map (fun (t, nd) -> t))
        (c2.Notes.Data |> Seq.map (fun (t, nd) -> t))
    |> Seq.iter(fun (a, b) -> printfn "%f, %f" a (b - a))
    0