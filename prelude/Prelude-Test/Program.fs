open System
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
    try
        Collide.collide
    with
    | e ->
        Logging.Error (e.ToString()) ""
    0