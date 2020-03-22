open System
open Prelude.Common
open FParsec

(*
    Prelude features to tackle next:
        Finish sm <> interlude conversion
        HP system/life calculation
        Chart modifiers
        Chart coloring systems
        Difficulty rating calculation
        Chart caching tools
*)

[<EntryPoint>]
let main argv =
    try
        Collide.collide
    with
    | e ->
        Logging.Error (e.ToString()) ""
    0