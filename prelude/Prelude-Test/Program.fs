open System
open Prelude.Common
open FParsec

(*
    Prelude features to tackle next:
        HP system/life calculation
        Chart modifiers
        Chart coloring systems
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