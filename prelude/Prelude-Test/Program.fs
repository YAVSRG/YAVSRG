open System
open System.IO
open Prelude.Common

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
    Path.Combine(Prelude.Data.Profiles.Profile.profilePath, "profile.json")
    |> Prelude.Data.Profiles.Profile.load
    |> printfn "%A"
    0