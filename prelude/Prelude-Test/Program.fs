open System
open System.IO
open Prelude.Common
open Prelude.Data.SkinConversions
open Prelude.Charts.osu

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

    try
        let skin = new osuSkin.osuSkin("C:\Users\percy\AppData\Local\osu!\Skins\skins! 16-9 (4-3 Play Area)")
        Directory.Delete("SkinTest", true)
        skin.ToNoteSkin "SkinTest" 7
    with err -> printfn "%O" err
    0