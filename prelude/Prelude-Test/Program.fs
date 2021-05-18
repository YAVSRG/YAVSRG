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

    let (general, editor, meta, difficulty, sb, notes, timing) = loadBeatmapFile @"C:\Users\percy\AppData\Local\osu!\Songs\beatmap-637550765344867554-Hyperventilation\RADWIMPS - Hyperventilation (Percyqaz) [backup].osu"
    let timef = fun x -> if x > 218093.0f<ms> then (x - 218093.0f<ms>) * (122.0f / 120.1f) + 218093.0f<ms> else x
    let newNotes =
        notes
        |> List.map (function HitObject.HitCircle (pos, time, sound, sound2) -> HitCircle (pos, timef time, sound, sound2) | x -> x) 
    let newmap = (general, editor, { meta with Version = "output" }, difficulty, sb, newNotes, timing)
    saveBeatmapFile @"C:\Users\percy\AppData\Local\osu!\Songs\beatmap-637550765344867554-Hyperventilation\RADWIMPS - Hyperventilation (Percyqaz) [output].osu" newmap

    (*
    printfn "Starting tests"
    for file in Directory.EnumerateFiles("skinini/") do
        if file.EndsWith(".ini") then
            printfn "Testing %s" file
            let skin = osuSkin.parseSkinINI file
            printfn "%A" skin.Mania
            Console.ReadLine() |> ignore
    *)
    0