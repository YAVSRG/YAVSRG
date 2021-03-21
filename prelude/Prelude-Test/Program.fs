open System
open System.IO
open Prelude.Data.SkinConversions

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

    printfn "Starting tests"
    for file in Directory.EnumerateFiles("skinini/") do
        if file.EndsWith(".ini") then
            printfn "Testing %s" file
            let skin = osuSkin.parseSkinINI file
            printfn "%A" skin.Mania
            Console.ReadLine() |> ignore
    0