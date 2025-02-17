module Trendy

open Prelude
open Prelude.Formats.Osu

let trendy =
    Beatmap.FromFile
        @"C:\Users\percy\AppData\Local\osu!\Songs\1010949 greyl - Trendy\greyl - Trendy (Percyqaz) [Uncut Ver.].osu"
    |> Result.toOption
    |> Option.get

let times =
    [|
        0.132
        0.593
        1.055
        1.516
        3.824
        4.285
        4.747
        5.208
        7.516
        7.978
        8.439
        8.901
        11.208
        11.670
        14.901
        15.362
        15.824
        16.285
        18.593
        19.055
        19.516
        19.978
        22.285
        22.747
        23.208
        23.670
        25.978
        26.439
        35.208
        88.747
        89.208
        89.670
        92.439
        92.901
        93.362
        96.132
        96.593
        97.055
        97.516
        99.824
        100.285
        103.516
        103.978
        104.439
        104.901
        107.208
        107.670
        108.132
        108.593
        110.901
        111.362
        111.824
        112.285
        114.593
        115.055
        123.824
        177.362
        177.824
        178.285
        181.055
        181.516
        181.978
        184.747
        185.208
        185.670
        188.439
        188.670
        188.901
        189.362
        189.593
        189.824
        236.439
        236.901
        237.362
        237.824
        240.132
        240.593
        241.055
        241.516
        243.824
        244.285
        244.747
        245.208
        247.516
        247.978
        251.208
        251.670
        252.132
        252.593
        254.901
        255.362
        255.824
        256.285
        258.593
        259.055
        259.516
        259.978
        262.285
        262.747
        263.208
        263.670
        267.824
    |]
    |> Array.map (fun x -> float32 (1000.0 * x) * 1.0f<ms>)

let startRate = 0.55
let incr = 0.01

// 0-0.132 is at 0.55 rate

let mutable index = 0
let mutable rate = 0.56f
let mutable actualNow = 0.0f<ms>
let mutable originalNow = 0.0f<ms>

let updateIndex time =
    if index < times.Length - 1 then
        let nextTime = times.[index + 1]

        if time >= nextTime then
            assert (time = nextTime)
            index <- index + 1
            actualNow <- actualNow + (nextTime - originalNow) / rate
            originalNow <- nextTime
            rate <- rate + 0.01f

let handle_object (object: HitObject) =
    match object with
    | HitCircle note ->
        updateIndex (Time.of_number note.Time)
        let new_time = actualNow + (Time.of_number note.Time - originalNow) / rate
        HitCircle { note with Time = int new_time }
    | _ -> failwith ""

let main () =
    { trendy with
        Objects = trendy.Objects |> List.map handle_object
        General =
            { trendy.General with
                AudioFilename = "output.mp3"
            }
        Metadata =
            { trendy.Metadata with
                Version = "Faster"
                BeatmapID = 0
            }
    }.ToFile
        @"C:\Users\percy\AppData\Local\osu!\Songs\1010949 greyl - Trendy\greyl - Trendy (Percyqaz) [Faster].osu"