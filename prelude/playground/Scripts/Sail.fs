module Sail

open Prelude
open Prelude.Formats.Osu

let source =
    @"C:\Users\percy\AppData\Local\osu!\Songs\beatmap-637823398692216004-AWOLNATION - Sail (Official Music Video)\AWOLNATION - Sail (Percyqaz) [SAIL WITH ME INTO THE DARK].osu"

let sail = Beatmap.FromFile source |> Result.toOption |> Option.get

let note_gradient (start_nps, end_nps, start_time, end_time) =

    // this is the "average speed formula" - i discovered it myself from this problem and then looked it up
    let average_nps = (start_nps * end_nps) * 2.0f / (start_nps + end_nps)

    // gradient of curve is proportional to spacing of notes
    let grad_1 = average_nps / start_nps
    let grad_2 = average_nps / end_nps

    // a curve from [0,1] to [0,1], gradient at 0 = grad_1, gradient at 1 = grad_2
    // gradient linearly changes from grad_1 to grad_2
    let func = fun (x: float32) -> grad_1 * x + (grad_2 - grad_1) * x * x * 0.5f

    // sample the curve at N evenly spaced intervals
    // this will give the correct note timings
    let duration: Time = end_time - start_time
    let samplesf = average_nps * float32 duration / 1000.0f
    // for a clean transition, N should come out to be an integer (so that the next note would land exactly at `end_time`)
    let samples = int samplesf

    [ 0..samples ]
    |> List.map (fun s -> float32 s / float32 samples) // normalise to range [0,1]
    |> List.map (fun s -> func s) // [0,1] still, but now gradiented according to desired transition
    |> List.map (fun l -> l * duration + start_time) // output as time in original range

let start = 17335f<ms> // change this to change offset
let bpm = 119f<beat / minute>

let measure (n: float32) : Time =
    start + n * 4.0f<beat> * 60000f<ms / minute> / bpm

let snap (per_beat: float32) = per_beat * bpm / 60f<beat / minute>

let bass m =
    note_gradient (snap 16f, snap 14f, measure m, measure (m + 0.5f))

let bass_ez m =
    note_gradient (snap 16f, snap 12f, measure m, measure (m + 0.5f))

let SAIL m =
    note_gradient (snap 20f, snap 18f, measure m, measure (m + 0.25f))

let gentle_oo m =
    note_gradient (snap 12f, snap 9f, measure m, measure (m + 0.5f))

let oo1 m =
    note_gradient (snap 9f, snap 10f, measure m, measure (m + 0.5f))
    @ note_gradient (snap 10f, snap 12f, measure (m + 0.5f), measure (m + 1.25f))

let oo2 m =
    note_gradient (snap 9f, snap 14f, measure m, measure (m + 0.5f))
    @ note_gradient (snap 14f, snap 9f, measure (m + 0.5f), measure (m + 1.25f))

let ``SAIL!!`` m =
    note_gradient (snap 25f, snap 20f, measure m, measure (m + 0.25f))

let into_the_dark m =
    note_gradient (snap 12f, snap 14f, measure m, measure (m + 1.75f))

let sail_with_me m =
    note_gradient (snap 12f, snap 10f, measure m, measure (m + 1.75f))

let lines =
    [
        // SAIL
        gentle_oo 8f
        gentle_oo 10f
        gentle_oo 12f
        gentle_oo 14f
        gentle_oo 15f
        // verse -- this is how i show my love
        bass 16f
        bass 18f
        bass 20f
        bass_ez 22f
        bass_ez 23f
        // this is how an angel cries
        bass 24f
        bass 26f
        bass 28f
        bass_ez 30f
        bass_ez 31f
        // SAIL!
        SAIL 31.75f
        oo1 32f
        SAIL 33.75f
        oo1 34f
        SAIL 35.75f
        oo1 36f
        SAIL 37.75f
        oo1 38f
        SAIL 39.75f
        // maybe i should cry for help
        bass 40f
        bass 42f
        bass 44f
        bass_ez 46f
        bass_ez 47f
        // maybe i'm a different breed
        bass 48f
        bass 50f
        bass 52f
        bass_ez 54f
        bass_ez 55f
        // SAIL!!
        SAIL 55.75f
        oo2 56f
        SAIL 57.75f
        oo2 58f
        SAIL 59.75f
        oo2 60f
        SAIL 61.75f
        oo2 62f
        SAIL 63.75f
        // la lala lala
        // SAIL!!!
        ``SAIL!!`` 79.75f
        oo1 80f
        ``SAIL!!`` 81.75f
        oo2 82f
        ``SAIL!!`` 83.75f
        oo1 84f
        ``SAIL!!`` 85.75f
        oo2 86f
        ``SAIL!!`` 87.75f
        into_the_dark 88f
        ``SAIL!!`` 89.75f
        into_the_dark 90f
        ``SAIL!!`` 91.75f
        into_the_dark 92f
        ``SAIL!!`` 93.75f
        sail_with_me 94f
        ``SAIL!!`` 95.75f
        // outro
        bass 98f
        bass 100f
        bass_ez 102f
        bass_ez 103f
        bass 104f
    ]

let line (t: Time) =
    { UninheritedTimingPoint.Create(t, 60000f<ms / minute> / bpm, 4) with
        Effects = TimingEffect.OmitFirstBarline
    } |> Uninherited

let renderedLines = lines |> List.concat |> List.map line

let main () =
    { sail with
        Timing = (line start) :: renderedLines
    }.ToFile source