module Polyrhythm

open Prelude
open Prelude.Formats.Osu

let source =
    @"C:\Users\percy\AppData\Local\osu!\Songs\beatmap-638573341266920297-audio\Virtual Riot - I heard you like polyrhythms (Percyqaz) [4K].osu"

let osu_file = Beatmap.FromFile source |> Result.toOption |> Option.get

let poly n =
    seq {
        let total_beats = 450 - n
        let total_span = 449000.0f<ms>

        let beat_spacing = total_span / float32 (total_beats - 1)

        for i = 0 to total_beats do
            yield 340.0f<ms> + beat_spacing * float32 i, n
    }

let line (t: Time) =
    { UninheritedTimingPoint.Create(t, 500.0, 4) with
        Effects = TimingEffect.OmitFirstBarline
    } |> Uninherited

let mutable column = 4
let note (t: Time) =
    column <- (column + 3) % 4
    HitObject.CreateManiaNote(4, column, t)

let seen = System.Collections.Generic.SortedSet<Time>()

let notes =
    seq { 0 .. 34 }
    |> Seq.collect poly
    |> Seq.filter (fun (value, n) ->
        let threshold = 2.5f<ms> * float32 n
        if seen.GetViewBetween(value - threshold, value + threshold).Count = 0 then
            seen.Add value |> ignore
            true
        else false
    )
    |> Seq.sort

let main () =
    { osu_file with
        Timing = [ line 340.0f<ms> ]
        Objects = notes |> Seq.map (fst >> note) |> List.ofSeq
    }.ToFile source