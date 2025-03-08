namespace Prelude.Calculator

open System.Collections.Generic
open Prelude
open Prelude.Charts

module Variety =

    let VARIETY_WINDOW = 750.0f<ms / rate>

    /// So basically, round all the difficulties of notes to the nearest 5bpm
    /// Then count how many unique bpms appear within +-`VARIETY_WINDOW` ms of each note
    /// Goofy but this is by FAR the best metric I have come up with for measuring variety/technicality so enjoy
    let calculate_variety (rate: Rate, notes: TimeArray<NoteRow>) (note_difficulties: NoteDifficulty array array) : float32 array =
        let keys = notes.[0].Data.Length

        let buckets = Dictionary<float32, int>()
        let mutable front = 0
        let mutable back = 0

        seq {
            for i = 0 to notes.Length - 1 do
                let now = notes.[i].Time

                while front < notes.Length && notes.[front].Time < now + VARIETY_WINDOW * rate do
                    let fnr = notes.[front].Data
                    for k = 0 to keys - 1 do
                        if fnr.[k] = NoteType.NORMAL || fnr.[k] = NoteType.HOLDHEAD then
                            let strain = note_difficulties.[front].[k].Total / 5.0f |> round
                            buckets.[strain] <- buckets.GetValueOrDefault strain + 1
                    front <- front + 1

                while back < i && notes.[back].Time < now - VARIETY_WINDOW * rate do
                    let bnr = notes.[back].Data
                    for k = 0 to keys - 1 do
                        if bnr.[k] = NoteType.NORMAL || bnr.[k] = NoteType.HOLDHEAD then
                            let strain = note_difficulties.[back].[k].Total / 5.0f |> round
                            buckets.[strain] <- buckets.GetValueOrDefault strain - 1
                            if buckets.[strain] = 0 then buckets.Remove strain |> ignore
                    back <- back + 1

                yield float32 buckets.Count
        }
        |> Array.ofSeq