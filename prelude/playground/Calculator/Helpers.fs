namespace Prelude.Playground.Calculator

open Prelude
open Prelude.Charts

module ChartBuilder =

    let [<Literal>] INTERVAL = MS_PER_MINUTE / 120.0f<beat/minute> / 4.0f</beat>
    let repeated_pattern (keys: int, duration: Time, pattern: Bitmask array) : NoteData =
        let inline loop_notes() = 
            seq {
                let mutable last_time = 0.0f<ms>
                let mutable i = 0
                while last_time < duration do
                    yield { Time = last_time; Data = NoteRow.create_notes keys pattern.[i] }
                    i <- (i + 1) % pattern.Length
                    last_time <- last_time + INTERVAL
            }
        { Keys = keys; Notes = Array.ofSeq(loop_notes()) }