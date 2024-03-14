namespace Interlude.Features.Score

open Prelude
open Prelude.Charts.Processing.NoteColors
open Prelude.Gameplay
open Prelude.Charts
open Prelude.Data

type ScoreScreenStats =
    {
        Notes: int * int
        Holds: int * int
        Releases: int * int

        TapMean: Time
        TapStandardDeviation: Time
        TapEarlyPercent: float

        ReleaseMean: Time
        ReleaseStandardDeviation: Time
        ReleaseEarlyPercent: float

        JudgementCount: int
    }
    static member Generate(events: HitEvent<HitEventGuts> seq) =
        let inc (x: int ref) = x.Value <- x.Value + 1
        let (++) (x: Time ref) (t: Time) = x.Value <- x.Value + t

        let taps = ref 1
        let early_taps = ref 0
        let tap_sum = ref 0.0f<ms>
        let tap_sumOfSq = ref 0.0f<ms>
        let releases = ref 1
        let early_releases = ref 0
        let release_sum = ref 0.0f<ms>
        let release_sumOfSq = ref 0.0f<ms>

        let notes_hit = ref 0
        let notes_count = ref 0
        let holds_held = ref 0
        let holds_count = ref 0
        let releases_released = ref 0
        let releases_count = ref 0

        for ev in events do
            match ev.Guts with
            | Hit e ->
                if e.IsHold then
                    if not e.Missed then
                        inc holds_held

                    inc holds_count
                else
                    if not e.Missed then
                        inc notes_hit

                    inc notes_count

                if e.Judgement.IsSome then
                    inc taps

                    if e.Delta < 0.0f<ms> then
                        inc early_taps

                    if not e.Missed then
                        tap_sum ++ e.Delta
                        tap_sumOfSq ++ e.Delta * float32 e.Delta

            | Release e ->
                if not e.Missed then
                    inc releases_released

                inc releases_count

                if e.Delta < 0.0f<ms> then
                    inc early_releases

                if e.Judgement.IsSome then
                    inc releases

                    if not e.Missed then
                        release_sum ++ e.Delta
                        release_sumOfSq ++ e.Delta * float32 e.Delta

        let tap_mean = tap_sum.Value / float32 taps.Value
        let release_mean = release_sum.Value / float32 releases.Value

        {
            Notes = notes_hit.Value, notes_count.Value
            Holds = holds_held.Value, holds_count.Value
            Releases = releases_released.Value, releases_count.Value

            TapMean = tap_mean
            TapStandardDeviation =
                System.MathF.Sqrt(
                    ((tap_sumOfSq.Value / float32 taps.Value * 1.0f<ms>) - tap_mean * tap_mean)
                    |> float32
                )
                * 1.0f<ms>
            TapEarlyPercent = float early_taps.Value / float taps.Value

            ReleaseMean = release_mean
            ReleaseStandardDeviation =
                System.MathF.Sqrt(
                    ((release_sumOfSq.Value / float32 releases.Value * 1.0f<ms>)
                     - release_mean * release_mean)
                    |> float32
                )
                * 1.0f<ms>
            ReleaseEarlyPercent = float early_releases.Value / float releases.Value

            JudgementCount = taps.Value + releases.Value - 2
        }

module ScoreScreenHelpers =

    let mutable watch_replay: ScoreInfo * ColoredChart -> unit = ignore

    let mutable continue_endless_mode: unit -> bool = fun () -> false
