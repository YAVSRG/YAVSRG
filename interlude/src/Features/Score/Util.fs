namespace Interlude.Features.Score

open Prelude
open Prelude.Charts.Tools
open Prelude.Gameplay
open Prelude.Data.Scores

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
        let earlyTaps = ref 0
        let tap_sum = ref 0.0f<ms>
        let tap_sumOfSq = ref 0.0f<ms>
        let releases = ref 1
        let earlyReleases = ref 0
        let release_sum = ref 0.0f<ms>
        let release_sumOfSq = ref 0.0f<ms>

        let notesHit = ref 0
        let notesCount = ref 0
        let holdsHeld = ref 0
        let holdsCount = ref 0
        let releasesReleased = ref 0
        let releasesCount = ref 0

        for ev in events do
            match ev.Guts with
            | Hit e ->
                if e.IsHold then
                    if not e.Missed then
                        inc holdsHeld

                    inc holdsCount
                else
                    if not e.Missed then
                        inc notesHit

                    inc notesCount

                if e.Judgement.IsSome then
                    inc taps

                    if e.Delta < 0.0f<ms> then
                        inc earlyTaps

                    if not e.Missed then
                        tap_sum ++ e.Delta
                        tap_sumOfSq ++ e.Delta * float32 e.Delta

            | Release e ->
                if not e.Missed then
                    inc releasesReleased

                inc releasesCount

                if e.Delta < 0.0f<ms> then
                    inc earlyReleases

                if e.Judgement.IsSome then
                    inc releases

                    if not e.Missed then
                        release_sum ++ e.Delta
                        release_sumOfSq ++ e.Delta * float32 e.Delta

        let tap_mean = tap_sum.Value / float32 taps.Value
        let release_mean = release_sum.Value / float32 releases.Value

        {
            Notes = notesHit.Value, notesCount.Value
            Holds = holdsHeld.Value, holdsCount.Value
            Releases = releasesReleased.Value, releasesCount.Value

            TapMean = tap_mean
            TapStandardDeviation =
                System.MathF.Sqrt(
                    ((tap_sumOfSq.Value / float32 taps.Value * 1.0f<ms>) - tap_mean * tap_mean)
                    |> float32
                )
                * 1.0f<ms>
            TapEarlyPercent = float earlyTaps.Value / float taps.Value

            ReleaseMean = release_mean
            ReleaseStandardDeviation =
                System.MathF.Sqrt(
                    ((release_sumOfSq.Value / float32 releases.Value * 1.0f<ms>)
                     - release_mean * release_mean)
                    |> float32
                )
                * 1.0f<ms>
            ReleaseEarlyPercent = float earlyReleases.Value / float releases.Value

            JudgementCount = taps.Value + releases.Value - 2
        }

module ScoreScreenHelpers =

    let mutable watch_replay: Score * ModChart * ReplayData -> unit = ignore
