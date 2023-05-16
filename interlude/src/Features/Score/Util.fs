namespace Interlude.Features.Score

open Prelude
open Prelude.Charts.Tools
open Prelude.Gameplay

type ScoreScreenStats =
    {
        Notes: int * int
        Holds: int * int
        Releases: int * int

        Mean: Time
        StandardDeviation: Time
        EarlyMean: Time
        LateMean: Time

        JudgementCount: int
    }
    static member Generate(events: HitEvent<HitEventGuts> seq) =
        let inc (x: int ref) = x.Value <- x.Value + 1
        let (++) (x: Time ref) (t: Time) = x.Value <- x.Value + t

        let sum = ref 0.0f<ms>
        let sumOfSq = ref 0.0f<ms>
        let earlySum = ref 0.0f<ms>
        let lateSum = ref 0.0f<ms>

        let judgementCount = ref 0
        
        let notesHit = ref 0
        let notesCount = ref 0
        let holdsHeld = ref 0
        let holdsCount = ref 0
        let releasesReleased = ref 0
        let releasesCount = ref 0

        let earlyHitCount = ref 0
        let lateHitCount = ref 0

        for ev in events do
            match ev.Guts with
            | Hit e ->
                if e.IsHold then
                    if not e.Missed then inc holdsHeld
                    inc holdsCount
                else
                    if not e.Missed then inc notesHit
                    inc notesCount
                if e.Judgement.IsSome then
                    inc judgementCount
                    if not e.Missed then
                        if e.Delta < 0.0f<ms> then
                            earlySum ++ e.Delta
                            inc earlyHitCount
                        else
                            lateSum ++ e.Delta
                            inc lateHitCount
                        sum ++ e.Delta
                        sumOfSq ++ e.Delta * float32 e.Delta
            | Release e ->
                if not e.Missed then inc releasesReleased
                inc releasesCount
                if e.Judgement.IsSome then
                    inc judgementCount
                    if not e.Missed then
                        if e.Delta < 0.0f<ms> then
                            earlySum ++ e.Delta
                            inc earlyHitCount
                        else
                            lateSum ++ e.Delta
                            inc lateHitCount
                        sum ++ e.Delta
                        sumOfSq ++ e.Delta * float32 e.Delta

        let judgementCount = max 1 judgementCount.Value
        let mean = sum.Value / float32 judgementCount
        {
            Notes = notesHit.Value, notesCount.Value
            Holds = holdsHeld.Value, holdsCount.Value
            Releases = releasesReleased.Value, releasesCount.Value

            Mean = mean
            EarlyMean = earlySum.Value / (max 1.0f (float32 earlyHitCount.Value))
            LateMean = lateSum.Value / (max 1.0f (float32 lateHitCount.Value))
            StandardDeviation = System.MathF.Sqrt( ((sumOfSq.Value / float32 judgementCount * 1.0f<ms>) - mean * mean) |> float32 ) * 1.0f<ms>

            JudgementCount = judgementCount
        }

module ScoreScreenHelpers =

    let mutable watchReplay : ModChart * float32 * ReplayData -> unit = ignore