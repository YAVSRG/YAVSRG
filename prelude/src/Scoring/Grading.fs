namespace Prelude.Scoring.Grading

open Percyqaz.Json
open Prelude.Scoring

module Lamp =

    type LampResult =
        {
            Lamp: int
            /// Improvement needed to get next lamp; None if you got the best lamp
            ImprovementNeeded: {| Judgement: JudgementId; LessNeeded: int|} option
        }

    let calculateWithTarget (lamps: Lamp array) (state: AccuracySystemState) : LampResult =

        let worstJudgement =
            let mutable w = -1
            let mutable i = 0
            while i < state.Judgements.Length do
                if state.Judgements.[i] > 0 then w <- i
                i <- i + 1
            w

        let rec loop (achieved: int) =
            if achieved + 1 = lamps.Length then // got max grade already
                { Lamp = achieved; ImprovementNeeded = None }
            else
                let nextLamp = lamps.[achieved + 1]
                if nextLamp.Judgement < 0 then // then it refers to cbs
                    if state.ComboBreaks > nextLamp.JudgementThreshold then
                        { Lamp = achieved; ImprovementNeeded = Some {| Judgement = -1; LessNeeded = state.ComboBreaks - nextLamp.JudgementThreshold |} }
                    else loop (achieved + 1)
                else
                    if worstJudgement > nextLamp.Judgement then
                        { Lamp = achieved; ImprovementNeeded = Some {| Judgement = worstJudgement; LessNeeded = state.Judgements.[worstJudgement] |} }
                    elif state.Judgements.[nextLamp.Judgement] > nextLamp.JudgementThreshold then
                        { Lamp = achieved; ImprovementNeeded = Some {| Judgement = nextLamp.Judgement; LessNeeded = state.Judgements.[nextLamp.Judgement] - nextLamp.JudgementThreshold |} }
                    else loop (achieved + 1)
        loop -1
    
    let calculate (lamps: Lamp array) (state: AccuracySystemState) : int =
        (calculateWithTarget lamps state).Lamp

module Grade =

    type GradeResult =
        {
            Grade: int
            /// Improvement needed to get next grade; None if you got the best grade
            AccuracyNeeded: float option
        }

    let calculateWithTarget (grades: Grade array) (state: AccuracySystemState) : GradeResult =
        let percent = state.PointsScored / state.MaxPointsScored

        let rec loop (achieved: int) =
            if achieved + 1 = grades.Length then // got max grade already
                { Grade = achieved; AccuracyNeeded = None }
            else
                let accuracyNeeded = grades.[achieved + 1].Accuracy - percent
                if accuracyNeeded > 0.0 then { Grade = achieved; AccuracyNeeded = Some accuracyNeeded }
                else loop (achieved + 1)

        loop -1

    let calculate (grades: Grade array) (state: AccuracySystemState) =
        (calculateWithTarget grades state).Grade


[<Json.AutoCodec>]
type PersonalBests<'T> = { Best: 'T * float32; Fastest: 'T * float32 }

type PersonalBestType =
    | FasterBetter = 3
    | Faster = 2
    | Better = 1
    | None = 0

module PersonalBests =

    let create (value: 'T, rate: float32) : PersonalBests<'T> = { Best = value, rate; Fastest = value, rate }

    let update (value: 'T, rate: float32) ({ Best = bestA, rateA; Fastest = bestR, rateR }: PersonalBests<'T>) =
        let r, rv =
            if rate > rateR then (value, rate), PersonalBestType.Faster
            elif rate = rateR then
                if value > bestR then (value, rate), PersonalBestType.Faster else (bestR, rate), PersonalBestType.None
            else (bestR, rateR), PersonalBestType.None
        let a, av =
            if value > bestA then (value, rate), PersonalBestType.Better
            elif value = bestA then
                if rate > rateA then (value, rate), PersonalBestType.Better else (bestA, rateA), PersonalBestType.None
            else (bestA, rateA), PersonalBestType.None
        { Best = a; Fastest = r }, (av ||| rv) : PersonalBests<'T> * PersonalBestType

    let best_this_rate (rate: float32) ({ Best = p1, r1; Fastest = p2, r2 }: PersonalBests<'T>) : 'T option =
        if r1 < rate then
            if r2 < rate then None else Some p2
        else Some p1