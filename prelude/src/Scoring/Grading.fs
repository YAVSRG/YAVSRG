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



[<RequireQualifiedAccess>]
type Improvement<'T> =
    | FasterBetter of rate_increase: float32 * improvement: 'T
    | Faster of rate_increase: float32
    | Better of improvement: 'T
    | New
    | None

module Improvement =

    let map (f: 'T -> 'U) (i: Improvement<'T>) =
        match i with
        | Improvement.FasterBetter (r, i) -> Improvement.FasterBetter (r, f i)
        | Improvement.Faster r -> Improvement.Faster r
        | Improvement.Better i -> Improvement.Better (f i)
        | Improvement.New -> Improvement.New
        | Improvement.None -> Improvement.None

[<Json.AutoCodec>]
type PersonalBests<'T> = { Best: 'T * float32; Fastest: 'T * float32 }

module PersonalBests =

    let create (value: 'T, rate: float32) : PersonalBests<'T> = { Best = value, rate; Fastest = value, rate }

    let map (f: 'T -> 'U) ({ Best = best_overall, best_rate; Fastest = best_fastest, fastest_rate }: PersonalBests<'T>) : PersonalBests<'U> =
        { Best = f best_overall, best_rate; Fastest = f best_fastest, fastest_rate }

    let inline update (value: 'T, rate: float32) ({ Best = best_overall, best_rate; Fastest = best_fastest, fastest_rate }: PersonalBests<'T>) =

        let new_fastest_best, f_rate_increase, f_improvement =
            if rate > fastest_rate then (value, rate), Some (rate - fastest_rate), None
            elif rate = fastest_rate then
                if value > best_fastest then (value, rate), None, Some (value - best_fastest)
                else (best_fastest, rate), None, None
            else (best_fastest, fastest_rate), None, None

        let new_overall_best, b_rate_increase, b_improvement =
            if value > best_overall then (value, rate), None, Some (value - best_overall)
            elif value = best_overall then
                if rate > best_rate then (value, rate), Some (rate - best_rate), None
                else (best_overall, best_rate), None, None
            else (best_overall, best_rate), None, None

        let result = { Best = new_overall_best; Fastest = new_fastest_best }
        let info =

            let rate_increase =
                match f_rate_increase, b_rate_increase with
                | Some f, Some b -> Some (max f b)
                | Some f, None -> Some f
                | None, Some b -> Some b
                | None, None -> None
            
            let improvement =
                match f_improvement, b_improvement with
                | Some f, Some b -> Some (max f b)
                | Some f, None -> Some f
                | None, Some b -> Some b
                | None, None -> None

            match rate_increase, improvement with
            | Some r, Some i -> Improvement.FasterBetter (r, i)
            | Some r, None -> Improvement.Faster r
            | None, Some i -> Improvement.Better i
            | None, None -> Improvement.None

        result, info

    let best_this_rate (rate: float32) ({ Best = p1, r1; Fastest = p2, r2 }: PersonalBests<'T>) : 'T option =
        if r1 < rate then
            if r2 < rate then None else Some p2
        else Some p1