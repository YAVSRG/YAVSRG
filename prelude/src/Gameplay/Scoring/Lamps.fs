namespace Prelude.Gameplay.Scoring

open Prelude.Gameplay.Rulesets

[<RequireQualifiedAccess>]
[<Struct>]
type LampImprovement =
    | FewerOfJudgement of judgement: int * count: int
    | FewerComboBreaks of int
    | AchievedMax

[<Struct>]
type LampResult =
    {
        Lamp: int
        NextLampGoal: LampImprovement
    }

module Lamp =

    let calculate_with_target (lamps: Lamp array) (judgements: int array) (combo_breaks: int) : LampResult =

        let worst_judgement =
            let mutable w = -1
            let mutable i = 0

            while i < judgements.Length do
                if judgements.[i] > 0 then
                    w <- i

                i <- i + 1

            w

        let rec loop (achieved: int) =
            if achieved + 1 = lamps.Length then // got max grade already
                {
                    Lamp = achieved
                    NextLampGoal = LampImprovement.AchievedMax
                }
            else
                let next_lamp = lamps.[achieved + 1]

                match next_lamp.Requirement with
                | LampRequirement.ComboBreaksAtMost threshold ->
                    if combo_breaks > threshold then
                        {
                            Lamp = achieved
                            NextLampGoal = LampImprovement.FewerComboBreaks (combo_breaks - threshold)
                        }
                    else
                        loop (achieved + 1)
                | LampRequirement.JudgementAtMost (judgement, threshold) ->
                    if worst_judgement > judgement then
                        {
                            Lamp = achieved
                            NextLampGoal = LampImprovement.FewerOfJudgement (worst_judgement, judgements.[worst_judgement])
                        }
                    elif judgements.[judgement] > threshold then
                        {
                            Lamp = achieved
                            NextLampGoal = LampImprovement.FewerOfJudgement (judgement, judgements.[judgement] - threshold)
                        }
                    else
                        loop (achieved + 1)

        loop -1

    let calculate (lamps: Lamp array) (judgements: int array) (combo_breaks: int) : int =
        (calculate_with_target lamps judgements combo_breaks).Lamp