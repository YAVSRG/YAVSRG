namespace Prelude.Gameplay.ScoringV2

open Prelude.Gameplay.RulesetsV2

[<Struct>]
type GradeResult =
    {
        Grade: int
        AccuracyIncreaseForNextGrade: float option
    }

module Grade =

    let calculate_with_target (grades: Grade array) (accuracy: float) : GradeResult =

        let rec loop (achieved: int) =
            if achieved + 1 = grades.Length then // got max grade already
                {
                    Grade = achieved
                    AccuracyIncreaseForNextGrade = None
                }
            else
                let accuracy_needed = grades.[achieved + 1].Accuracy - accuracy

                if accuracy_needed > 0.0 then
                    {
                        Grade = achieved
                        AccuracyIncreaseForNextGrade = Some accuracy_needed
                    }
                else
                    loop (achieved + 1)

        loop -1

    let calculate (grades: Grade array) (accuracy: float) =
        (calculate_with_target grades accuracy).Grade