namespace Interlude.Features.Rulesets.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.UI

type ConfigureAccuracyPage(ruleset: Setting<Ruleset>) =
    inherit Page()

    let is_wife_curve =
        match ruleset.Value.Accuracy with
        | AccuracyPoints.WifeCurve _ -> true
        | AccuracyPoints.PointsPerJudgement _ -> false
        |> Setting.simple

    let wife_judge =
        match ruleset.Value.Accuracy with
        | AccuracyPoints.WifeCurve j -> j
        | AccuracyPoints.PointsPerJudgement _ -> 4
        |> Setting.simple

    let points_per_judgement : float array =
        match ruleset.Value.Accuracy with
        | AccuracyPoints.WifeCurve _ -> Array.create ruleset.Value.Judgements.Length 1.0
        | AccuracyPoints.PointsPerJudgement p -> p

    let decimal_places = Setting.simple ruleset.Value.Formatting.DecimalPlaces

    member this.SaveChanges() =
        ruleset.Set
            { ruleset.Value with
                Formatting = { DecimalPlaces = decimal_places.Value }
                Accuracy =
                    if is_wife_curve.Value then
                        AccuracyPoints.WifeCurve wife_judge.Value
                    else
                        AccuracyPoints.PointsPerJudgement points_per_judgement
            }

    override this.Content() =
        this.OnClose(this.SaveChanges)

        let judgements_container = FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)

        for i, j in ruleset.Value.Judgements |> Array.indexed do
            let setting =
                Setting.make
                    (fun v -> points_per_judgement.[i] <- v)
                    (fun () -> points_per_judgement.[i])
                |> Setting.bound (-10.0, 1.0)
            judgements_container.Add (PageSetting(j.Name, NumberEntry.Create setting))

        page_container()
            .With(
                PageSetting(%"rulesets.accuracy.decimal_places",
                    SelectDropdown([| DecimalPlaces.TWO, "2"; DecimalPlaces.THREE, "3"; DecimalPlaces.FOUR, "4" |], decimal_places)
                )
                    .Pos(0),
                PageSetting(%"rulesets.accuracy.accuracy_type",
                    SelectDropdown([| true, "Wife3"; false, %"rulesets.accuracy.accuracy_type.points_per_judgement" |], is_wife_curve)
                )
                    .Pos(3),
                PageSetting(%"rulesets.accuracy.wife_judge",
                    SelectDropdown([| 4, "4"; 5, "5"; 6, "6"; 7, "7"; 8, "8"; 9, "JUSTICE"|], wife_judge)
                )
                    .Conditional(is_wife_curve.Get)
                    .Pos(5),
                ScrollContainer(judgements_container)
                    .Conditional(is_wife_curve.Get >> not)
                    .Pos(5, PAGE_BOTTOM - 5)
            )
    override this.Title = %"rulesets.edit.accuracy"