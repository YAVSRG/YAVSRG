namespace Interlude.Features.Rulesets.Edit

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.UI

type EditGradePage(ruleset: Setting<Ruleset>, id: int) =
    inherit Page()

    let grade = ruleset.Value.Grades.[id]
    let name = Setting.simple grade.Name
    let color = Setting.simple grade.Color
    let acc_required = grade.Accuracy |> Setting.bounded(0.0, 1.0) |> Setting.round 6

    member this.SaveChanges() : unit =
        let new_grades = ruleset.Value.Grades |> Array.copy

        new_grades.[id] <-
            { Name = name.Value.Trim(); Color = color.Value; Accuracy = System.Math.Round(acc_required.Value, 6) }

        ruleset.Set({ ruleset.Value with Grades = new_grades |> Array.sortBy(fun l -> l.Accuracy) })

    override this.Content() : Widget =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageTextEntry(%"rulesets.grade.name", name).Pos(0),
                PageSetting(
                    %"rulesets.grade.color",
                    ColorPicker(%"rulesets.grade.color", color, false).Preview(name.get_Value)
                )
                    .Pos(2),
                PageSetting(%"rulesets.grade.accuracy", NumberEntry.CreatePercent(acc_required)).Pos(4)
            )

    override this.Title = grade.Name