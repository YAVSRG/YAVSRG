namespace Interlude.Features.Rulesets.Edit

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.UI

type EditLampPage(ruleset: Setting<Ruleset>, id: int) =
    inherit Page()

    let lamp = ruleset.Value.Lamps.[id]
    let name = Setting.simple lamp.Name
    let color = Setting.simple lamp.Color

    let judgement_type =
        Setting.simple(
            match lamp.Requirement with
            | LampRequirement.ComboBreaksAtMost _ -> -1
            | LampRequirement.JudgementAtMost(j, _) -> j
        )

    let judgement_threshold =
        Setting.simple(
            match lamp.Requirement with
            | LampRequirement.ComboBreaksAtMost n -> n
            | LampRequirement.JudgementAtMost(_, n) -> min 99 n
        )

    member this.SaveChanges() : unit =
        let new_lamps = ruleset.Value.Lamps |> Array.copy

        new_lamps.[id] <-
            {
                Name = name.Value.Trim()
                Color = color.Value
                Requirement =
                    if judgement_type.Value < 0 then
                        LampRequirement.ComboBreaksAtMost(judgement_threshold.Value)
                    else
                        LampRequirement.JudgementAtMost(judgement_type.Value, judgement_threshold.Value)
            }

        ruleset.Set({ ruleset.Value with Lamps = new_lamps |> Array.sortByDescending(fun l -> l.Requirement.SortKey) })

    override this.Content() : Widget =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageTextEntry(%"rulesets.lamp.name", name).Pos(0),
                PageSetting(
                    %"rulesets.lamp.color",
                    ColorPicker(%"rulesets.lamp.color", color, false).Preview(name.get_Value)
                )
                    .Pos(2),
                PageSetting(
                    %"rulesets.lamp.requirement",
                    NavigationContainer
                        .Row()
                        .With(
                            SelectDropdown(
                                seq {
                                    for i, j in ruleset.Value.Judgements |> Array.indexed do
                                        yield i, j.Name

                                    yield -1, %"rulesets.lamp.requirement.combo_breaks"
                                }
                                |> Array.ofSeq,
                                judgement_type
                            )
                                .Position(Position.GridX(1, 2, 200.0f)),

                            Text("<="),

                            Selector([| 99, "99"; 9, "9"; 1, "1"; 0, "0" |], judgement_threshold)
                                .Position(Position.GridX(2, 2, 200.0f))
                        )
                )
                    .Pos(4)
            )

    override this.Title = lamp.Name
