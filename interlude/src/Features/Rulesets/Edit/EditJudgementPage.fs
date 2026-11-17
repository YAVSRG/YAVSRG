namespace Interlude.Features.Rulesets.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.UI

type EditJudgementPage(ruleset: Setting<Ruleset>, id: int) =
    inherit Page()

    let judgement = ruleset.Value.Judgements.[id]
    let name = Setting.simple judgement.Name
    let color = Setting.simple judgement.Color
    let breaks_combo = Setting.simple judgement.BreaksCombo

    member this.SaveChanges() : unit =
        let new_js = ruleset.Value.Judgements |> Array.copy

        new_js.[id] <-
            { judgement with Name = name.Value.Trim(); Color = color.Value; BreaksCombo = breaks_combo.Value }

        ruleset.Set({ ruleset.Value with Judgements = new_js })

    override this.Content() : Widget =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageTextEntry(%"rulesets.judgement.name", name).Pos(0),
                PageSetting(
                    %"rulesets.judgement.color",
                    ColorPicker(%"rulesets.judgement.color", color, false).Preview(name.get_Value)
                )
                    .Pos(2),
                PageSetting(%"rulesets.judgement.breaks_combo", Checkbox breaks_combo).Pos(4)
            )

    override this.Title = judgement.Name