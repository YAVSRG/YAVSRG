namespace Interlude.Features.Rulesets

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.Content
open Interlude.UI

type OsuRulesetPage() =
    inherit Page()

    let od = 8.0f |> Setting.bounded (0.0f, 10.0f)
    let mode = Setting.simple OsuMania.NoMod

    let create () =
        Rulesets.install (OsuMania.create od.Value mode.Value)
        Menu.Back()
        Menu.Back()

    override this.Content() =
        page_container()
            .With(
                PageSetting(%"ruleset.osu_od", Slider(od, Step = 0.1f))
                    .Pos(0),
                PageSetting(
                    %"ruleset.osu_modifier",
                    Selector(
                        [|
                            OsuMania.NoMod, %"ruleset.osu_modifier.nomod"
                            OsuMania.HardRock, %"ruleset.osu_modifier.hardrock"
                            OsuMania.Easy, %"ruleset.osu_modifier.easy"
                        |],
                        mode
                    )
                )
                    .Pos(2),
                PageButton.Once(%"rulesets.create", create)
                    .Pos(5)
            )

    override this.Title = %"rulesets.create.osu"