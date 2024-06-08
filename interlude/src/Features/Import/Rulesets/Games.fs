namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.PremadeRulesets
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Content

type OsuRulesetPage() =
    inherit Page()

    let od = Setting.bounded 8.0f 0.0f 10.0f
    let mode = Setting.simple ``osu!``.NoMod

    let create () =
        let ruleset_id = 
            sprintf "osu-od-%.1f%s" 
                od.Value
                (
                    match mode.Value with 
                    | ``osu!``.NoMod -> "" 
                    | ``osu!``.HardRock -> "-hr" 
                    | ``osu!``.Easy -> "-ez"
                )
        Rulesets.install_or_update ruleset_id (``osu!``.create od.Value mode.Value)
        Menu.Back()

    override this.Content() =
        page_container()
        |+ PageSetting(%"ruleset.osu_od", Slider(od, Step = 0.1f)).Pos(0)
        |+ PageSetting(
            %"ruleset.osu_modifier", 
            Selector(
                [| 
                    ``osu!``.NoMod, %"ruleset.osu_modifier.nomod"
                    ``osu!``.HardRock, %"ruleset.osu_modifier.hardrock"
                    ``osu!``.Easy, %"ruleset.osu_modifier.easy"
                |], 
                mode
            )
        ).Pos(2)
        |+ PageButton.Once(%"ruleset.create", create).Pos(5)
        :> Widget

    override this.Title = %"confirm"
    override this.OnClose() = ()