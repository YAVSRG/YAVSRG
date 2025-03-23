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
        |+ PageSetting(%"ruleset.osu_od", Slider(od, Step = 0.1f)).Pos(0)
        |+ PageSetting(
            %"ruleset.osu_modifier",
            Selector(
                [|
                    OsuMania.NoMod, %"ruleset.osu_modifier.nomod"
                    OsuMania.HardRock, %"ruleset.osu_modifier.hardrock"
                    OsuMania.Easy, %"ruleset.osu_modifier.easy"
                |],
                mode
            )
        ).Pos(2)
        |+ PageButton.Once(%"rulesets.create", create).Pos(5)
        :> Widget

    override this.Title = %"rulesets.create.osu"
    override this.OnClose() = ()

type QuaverRulesetPage() =
    inherit Page()

    let judgement = Setting.simple Quaver.Standard

    let create () =
        Rulesets.install (Quaver.create judgement.Value)
        Menu.Back()
        Menu.Back()

    override this.Content() =
        page_container()
        |+ PageSetting(
            %"ruleset.quaver_judgement",
            SelectDropdown(
                Quaver.Judgement.LIST |> Array.map (fun j -> j, j.ToString()),
                judgement
            )
        ).Pos(0)
        |+ PageButton.Once(%"rulesets.create", create).Pos(3)
        :> Widget

    override this.Title = %"rulesets.create.quaver"
    override this.OnClose() = ()

type WifeRulesetPage() =
    inherit Page()

    let judge = Setting.simple 4

    let create () =
        Rulesets.install (Wife3.create judge.Value)
        Menu.Back()
        Menu.Back()

    override this.Content() =
        page_container()
        |+ PageSetting(%"ruleset.wife_judge",
            SelectDropdown(
                [|
                    4, "4"
                    5, "5"
                    6, "6"
                    7, "7"
                    8, "8"
                    9, "JUSTICE"
                |],
                judge
            )
        ).Pos(0, 2, PageWidth.Custom (PAGE_LABEL_WIDTH + 200.0f))
        |+ PageButton.Once(%"rulesets.create", create).Pos(3)
        :> Widget

    override this.Title = %"rulesets.create.wife"
    override this.OnClose() = ()

type SCRulesetPage() =
    inherit Page()

    let judge = Setting.simple 4

    let create () =
        Rulesets.install (SC.create judge.Value)
        Menu.Back()
        Menu.Back()

    override this.Content() =
        page_container()
        |+ PageSetting(%"ruleset.sc_judge",
            SelectDropdown(
                [|
                    2, "2"
                    3, "3"
                    4, "4"
                    5, "5"
                    6, "6"
                    7, "7"
                    8, "8"
                    9, "9"
                |],
                judge
            )
        ).Pos(0, 2, PageWidth.Custom (PAGE_LABEL_WIDTH + 200.0f))
        |+ PageButton.Once(%"rulesets.create", create).Pos(3)
        :> Widget

    override this.Title = %"rulesets.create.sc"
    override this.OnClose() = ()