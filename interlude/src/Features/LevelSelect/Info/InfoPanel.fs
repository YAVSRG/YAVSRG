namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Interlude.Features.Rulesets
open Interlude.Options
open Interlude.UI
open Interlude.Features.Gameplay

type InfoPanel() =
    inherit Container(NodeType.None)

    let display = Setting.simple Display.Local

    let scoreboard = Scoreboard(display, Position = Position.ShrinkB(GameplayInfo.HEIGHT + 55.0f))
    let online = Leaderboard(display, Position = Position.ShrinkB(GameplayInfo.HEIGHT + 55.0f))
    let patterns = Patterns(display, Position = Position.ShrinkB(GameplayInfo.HEIGHT + 55.0f))

    override this.Init(parent) =
        let change_rate change_rate_by =
            if Transitions.in_progress() then
                ()
            else
                SelectedChart.rate.Value <- SelectedChart.rate.Value + change_rate_by
                LevelSelect.refresh_details ()

        this
        |+ scoreboard.Conditional(fun () -> display.Value = Display.Local)
        |+ online.Conditional(fun () -> display.Value = Display.Online)
        |+ patterns.Conditional(fun () -> display.Value = Display.Patterns)

        |+ GameplayInfo()
            .Position(Position.SliceB(GameplayInfo.HEIGHT).TranslateY(-50.0f))

        |+ AngledButton(
            sprintf "%s %s" Icons.EYE %"levelselect.preview",
            (fun () -> SelectedChart.when_loaded false <| fun info -> Preview(info, change_rate).Show()),
            Palette.MAIN_100
        )
            .Hotkey("preview")
            .LeanLeft(false)
            .Position(
                Position
                    .SliceB(AngledButton.HEIGHT)
                    .GridX(1, 3, AngledButton.LEAN_AMOUNT)
            )
            .Help(Help.Info("levelselect.preview", "preview"))

        |+ ModSelect(
            change_rate,
            scoreboard.ModsChanged
        )
            .Position(
                Position
                    .SliceB(AngledButton.HEIGHT)
                    .GridX(2, 3, AngledButton.LEAN_AMOUNT)
            )
            .Help(Help.Info("levelselect.mods", "mods"))

        |* RulesetSwitcher(options.SelectedRuleset)
            .Position(
                Position
                    .SliceB(AngledButton.HEIGHT)
                    .GridX(3, 3, AngledButton.LEAN_AMOUNT)
            )
            .Help(Help.Info("levelselect.rulesets", "ruleset_switch"))

        base.Init(parent)

    override this.Draw() =
        let info_area = this.Bounds.SliceB(GameplayInfo.HEIGHT).TranslateY(-50.0f)
        Render.rect (info_area.BorderT(Style.PADDING)) (!*Palette.MAIN)
        Render.rect info_area (!*Palette.DARK_100)
        base.Draw()