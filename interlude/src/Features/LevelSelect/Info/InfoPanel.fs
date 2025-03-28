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

    let info_panel_mode = Setting.simple InfoPanelMode.Local

    override this.Init(parent: Widget) =

        let change_rate (change_rate_by: Rate) : unit =
            if Transitions.in_progress() then
                ()
            else
                SelectedChart.rate.Value <- SelectedChart.rate.Value + change_rate_by
                LevelSelect.refresh_details ()

        let main_display =
            Container(NodeType.None)
                .Position(Position.ShrinkB(GameplayInfo.HEIGHT + AngledButton.HEIGHT + Style.PADDING))
                .With(
                    Scoreboard(info_panel_mode)
                        .Conditional(fun () -> info_panel_mode.Value = InfoPanelMode.Local),
                    Leaderboard(info_panel_mode)
                        .Conditional(fun () -> info_panel_mode.Value = InfoPanelMode.Online),
                    Patterns(info_panel_mode)
                        .Conditional(fun () -> info_panel_mode.Value = InfoPanelMode.Patterns)
                )

        this
            .Add(
                main_display,

                GameplayInfo()
                    .Position(Position.SliceB(AngledButton.HEIGHT, GameplayInfo.HEIGHT)),

                AngledButton(
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
                    .Help(Help.Info("levelselect.preview", "preview")),

                ModSelect(change_rate)
                    .Position(
                        Position
                            .SliceB(AngledButton.HEIGHT)
                            .GridX(2, 3, AngledButton.LEAN_AMOUNT)
                    )
                    .Help(Help.Info("levelselect.mods", "mods")),

                RulesetSwitcher(options.SelectedRuleset)
                    .Position(
                        Position
                            .SliceB(AngledButton.HEIGHT)
                            .GridX(3, 3, AngledButton.LEAN_AMOUNT)
                    )
                    .Help(Help.Info("levelselect.rulesets", "ruleset_switch"))
            )

        base.Init(parent)

    override this.Draw() =
        let info_area = this.Bounds.SliceB(GameplayInfo.HEIGHT).TranslateY(-50.0f)
        Render.rect (info_area.BorderT(Style.PADDING)) (!*Palette.MAIN)
        Render.rect info_area (!*Palette.DARK_100)
        base.Draw()