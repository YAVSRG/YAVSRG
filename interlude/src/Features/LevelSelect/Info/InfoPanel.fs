namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Interlude.Features.Rulesets
open Interlude.Options
open Interlude.UI
open Interlude.Features.Gameplay

type InfoPanel() as this =
    inherit Container(NodeType.None)

    let display = Setting.simple Display.Local |> Setting.trigger (fun _ -> this.Refresh())

    let scoreboard = Scoreboard(display, Position = Position.ShrinkB(ChartDetails.HEIGHT + 55.0f))
    let online = Leaderboard(display, Position = Position.ShrinkB(ChartDetails.HEIGHT + 55.0f))
    let patterns = Patterns(display, Position = Position.ShrinkB(ChartDetails.HEIGHT + 55.0f))

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

        |+ ChartDetails(Position = Position.SliceB(ChartDetails.HEIGHT).TranslateY(-50.0f))

        |+ StylishButton(
            (fun () -> SelectedChart.when_loaded false <| fun info -> Preview(info, change_rate).Show()),
            K(sprintf "%s %s" Icons.EYE %"levelselect.preview"),
            !%Palette.MAIN_100,
            Hotkey = "preview",
            TiltLeft = false,
            Position =
                {
                    Left = 0.0f %+ 0.0f
                    Top = 1.0f %- 50.0f
                    Right = 0.33f %- 25.0f
                    Bottom = 1.0f %- 0.0f
                }
        )
            .Help(Help.Info("levelselect.preview", "preview"))

        |+ ModSelect(
            change_rate,
            scoreboard.ModsChanged,
            Position =
                {
                    Left = 0.33f %+ 0.0f
                    Top = 1.0f %- 50.0f
                    Right = 0.66f %- 25.0f
                    Bottom = 1.0f %- 0.0f
                }
        )
            .Help(Help.Info("levelselect.mods", "mods"))

        |* RulesetSwitcher(
            options.SelectedRuleset,
            Position =
                {
                    Left = 0.66f %+ 0.0f
                    Top = 1.0f %- 50.0f
                    Right = 1.0f %- 0.0f
                    Bottom = 1.0f %- 0.0f
                }
        )
            .Help(Help.Info("levelselect.rulesets", "ruleset_switch"))

        LevelSelect.on_refresh_all.Add this.Refresh
        LevelSelect.on_refresh_details.Add this.Refresh
        SelectedChart.on_chart_change_finished.Add this.OnChartUpdated

        base.Init(parent)

    override this.Draw() =
        let info_area = this.Bounds.SliceB(ChartDetails.HEIGHT).TranslateY(-50.0f)
        Render.rect (info_area.BorderT(Style.PADDING)) (Palette.color (255, 0.8f, 0.0f))
        Render.rect info_area (!*Palette.DARK_100)
        base.Draw()

    member this.OnChartUpdated(info: LoadedChartInfo) =
        match display.Value with
        | Display.Patterns -> patterns.OnChartUpdated(info)
        | _ -> ()

    member this.Refresh() = SelectedChart.when_loaded false this.OnChartUpdated