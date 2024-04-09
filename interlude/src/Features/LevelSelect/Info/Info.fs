namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts
open Prelude.Charts.Processing.Difficulty
open Prelude.Gameplay.Mods
open Interlude.Features
open Interlude.Options
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Gameplay

type ChartInfo() as this =
    inherit Container(NodeType.None)

    let display =
        Setting.simple Display.Local |> Setting.trigger (fun _ -> this.Refresh())

    let scoreboard = Scoreboard(display, Position = Position.TrimBottom 170.0f)
    let online = Leaderboard(display, Position = Position.TrimBottom 170.0f)
    let patterns = Patterns(display, Position = Position.TrimBottom 170.0f)

    let mutable rating = 0.0
    let mutable notecounts = ""

    do
        let change_rate change_rate_by =
            if Transitions.active then
                ()
            else
                rate.Value <- rate.Value + change_rate_by
                LevelSelect.refresh_details ()

        this
        |+ Conditional((fun () -> display.Value = Display.Local), scoreboard)
        |+ Conditional((fun () -> display.Value = Display.Online), online)
        |+ Conditional((fun () -> display.Value = Display.Patterns), patterns)

        |+ Text(
            (fun () -> sprintf "%s %.2f" Icons.STAR rating),
            Color = (fun () -> Color.White, DifficultyRating.physical_color rating),
            Align = Alignment.LEFT,
            Position =
                {
                    Left = 0.0f %+ 10.0f
                    Top = 1.0f %- 170.0f
                    Right = 0.33f %- 10.0f
                    Bottom = 1.0f %- 100.0f
                }
        )

        |+ Text(
            (fun () -> Chart.FMT_BPM),
            Align = Alignment.CENTER,
            Position =
                {
                    Left = 0.33f %+ 10.0f
                    Top = 1.0f %- 170.0f
                    Right = 0.66f %- 10.0f
                    Bottom = 1.0f %- 100.0f
                }
        )

        |+ Text(
            (fun () -> Chart.FMT_DURATION),
            Align = Alignment.RIGHT,
            Position =
                {
                    Left = 0.66f %+ 10.0f
                    Top = 1.0f %- 170.0f
                    Right = 1.0f %- 10.0f
                    Bottom = 1.0f %- 100.0f
                }
        )

        |+ Text(
            (fun () -> notecounts),
            Align = Alignment.RIGHT,
            Position =
                {
                    Left = 0.0f %+ 10.0f
                    Top = 1.0f %- 100.0f
                    Right = 1.0f %- 17.0f
                    Bottom = 1.0f %- 60.0f
                }
        )

        |+ (Container(
                NodeType.None,
                Position =
                    {
                        Left = 0.0f %+ 10.0f
                        Top = 1.0f %- 100.0f
                        Right = 0.5f %- 10.0f
                        Bottom = 1.0f %- 60.0f
                    }
            )
            |+ Text((fun () -> Mods.format (rate.Value, selected_mods.Value, autoplay)), Align = Alignment.LEFT))
            .Tooltip(
                Tooltip
                    .Info("levelselect.selected_mods")
                    .Hotkey(%"levelselect.selected_mods.mods.hint", "mods")
                    .Body(%"levelselect.selected_mods.rate.hint")
                    .Hotkey(%"levelselect.selected_mods.uprate.hint", "uprate")
                    .Hotkey(%"levelselect.selected_mods.downrate.hint", "downrate")
            )

        |+ StylishButton(
            (fun () -> Chart.when_loaded <| fun info -> Preview(info, change_rate).Show()),
            K(sprintf "%s %s" Icons.EYE %"levelselect.preview.name"),
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
            .Tooltip(Tooltip.Info("levelselect.preview", "preview"))

        |+ ModSelect(
            change_rate,
            scoreboard.Refresh,
            Position =
                {
                    Left = 0.33f %+ 0.0f
                    Top = 1.0f %- 50.0f
                    Right = 0.66f %- 25.0f
                    Bottom = 1.0f %- 0.0f
                }
        )
            .Tooltip(Tooltip.Info("levelselect.mods", "mods"))

        |* Rulesets
            .QuickSwitcher(
                options.SelectedRuleset |> Setting.trigger (ignore >> LevelSelect.refresh_all),
                Position =
                    {
                        Left = 0.66f %+ 0.0f
                        Top = 1.0f %- 50.0f
                        Right = 1.0f %- 0.0f
                        Bottom = 1.0f %- 0.0f
                    }
            )
            .Tooltip(
                Tooltip
                    .Info("levelselect.rulesets", "ruleset_switch")
            )

        LevelSelect.on_refresh_all.Add this.Refresh
        LevelSelect.on_refresh_details.Add this.Refresh
        Chart.on_chart_change_finished.Add this.OnChartUpdated
        Chart.on_chart_update_finished.Add (fun info -> rating <- info.Rating.Physical; notecounts <- info.NotecountsString)

    member this.OnChartUpdated(info: Chart.LoadedChartInfo) =
        match display.Value with
        | Display.Local -> scoreboard.OnChartUpdated(info)
        | Display.Online -> online.OnChartUpdated(info)
        | Display.Patterns -> patterns.OnChartUpdated(info)

        rating <- info.Rating.Physical
        notecounts <- info.NotecountsString

    member this.Refresh() = Chart.when_loaded this.OnChartUpdated
