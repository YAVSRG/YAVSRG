namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts.Processing.Difficulty
open Prelude.Gameplay.Mods
open Prelude.Gameplay
open Interlude.Content
open Interlude.Features.Rulesets
open Interlude.Options
open Interlude.UI
open Interlude.Features.Gameplay

type PersonalBests() =
    inherit Container(NodeType.None)

    let mutable save_data = None
    let mutable patterns = None

    let refresh(info: LoadedChartInfo) =
        save_data <- Some info.SaveData
        patterns <- Some info.Patterns

    override this.Init(parent) =
        SelectedChart.on_chart_change_finished.Add refresh
        SelectedChart.on_chart_update_finished.Add refresh
        SelectedChart.if_loaded refresh
        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.BorderT(Style.PADDING)) (Palette.color (255, 0.8f, 0.0f))
        Draw.rect this.Bounds (!*Palette.DARK_100)
        match save_data with
        | Some save_data when save_data.PersonalBests.ContainsKey Rulesets.current_hash ->

            let pbs = save_data.PersonalBests.[Rulesets.current_hash]

            let accuracy = 
                match pbs.Accuracy |> PersonalBests.get_best_above SelectedChart.rate.Value with
                | Some (acc, rate, timestamp) -> 
                    let grade = (Grade.calculate_with_target Rulesets.current.Grading.Grades acc).Grade
                    let color = Rulesets.current.GradeColor grade
                    Some (acc, rate, timestamp, color)
                | None ->

                match pbs.Accuracy |> PersonalBests.get_best_below SelectedChart.rate.Value with
                | Some (acc, rate, timestamp) -> Some (acc, rate, timestamp, Colors.white.O2)
                | None -> None

            let lamp =
                match pbs.Lamp |> PersonalBests.get_best_above SelectedChart.rate.Value with
                | Some (lamp, rate, timestamp) -> 
                    let color = Rulesets.current.LampColor lamp
                    Some (lamp, rate, timestamp, color)
                | None ->

                match pbs.Lamp |> PersonalBests.get_best_below SelectedChart.rate.Value with
                | Some (lamp, rate, timestamp) -> Some (lamp, rate, timestamp, Colors.white.O2)
                | None -> None

            let accuracy_bounds = this.Bounds.SliceR(180.0f).Shrink(10.0f)
            match accuracy with
            | Some (acc, rate, timestamp, color) ->
                Draw.rect accuracy_bounds Colors.shadow_2.O2
                let time_ago = System.DateTimeOffset.UtcNow - Timestamp.to_datetimeoffset timestamp
                Text.fill_b (Style.font, format_accuracy acc, accuracy_bounds.SliceT(50.0f).Shrink(10.0f, 0.0f), (color, Colors.shadow_2), Alignment.CENTER)
                Text.fill_b (Style.font, sprintf "(%.2fx)  •  %s" rate (format_timespan time_ago), accuracy_bounds.SliceB(30.0f).Shrink(10.0f, 0.0f).Translate(0.0f, -8.0f), (color, Colors.shadow_2), Alignment.CENTER)
            | None -> ()

            let lamp_bounds = accuracy_bounds.Translate(-180.0f, 0.0f)
            match lamp with
            | Some (lamp, rate, timestamp, color) ->
                Draw.rect lamp_bounds Colors.shadow_2.O2
                let time_ago = System.DateTimeOffset.UtcNow - Timestamp.to_datetimeoffset timestamp
                Text.fill_b (Style.font, Rulesets.current.LampName lamp, lamp_bounds.SliceT(50.0f).Shrink(10.0f, 0.0f), (color, Colors.shadow_2), Alignment.CENTER)
                Text.fill_b (Style.font, sprintf "(%.2fx)  •  %s" rate (format_timespan time_ago), lamp_bounds.SliceB(30.0f).Shrink(10.0f, 0.0f).Translate(0.0f, -8.0f), (color, Colors.shadow_2), Alignment.CENTER)
            | None -> ()

        | _ ->
            let no_pb_bounds = this.Bounds.SliceR(360.0f).Shrink(10.0f)
            Draw.rect no_pb_bounds Colors.shadow_2.O2
            Text.fill_b (Style.font, %"levelselect.no_personal_best", no_pb_bounds.SliceT(50.0f).Shrink(10.0f, 0.0f), Colors.text_greyout, Alignment.CENTER)
            Text.fill_b (Style.font, %"levelselect.no_personal_best.subtitle", no_pb_bounds.SliceB(30.0f).Shrink(10.0f, 0.0f).Translate(0.0f, -8.0f), Colors.text_greyout, Alignment.CENTER)

        match patterns with
        | None -> ()
        | Some info ->

        let pattern_bounds = this.Bounds.ShrinkR(360.0f).Shrink(10.0f)
        Draw.rect pattern_bounds Colors.shadow_2.O2
        let category = info.Category

        Text.fill_b (
            Style.font,
            category.Category,
            pattern_bounds.SliceT(50.0f).Shrink(20.0f, 0.0f),
            Colors.text,
            Alignment.CENTER
        )

        Text.fill_b (
            Style.font,
            String.concat ", " category.MajorFeatures,
            pattern_bounds.SliceB(30.0f).Translate(0.0f, -8.0f).Shrink(20.0f, 0.0f),
            Colors.text_subheading,
            Alignment.CENTER
        )

type ChartInfo() as this =
    inherit Container(NodeType.None)

    let display = Setting.simple Display.Local |> Setting.trigger (fun _ -> this.Refresh())

    let scoreboard = Scoreboard(display, Position = Position.ShrinkB 270.0f)
    let online = Leaderboard(display, Position = Position.ShrinkB 270.0f)
    let patterns = Patterns(display, Position = Position.ShrinkB 270.0f)

    let mutable rating = 0.0
    let mutable notecounts = ""

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

        |+ Text(
            (fun () -> sprintf "%s %.2f" Icons.STAR rating),
            Color = (fun () -> Color.White, DifficultyRating.physical_color rating),
            Align = Alignment.LEFT,
            Position =
                {
                    Left = 0.0f %+ 10.0f
                    Top = 1.0f %- 270.0f
                    Right = 0.33f %- 10.0f
                    Bottom = 1.0f %- 200.0f
                }
        )

        |+ Text(
            (fun () -> SelectedChart.FMT_BPM),
            Align = Alignment.CENTER,
            Position =
                {
                    Left = 0.33f %+ 10.0f
                    Top = 1.0f %- 270.0f
                    Right = 0.66f %- 10.0f
                    Bottom = 1.0f %- 200.0f
                }
        )

        |+ Text(
            (fun () -> SelectedChart.FMT_DURATION),
            Align = Alignment.RIGHT,
            Position =
                {
                    Left = 0.66f %+ 10.0f
                    Top = 1.0f %- 270.0f
                    Right = 1.0f %- 10.0f
                    Bottom = 1.0f %- 200.0f
                }
        )

        |+ Text(
            (fun () -> notecounts),
            Align = Alignment.RIGHT,
            Position =
                {
                    Left = 0.0f %+ 10.0f
                    Top = 1.0f %- 200.0f
                    Right = 1.0f %- 17.0f
                    Bottom = 1.0f %- 160.0f
                }
        )

        |+ (Container(
                NodeType.None,
                Position =
                    {
                        Left = 0.0f %+ 10.0f
                        Top = 1.0f %- 200.0f
                        Right = 0.5f %- 10.0f
                        Bottom = 1.0f %- 160.0f
                    }
            )
            |+ Text((fun () -> Mods.format (SelectedChart.rate.Value, SelectedChart.selected_mods.Value, SelectedChart.autoplay)), Align = Alignment.LEFT))
            .Help(
                Help
                    .Info("levelselect.selected_mods")
                    .Hotkey(%"levelselect.selected_mods.mods.hint", "mods")
                    .Body(%"levelselect.selected_mods.rate.hint")
                    .Hotkey(%"levelselect.selected_mods.uprate.hint", "uprate")
                    .Hotkey(%"levelselect.selected_mods.downrate.hint", "downrate")
            )

        |+ PersonalBests(Position = Position.ShrinkB(50.0f).SliceB(100.0f))

        |+ StylishButton(
            (fun () -> SelectedChart.when_loaded <| fun info -> Preview(info, change_rate).Show()),
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
            scoreboard.Refresh,
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
            options.SelectedRuleset |> Setting.trigger (ignore >> LevelSelect.refresh_all),
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
        SelectedChart.on_chart_update_finished.Add (fun info -> rating <- info.Rating.Physical; notecounts <- info.NotecountsString)

        base.Init(parent)

    member this.OnChartUpdated(info: LoadedChartInfo) =
        match display.Value with
        | Display.Local -> scoreboard.OnChartUpdated(info)
        | Display.Online -> online.OnChartUpdated(info)
        | Display.Patterns -> patterns.OnChartUpdated(info)

        rating <- info.Rating.Physical
        notecounts <- info.NotecountsString

    member this.Refresh() = SelectedChart.when_loaded this.OnChartUpdated
