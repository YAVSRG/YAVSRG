namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts.Processing.Difficulty
open Prelude.Gameplay
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Scoring
open Interlude.Content
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
        match save_data with
        | Some save_data when save_data.PersonalBests.ContainsKey Rulesets.current_hash ->

            let pbs = save_data.PersonalBests.[Rulesets.current_hash]

            let accuracy = 
                match pbs.Accuracy |> PersonalBests.get_best_above SelectedChart.rate.Value with
                | Some (acc, rate, timestamp) -> 
                    let grade = Grade.calculate Rulesets.current.Grades acc
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
                Text.fill_b (Style.font, Rulesets.current.FormatAccuracy acc, accuracy_bounds.SliceT(50.0f).Shrink(10.0f, 0.0f), (color, Colors.shadow_2), Alignment.CENTER)
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

type BottomInfo() =
    inherit Container(NodeType.None)

    let mutable rating = 0.0
    let mutable notecounts = ""
    let mutable last_played = "Never played"

    let refresh(info: LoadedChartInfo) =
        rating <- info.Rating.Physical
        notecounts <- info.NotecountsString
        last_played <- 
            if info.SaveData.LastPlayed <= 0L then
                "Never played"
            else
                sprintf "Last played %s ago" (Timestamp.since info.SaveData.LastPlayed |> format_timespan)

    static member HEIGHT = 230.0f

    override this.Init(parent) =
        SelectedChart.on_chart_change_finished.Add refresh
        SelectedChart.on_chart_update_finished.Add refresh
        SelectedChart.if_loaded refresh

        this |* PersonalBests(Position = Position.SliceT(100.0f))

        base.Init parent

    override this.Draw() =
        
        let play_info = this.Bounds.SliceT(40.0f).TranslateY(90.0f).ShrinkX(15.0f)

        Text.fill_b (Style.font, Mods.format (SelectedChart.rate.Value, SelectedChart.selected_mods.Value, SelectedChart.autoplay), play_info, Colors.text, Alignment.LEFT)
        Text.fill_b (Style.font, last_played, play_info, Colors.text, Alignment.RIGHT)

        let chart_info = this.Bounds.SliceT(30.0f).TranslateY(130.0f).ShrinkX(15.0f)
        Text.fill_b (Style.font, (match SelectedChart.CACHE_DATA with Some cc -> cc.DifficultyName | None -> ""), chart_info.SlicePercentL 0.5f, Colors.text_subheading, Alignment.LEFT)
        Text.fill_b (Style.font, notecounts, chart_info, Colors.text_subheading, Alignment.RIGHT)

        let three_icon_infos = this.Bounds.SliceT(70.0f).TranslateY(155.0f).ShrinkX(15.0f)
        Text.fill_b (Style.font, sprintf "%s %.2f" Icons.STAR rating, three_icon_infos, (Colors.white, DifficultyRating.physical_color rating), Alignment.LEFT)
        Text.fill_b (Style.font, SelectedChart.FMT_BPM, three_icon_infos, Colors.text, Alignment.CENTER)
        Text.fill_b (Style.font, SelectedChart.FMT_DURATION, three_icon_infos, Colors.text, Alignment.RIGHT)

        base.Draw()