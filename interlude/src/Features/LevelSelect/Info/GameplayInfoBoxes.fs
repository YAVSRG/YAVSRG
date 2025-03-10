namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Data.User
open Interlude.Content
open Interlude.Features.Gameplay

type GameplayInfoBoxes() =
    inherit Container(NodeType.None)

    let PERSONAL_BESTS_WIDTH = 360.0f

    let mutable save_data = None
    let mutable category = "Uncategorised"
    let mutable specific_patterns = ""

    let refresh(info: LoadedChartInfo) =
        save_data <- Some info.SaveData
        category <- info.Patterns.Category
        specific_patterns <- info.Patterns.ImportantClusters |> Seq.truncate 2 |> Seq.map (fun c -> c.Format SelectedChart.rate.Value) |> String.concat ", "

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

            let accuracy_bounds = this.Bounds.SliceR(PERSONAL_BESTS_WIDTH).SlicePercentR(0.5f).ShrinkL(10.0f)
            match accuracy with
            | Some (acc, rate, timestamp, color) ->
                Render.rect accuracy_bounds Colors.shadow_2.O2
                let time_ago = System.DateTimeOffset.UtcNow - Timestamp.to_datetimeoffset timestamp
                Text.fill_b (Style.font, Rulesets.current.FormatAccuracy acc, accuracy_bounds.SliceT(50.0f).Shrink(10.0f, 0.0f), (color, Colors.shadow_2), Alignment.CENTER)
                Text.fill_b (Style.font, sprintf "(%.2fx)  •  %s" rate (format_timespan time_ago), accuracy_bounds.SliceB(30.0f).Shrink(10.0f, 0.0f).Translate(0.0f, -8.0f), (color, Colors.shadow_2), Alignment.CENTER)
            | None -> ()

            let lamp_bounds = this.Bounds.SliceR(PERSONAL_BESTS_WIDTH).SlicePercentL(0.5f).ShrinkL(10.0f)
            match lamp with
            | Some (lamp, rate, timestamp, color) ->
                Render.rect lamp_bounds Colors.shadow_2.O2
                let time_ago = System.DateTimeOffset.UtcNow - Timestamp.to_datetimeoffset timestamp
                Text.fill_b (Style.font, Rulesets.current.LampName lamp, lamp_bounds.SliceT(50.0f).Shrink(10.0f, 0.0f), (color, Colors.shadow_2), Alignment.CENTER)
                Text.fill_b (Style.font, sprintf "(%.2fx)  •  %s" rate (format_timespan time_ago), lamp_bounds.SliceB(30.0f).Shrink(10.0f, 0.0f).Translate(0.0f, -8.0f), (color, Colors.shadow_2), Alignment.CENTER)
            | None -> ()

        | _ ->
            let no_pb_bounds = this.Bounds.SliceR(PERSONAL_BESTS_WIDTH).ShrinkL(10.0f)
            Render.rect no_pb_bounds Colors.shadow_2.O2
            Text.fill_b (Style.font, %"levelselect.no_personal_best", no_pb_bounds.SliceT(50.0f).Shrink(10.0f, 0.0f), Colors.text_greyout, Alignment.CENTER)
            Text.fill_b (Style.font, %"levelselect.no_personal_best.subtitle", no_pb_bounds.SliceB(30.0f).Shrink(10.0f, 0.0f).Translate(0.0f, -8.0f), Colors.text_greyout, Alignment.CENTER)

        let pattern_bounds = this.Bounds.ShrinkR(PERSONAL_BESTS_WIDTH)
        Render.rect pattern_bounds Colors.shadow_2.O2

        Text.fill_b (
            Style.font,
            category,
            pattern_bounds.SliceT(50.0f).Shrink(20.0f, 0.0f),
            Colors.text,
            Alignment.CENTER
        )

        Text.fill_b (
            Style.font,
            specific_patterns,
            pattern_bounds.SliceB(30.0f).Translate(0.0f, -8.0f).Shrink(20.0f, 0.0f),
            Colors.text_subheading,
            Alignment.CENTER
        )