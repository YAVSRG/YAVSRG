namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude.Common
open Prelude.Charts.Processing.Patterns
open Prelude.Gameplay
open Prelude.Data
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.UI.Components
open Interlude.Utils
open Interlude.Features
open Interlude.Features.Stats

#nowarn "3370"

type TopBanner(score_info: ScoreInfo) as this =
    inherit StaticContainer(NodeType.None)

    do
        this
        |+ Text(
            score_info.CachedChart.Artist + " - " + score_info.CachedChart.Title,
            Align = Alignment.LEFT,
            Position =
                {
                    Left = 0.0f %+ 20.0f
                    Top = 0.0f %+ 0.0f
                    Right = 1.0f %- 20.0f
                    Bottom = 0.0f %+ 85.0f
                }
        )
        |+ Text(
            score_info.CachedChart.DifficultyName,
            Align = Alignment.LEFT,
            Position =
                {
                    Left = 0.0f %+ 20.0f
                    Top = 0.0f %+ 75.0f
                    Right = 1.0f %- 20.0f
                    Bottom = 0.0f %+ 130.0f
                }
        )
        |+ Text(
            sprintf "From %s" score_info.CachedChart.Folder,
            Align = Alignment.LEFT,
            Position =
                {
                    Left = 0.0f %+ 20.0f
                    Top = 0.0f %+ 125.0f
                    Right = 1.0f %+ 0.0f
                    Bottom = 0.0f %+ 165.0f
                }
        )

        |+ Text(
            (score_info.TimePlayed |> Timestamp.to_datetime).ToLocalTime().ToString(),
            Align = Alignment.RIGHT,
            Position =
                {
                    Left = 0.0f %+ 0.0f
                    Top = 0.0f %+ 75.0f
                    Right = 1.0f %- 20.0f
                    Bottom = 0.0f %+ 130.0f
                }
        )
        |* Text(
            match score_info.PlayedBy with
            | ScorePlayedBy.Username p -> K(sprintf "Played by %s" p)
            | ScorePlayedBy.You -> (fun () -> "Current session: " + Stats.format_short_time Stats.session.GameTime)
            , Align = Alignment.RIGHT
            , Position =
                {
                    Left = 0.0f %+ 0.0f
                    Top = 0.0f %+ 125.0f
                    Right = 1.0f %- 20.0f
                    Bottom = 0.0f %+ 165.0f
                }
        )

    override this.Draw() =

        Draw.rect (this.Bounds.TrimBottom 5.0f) (Palette.color (127, 0.5f, 0.0f))
        Draw.rect (this.Bounds.SliceBottom 5.0f) Colors.white.O2

        base.Draw()

type Sidebar(stats: ScoreScreenStats ref, score_info: ScoreInfo) =
    inherit StaticContainer(NodeType.None)

    let mod_string = Mods.format_mods (score_info.Rate, score_info.Mods, false)

    let category, main_elements, minor_elements =
        let c =  score_info.Patterns.Category
        c.Category,
        String.concat ", " c.MajorFeatures |> (function "" -> "--" | x -> x),
        String.concat ", " (List.truncate 3 c.MinorFeatures) |> (function "" -> "--" | x -> x)

    override this.Init(parent) =
        this
        |+ Text(
            sprintf "%s  %iK Results" Icons.BAR_CHART score_info.Chart.Keys,
            Position = Position.SliceTop(90.0f).Margin(10.0f, 0.0f),
            Align = Alignment.CENTER
        )
        |+ Text(
            (fun () -> sprintf "%s %s  •  %s" Icons.ZAP mod_string score_info.Ruleset.Name),
            Position = Position.TrimTop(90.0f).SliceTop(70.0f),
            Align = Alignment.CENTER
        )
        |+ Text(
            sprintf "%s %.2f" Icons.STAR score_info.Rating.Physical,
            Position = Position.TrimTop(530.0f).SliceTop(70.0f).Margin(10.0f, 0.0f),
            Align = Alignment.LEFT
        )
        |+ Text(
            (fun () -> sprintf "%ix" score_info.Scoring.State.BestCombo),
            Position = Position.TrimTop(530.0f).SliceTop(70.0f).Margin(10.0f, 0.0f),
            Align = Alignment.CENTER
        )
        |+ Text(
            sprintf "%.2f" score_info.Physical,
            Position = Position.TrimTop(530.0f).SliceTop(70.0f).Margin(10.0f, 0.0f),
            Align = Alignment.RIGHT
        )
        // todo: update this on ruleset change
        |+ Tooltip(
            Callout.Normal
                .Icon(Icons.TRENDING_UP)
                .Title("Taps")
                .Body(
                    sprintf
                        "M: %.1fms | SD: %.1fms | %.1f%% early"
                        (!stats).TapMean
                        (!stats).TapStandardDeviation
                        (100.0 * (!stats).TapEarlyPercent)
                )
                .Title("Releases")
                .Body(
                    sprintf
                        "M: %.1fms | SD: %.1fms | %.1f%% early"
                        (!stats).ReleaseMean
                        (!stats).ReleaseStandardDeviation
                        (100.0 * (!stats).ReleaseEarlyPercent)
                ),
            Position = Position.TrimTop(600.0f).SliceTop(40.0f).Margin(10.0f, 0.0f)
        )
        |* Text(
            (fun () -> sprintf "M: %.1fms | SD: %.1fms" (!stats).TapMean (!stats).TapStandardDeviation),
            Position = Position.TrimTop(600.0f).SliceTop(40.0f).Margin(10.0f, 0.0f),
            Align = Alignment.RIGHT
        )

        this
        |+ Text(
            category,
            Position = Position.TrimBottom(95.0f).SliceBottom(60.0f).Margin(20.0f, 0.0f),
            Align = Alignment.LEFT
        )
        |+ Text(
            main_elements,
            Position = Position.TrimBottom(60.0f).SliceBottom(40.0f).Margin(20.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |* Text(
            minor_elements,
            Position = Position.TrimBottom(30.0f).SliceBottom(30.0f).Margin(20.0f, 0.0f),
            Color = K Colors.text_greyout,
            Align = Alignment.LEFT
        )

        base.Init(parent)

    override this.Draw() =
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds.SliceTop(160.0f), !*Palette.DARKER, 2.0f)
        Background.draw (this.Bounds.TrimTop(160.0f), (Color.FromArgb(40, 40, 40)), 2.0f)
        base.Draw()

        // accuracy info
        let counters =
            Rect.Box(this.Bounds.Left + 10.0f, this.Bounds.Top + 160.0f + 10.0f, this.Bounds.Width - 20.0f, 350.0f)

        let judgement_counts = score_info.Scoring.State.Judgements
        let judgements = score_info.Ruleset.Judgements |> Array.indexed
        let h = counters.Height / float32 judgements.Length
        let mutable y = counters.Top

        for i, j in judgements do
            let b = Rect.Create(counters.Left, y, counters.Right, y + h)
            Draw.rect b (Color.FromArgb(40, j.Color))

            Draw.rect
                (b.SliceLeft(
                    counters.Width
                    * (float32 judgement_counts.[i] / float32 (!stats).JudgementCount)
                ))
                (Color.FromArgb(127, j.Color))

            Text.fill_b (
                Style.font,
                sprintf "%s: %i" j.Name judgement_counts.[i],
                b.Shrink(5.0f, 2.0f),
                Colors.text,
                Alignment.LEFT
            )

            y <- y + h


type Grade(grade: Grade.GradeResult ref, score_info: ScoreInfo) =
    inherit StaticContainer(NodeType.None)

    override this.Init(parent) =
        this
        |* Text(
            (fun () -> score_info.Ruleset.GradeName (!grade).Grade),
            Color = (fun () -> (score_info.Ruleset.GradeColor (!grade).Grade, Colors.black)),
            Position = Position.Margin(-10.0f)
        )

        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        let grade_color = score_info.Ruleset.GradeColor (!grade).Grade
        Draw.rect this.Bounds grade_color.O1
        base.Draw()

type Accuracy
    (
        grade: Grade.GradeResult ref,
        improvements: ImprovementFlags ref,
        previous_personal_bests: Bests option ref,
        score_info: ScoreInfo
    ) =
    inherit StaticContainer(NodeType.None)

    let LOWER_SIZE = 40.0f
    let new_record = sprintf "%s %s" Icons.AWARD (%"score.new_record")
    let mutable hover = false

    override this.Init(parent) =
        this
        |* Text(
            (fun () -> score_info.Scoring.FormatAccuracy()),
            Color = (fun () -> (score_info.Ruleset.GradeColor (!grade).Grade, Colors.black)),
            Position = Position.Margin(10.0f, 0.0f).TrimBottom(LOWER_SIZE)
        )

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        hover <- Mouse.hover this.Bounds
        base.Update(elapsed_ms, moved)

    override this.Draw() =
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        let grade_color = score_info.Ruleset.GradeColor (!grade).Grade
        Draw.rect (this.Bounds.TrimBottom(LOWER_SIZE)) grade_color.O1
        Draw.rect (this.Bounds.SliceBottom(LOWER_SIZE)) grade_color.O2

        Text.fill_b (
            Style.font,
            score_info.Scoring.FormatAccuracy(),
            this.Bounds.Shrink(10.0f, 0.0f).TrimBottom(LOWER_SIZE),
            (grade_color, Colors.black),
            Alignment.CENTER
        )

        let text, color =
            match (!improvements).Accuracy with
            | Improvement.New -> new_record, Colors.text_yellow_2
            | Improvement.Faster r -> sprintf "%s  •  +%gx" new_record (System.MathF.Round(r, 2)), Colors.text_cyan_2
            | Improvement.Better b -> sprintf "%s  •  +%.2f%%" new_record (b * 100.0), Colors.text_green_2
            | Improvement.FasterBetter(r, b) ->
                sprintf "%s  •  +%.2f%%  •  +%gx" new_record (b * 100.0) (System.MathF.Round(r, 2)), Colors.text_pink_2
            | Improvement.None ->
                match (!previous_personal_bests) with
                | Some pbs ->
                    match PersonalBests.get_best_above_with_rate score_info.Rate pbs.Accuracy with
                    | Some(v, r) ->

                        let summary, distance_from_pb =
                            if r > score_info.Rate then
                                sprintf "%.2f%% (%.2fx)" (v * 100.0) r, (v - score_info.Scoring.Value)
                            else
                                sprintf "%.2f%%" (v * 100.0), (v - score_info.Scoring.Value)

                        if distance_from_pb < 0.0001 then
                            sprintf "Your record: %s" summary, (Colors.grey_2.O2, Colors.black)
                        else
                            sprintf "%.2f%% from record: %s" (distance_from_pb * 100.0) summary,
                            (Colors.grey_2.O2, Colors.black)

                    | None -> "--", (Colors.grey_2.O2, Colors.black)
                | None -> "--", (Colors.grey_2.O2, Colors.black)

        Text.fill_b (Style.font, text, this.Bounds.Shrink(10.0f, 0.0f).SliceBottom(LOWER_SIZE), color, Alignment.CENTER)
        base.Draw()

        if hover then
            let acc_tooltip = this.Bounds.Expand(-130.0f, 75.0f).SliceBottom(60.0f)
            Draw.rect acc_tooltip Colors.shadow_2.O2

            Text.fill_b (
                Style.font,
                sprintf "%.4f%%" (score_info.Scoring.Value * 100.0),
                acc_tooltip.Shrink(10.0f, 5.0f),
                Colors.text,
                Alignment.CENTER
            )

type Lamp
    (
        lamp: Lamp.LampResult ref,
        improvements: ImprovementFlags ref,
        previous_personal_bests: Bests option ref,
        score_info: ScoreInfo
    ) =
    inherit StaticContainer(NodeType.None)

    let LOWER_SIZE = 40.0f
    let new_record = sprintf "%s %s" Icons.AWARD (%"score.new_record")

    override this.Init(parent) =
        this
        |* Text(
            (fun () -> score_info.Ruleset.LampName (!lamp).Lamp),
            Color = (fun () -> (score_info.Ruleset.LampColor (!lamp).Lamp, Colors.black)),
            Position = Position.Margin(10.0f, 0.0f).TrimBottom(LOWER_SIZE)
        )

        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        Draw.rect (this.Bounds.TrimBottom(LOWER_SIZE)) (score_info.Ruleset.LampColor (!lamp).Lamp).O1
        Draw.rect (this.Bounds.SliceBottom(LOWER_SIZE)) (score_info.Ruleset.LampColor (!lamp).Lamp).O2

        let text, color =
            match (!improvements).Lamp with
            | Improvement.New -> new_record, (Colors.text_yellow_2)
            | Improvement.Faster r -> sprintf "%s  •  +%gx" new_record (System.MathF.Round(r, 2)), (Colors.text_cyan_2)
            | Improvement.Better b ->
                let new_lamp = score_info.Ruleset.LampName (!lamp).Lamp
                let old_lamp = score_info.Ruleset.LampName((!lamp).Lamp - b)
                sprintf "%s  •  %s > %s" new_record old_lamp new_lamp, (Colors.text_green_2)
            | Improvement.FasterBetter(r, b) ->
                let new_lamp = score_info.Ruleset.LampName (!lamp).Lamp
                let old_lamp = score_info.Ruleset.LampName((!lamp).Lamp - b)

                sprintf "%s  •  %s > %s  •  +%gx" new_record old_lamp new_lamp (System.MathF.Round(r, 2)),
                (Colors.text_pink_2)
            | Improvement.None ->
                match (!previous_personal_bests) with
                | Some pbs ->
                    match PersonalBests.get_best_above_with_rate score_info.Rate pbs.Lamp with
                    | Some(v, r) ->

                        let summary =
                            if r > score_info.Rate then
                                sprintf "%s (%.2fx)" (score_info.Ruleset.LampName v) r
                            else
                                score_info.Ruleset.LampName v

                        sprintf "Your record: %s" summary, (Colors.grey_2.O2, Colors.black)

                    | None -> "--", (Colors.grey_2.O2, Colors.black)
                | None -> "--", (Colors.grey_2.O2, Colors.black)

        Text.fill_b (Style.font, text, this.Bounds.Shrink(10.0f, 0.0f).SliceBottom(LOWER_SIZE), color, Alignment.CENTER)
        base.Draw()

type Results(grade, lamp, improvements, previous_personal_bests, score_info) =
    inherit StaticContainer(NodeType.None)

    override this.Init(parent) =
        StaticContainer(
            NodeType.None,
            Position =
                { Position.Default with
                    Left = 0.35f %+ 0.0f
                }
        )
        |+ Grade(grade, score_info, Position = Position.Box(0.0f, 0.0f, 40.0f, 40.0f, 160.0f, 160.0f))
        |+ Accuracy(
            grade,
            improvements,
            previous_personal_bests,
            score_info,
            Position =
                {
                    Left = 0.0f %+ 200.0f ^+ 40.0f
                    Right = 0.5f %+ 100.0f ^- 20.0f
                    Top = 0.0f %+ 40.0f
                    Bottom = 0.0f %+ 200.0f
                }
        )
        |+ Lamp(
            lamp,
            improvements,
            previous_personal_bests,
            score_info,
            Position =
                {
                    Left = 0.5f %+ 100.0f ^+ 20.0f
                    Right = 1.0f %- 40.0f
                    Top = 0.0f %+ 40.0f
                    Bottom = 0.0f %+ 200.0f
                }
        )
        |> this.Add

        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.SliceTop(160.0f).TrimTop(5.0f)) Colors.shadow_2.O2
        Draw.rect (this.Bounds.TrimTop(160.0f).SliceTop(5.0f)) Colors.white
        base.Draw()

open Prelude.Data.Library.Caching
open Interlude.Features.Collections
open Interlude.Features.Tables
open Interlude.Features.Online

type ScoreChartContextMenu(cc: CachedChart) as this =
    inherit Page()

    do
        let content =
            FlowContainer.Vertical(PRETTYHEIGHT, Position = Position.Margin(100.0f, 200.0f))

            |+ PageButton(
                "chart.add_to_collection",
                (fun () -> AddToCollectionPage(cc).Show()),
                Icon = Icons.FOLDER_PLUS
            )

        match Content.Table with
        | Some table ->
            if Network.status = Network.Status.LoggedIn && cc.Keys = table.Info.Keymode then
                content
                |* PageButton(
                    "chart.suggest_for_table",
                    (fun () -> SuggestChartPage(table, cc).Show()),
                    Icon = Icons.SIDEBAR
                )
        | _ -> ()

        this.Content content

    override this.Title = cc.Title
    override this.OnClose() = ()

type BottomBanner(stats: ScoreScreenStats ref, score_info: ScoreInfo, graph: ScoreGraph, refresh: unit -> unit) as this
    =
    inherit StaticContainer(NodeType.None)

    do
        graph.Position <-
            {
                Left = 0.35f %+ 30.0f
                Top = 0.0f %+ 25.0f
                Right = 1.0f %- 20.0f
                Bottom = 1.0f %- 65.0f
            }

        this
        |+ graph
        |+ Text(
            version + "  : :  www.yavsrg.net",
            Position = Position.SliceBottom(50.0f).Margin(20.0f, 5.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |+ StylishButton(
            (fun () -> ScoreChartContextMenu(score_info.CachedChart).Show()),
            sprintf "%s %s" Icons.SETTINGS (%"score.chart_actions") |> K,
            !%Palette.DARK_100,
            Hotkey = "context_menu",
            Position =
                {
                    Left = 0.4f %+ 0.0f
                    Top = 1.0f %- 50.0f
                    Right = 0.55f %- 25.0f
                    Bottom = 1.0f %- 0.0f
                }
        )
        |+ StylishButton(
            (fun () ->
                { new ScoreGraphSettingsPage() with
                    override this.OnClose() = graph.Refresh()
                }
                    .Show()
            ),
            sprintf "%s %s" Icons.EDIT_2 (%"score.graph.settings") |> K,
            !%Palette.MAIN_100,
            Position =
                {
                    Left = 0.55f %+ 0.0f
                    Top = 1.0f %- 50.0f
                    Right = 0.7f %- 25.0f
                    Bottom = 1.0f %- 0.0f
                }
        )
        |+ StylishButton(
            (fun () ->
                ScoreScreenHelpers.watch_replay (score_info, Gameplay.Chart.color_this_chart (score_info.WithMods))
            ),
            sprintf "%s %s" Icons.FILM (%"score.watch_replay.name") |> K,
            !%Palette.DARK_100,
            Position =
                {
                    Left = 0.7f %+ 0.0f
                    Top = 1.0f %- 50.0f
                    Right = 0.85f %- 25.0f
                    Bottom = 1.0f %- 0.0f
                }
        )
        |* Rulesets.QuickSwitcher(
            options.SelectedRuleset
            |> Setting.trigger (fun _ ->
                score_info.Ruleset <- Rulesets.current
                refresh ()
            ),
            Position =
                {
                    Left = 0.85f %+ 0.0f
                    Top = 1.0f %- 50.0f
                    Right = 1.0f %- 0.0f
                    Bottom = 1.0f %- 0.0f
                }
        )

    override this.Draw() =

        Draw.rect (this.Bounds.TrimTop 5.0f) (Palette.color (127, 0.5f, 0.0f))
        Draw.rect (this.Bounds.SliceTop 5.0f) Colors.white.O2

        // graph background
        Draw.rect (graph.Bounds.Expand(5.0f, 5.0f)) Color.White
        Background.draw (graph.Bounds, Color.FromArgb(127, 127, 127), 1.0f)

        base.Draw()
