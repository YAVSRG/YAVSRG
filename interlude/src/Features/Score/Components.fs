namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Gameplay
open Prelude.Data.Scores
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.UI.Components
open Interlude.Utils
open Interlude.Features
open Interlude.Features.Stats

type TopBanner(data: ScoreInfoProvider) as this =
    inherit StaticContainer(NodeType.None)

    do
        this
        |+ Text(data.Chart.Header.Artist + " - " + data.Chart.Header.Title,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 20.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %- 20.0f; Bottom = 0.0f %+ 85.0f })
        |+ Text(data.Chart.Header.DiffName,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 20.0f; Top = 0.0f %+ 75.0f; Right = 1.0f %- 20.0f; Bottom = 0.0f %+ 130.0f })
            // todo: bug in multiplayer when looking at a previous score for chart A while on chart B
        |+ Text(sprintf "From %s" (match Gameplay.Chart.cacheInfo with Some c -> c.Folder | None -> "a mysterious source"),
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 20.0f; Top = 0.0f %+ 125.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ 165.0f })

        |+ Text(data.ScoreInfo.time.ToString(),
            Align = Alignment.RIGHT,
            Position = { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 75.0f; Right = 1.0f %- 20.0f; Bottom = 0.0f %+ 130.0f })
        |* Text(
            match data.Player with 
            | Some p -> K (sprintf "Played by %s" p)
            | None -> (fun () -> "Current session: " + Stats.format_short_time Stats.session.GameTime)
            ,
            Align = Alignment.RIGHT,
            Position = { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 125.0f; Right = 1.0f %- 20.0f; Bottom = 0.0f %+ 165.0f })

    override this.Draw() =

        Draw.rect (this.Bounds.TrimBottom 5.0f) (Palette.color(127, 0.5f, 0.0f))
        Draw.rect (this.Bounds.SliceBottom 5.0f) Colors.white.O2

        base.Draw()

type Sidebar(stats: ScoreScreenStats ref, data: ScoreInfoProvider) =
    inherit StaticContainer(NodeType.None)

    override this.Init(parent) =
        this
        |+ Text(sprintf "%s  %iK Results" Icons.stats data.Chart.Keys, Position = Position.SliceTop(120.0f).Margin(10.0f, 0.0f), Align = Alignment.CENTER)
        |+ Text((fun () -> sprintf "%s %s  •  %s" Icons.mods data.Mods data.Ruleset.Name), Position = Position.TrimTop(120.0f).SliceTop(40.0f), Align = Alignment.CENTER)
        |+ Text(sprintf "%s %.2f" Icons.star data.Difficulty.Physical,
            Position = Position.TrimTop(530.0f).SliceTop(70.0f).Margin(10.0f, 0.0f),
            Align = Alignment.LEFT)
        |+ Text((fun () -> sprintf "%ix" data.Scoring.State.BestCombo),
            Position = Position.TrimTop(530.0f).SliceTop(70.0f).Margin(10.0f, 0.0f),
            Align = Alignment.CENTER)
        |+ Text(sprintf "%.2f" data.Physical, 
            Position = Position.TrimTop(530.0f).SliceTop(70.0f).Margin(10.0f, 0.0f),
            Align = Alignment.RIGHT)
        |* Text((fun () -> sprintf "M: %.1fms | SD: %.1fms" (!stats).Mean (!stats).StandardDeviation), 
            Position = Position.TrimTop(600.0f).SliceTop(40.0f).Margin(10.0f, 0.0f),
            Align = Alignment.RIGHT)
        base.Init(parent)

    override this.Draw() =
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds.SliceTop(120.0f), !*Palette.DARKER, 2.0f)
        Background.draw (this.Bounds.TrimTop(120.0f).SliceTop(40.0f), !*Palette.DARK, 2.0f)
        Background.draw (this.Bounds.TrimTop(160.0f), (Color.FromArgb(40, 40, 40)), 2.0f)
        base.Draw()

        // accuracy info
        let counters = Rect.Box(this.Bounds.Left + 10.0f, this.Bounds.Top + 160.0f + 10.0f, this.Bounds.Width - 20.0f, 350.0f)

        let judgeCounts = data.Scoring.State.Judgements
        let judgements = data.Ruleset.Judgements |> Array.indexed
        let h = counters.Height / float32 judgements.Length
        let mutable y = counters.Top
        for i, j in judgements do
            let b = Rect.Create(counters.Left, y, counters.Right, y + h)
            Draw.rect b (Color.FromArgb(40, j.Color))
            Draw.rect (b.SliceLeft(counters.Width * (float32 judgeCounts.[i] / float32 (!stats).JudgementCount))) (Color.FromArgb(127, j.Color))
            Text.drawFillB(Style.font, sprintf "%s: %i" j.Name judgeCounts.[i], b.Shrink(5.0f, 2.0f), Colors.text, Alignment.LEFT)
            y <- y + h

        
type Grade(grade: Grade.GradeResult ref, data: ScoreInfoProvider) =
    inherit StaticContainer(NodeType.None)

    override this.Init(parent) =
        this
        |* Text((fun () -> data.Ruleset.GradeName (!grade).Grade),
            Color = (fun () -> (data.Ruleset.GradeColor (!grade).Grade, Colors.black)),
            Position = Position.Margin(-10.0f))
        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        let grade_color = data.Ruleset.GradeColor (!grade).Grade
        Draw.rect this.Bounds grade_color.O1
        base.Draw()

type Accuracy(grade: Grade.GradeResult ref, improvements: ImprovementFlags ref, previous_personal_bests: Bests option ref, data: ScoreInfoProvider) =
    inherit StaticContainer(NodeType.None)

    let LOWER_SIZE = 40.0f
    let new_record = sprintf "%s %s" Icons.sparkle (L"score.new_record")

    override this.Init(parent) =
        this
        |* Text((fun () -> data.Scoring.FormatAccuracy()),
            Color = (fun () -> (data.Ruleset.GradeColor (!grade).Grade, Colors.black)),
            Position = Position.Margin(10.0f, 0.0f).TrimBottom(LOWER_SIZE))
        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        let grade_color = data.Ruleset.GradeColor (!grade).Grade
        Draw.rect (this.Bounds.TrimBottom(LOWER_SIZE)) grade_color.O1
        Draw.rect (this.Bounds.SliceBottom(LOWER_SIZE)) grade_color.O2
        Text.drawFillB(Style.font, data.Scoring.FormatAccuracy(), this.Bounds.Shrink(10.0f, 0.0f).TrimBottom(LOWER_SIZE), (grade_color, Colors.black), Alignment.CENTER)
        let text, color =
            match (!improvements).Accuracy with
            | Improvement.New -> new_record, Colors.text_yellow_2
            | Improvement.Faster r -> sprintf "%s  •  +%gx" new_record (System.MathF.Round(r, 2)), Colors.text_cyan_2
            | Improvement.Better b -> sprintf "%s  •  +%.2f%%" new_record (b * 100.0), Colors.text_green_2
            | Improvement.FasterBetter (r, b) ->  sprintf "%s  •  +%.2f%%  •  +%gx" new_record (b * 100.0) (System.MathF.Round(r, 2)), Colors.text_pink_2
            | Improvement.None ->
                match (!previous_personal_bests) with
                | Some pbs ->
                    match PersonalBests.get_best_above_with_rate data.ScoreInfo.rate pbs.Accuracy with
                    | Some (v, r) ->

                        let summary, distance_from_pb = 
                            if r > data.ScoreInfo.rate then sprintf "%.2f%% (%.2fx)" (v * 100.0) r, (v - data.Scoring.Value)
                            else sprintf "%.2f%%" (v * 100.0), (v - data.Scoring.Value)

                        if distance_from_pb < 0.0001 then sprintf "Your record: %s" summary, (Colors.grey_2.O2, Colors.black)
                        else sprintf "%.2f%% from record: %s" (distance_from_pb * 100.0) summary, (Colors.grey_2.O2, Colors.black)

                    | None -> "--", (Colors.grey_2.O2, Colors.black)
                | None -> "--", (Colors.grey_2.O2, Colors.black)
        Text.drawFillB(Style.font, text, this.Bounds.Shrink(10.0f, 0.0f).SliceBottom(LOWER_SIZE), color, Alignment.CENTER)
        base.Draw()

type Lamp(lamp: Lamp.LampResult ref, improvements: ImprovementFlags ref, previous_personal_bests: Bests option ref, data: ScoreInfoProvider) =
    inherit StaticContainer(NodeType.None)

    let LOWER_SIZE = 40.0f
    let new_record = sprintf "%s %s" Icons.sparkle (L"score.new_record")

    override this.Init(parent) =
        this
        |* Text((fun () -> data.Ruleset.LampName (!lamp).Lamp),
            Color = (fun () -> (data.Ruleset.LampColor (!lamp).Lamp, Colors.black)),
            Position = Position.Margin(10.0f, 0.0f).TrimBottom(LOWER_SIZE))
        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        Draw.rect (this.Bounds.TrimBottom(LOWER_SIZE)) (data.Ruleset.LampColor (!lamp).Lamp).O1
        Draw.rect (this.Bounds.SliceBottom(LOWER_SIZE)) (data.Ruleset.LampColor (!lamp).Lamp).O2
        let text, color =
            match (!improvements).Lamp with
            | Improvement.New -> new_record, (Colors.text_yellow_2)
            | Improvement.Faster r -> sprintf "%s  •  +%gx" new_record (System.MathF.Round(r, 2)), (Colors.text_cyan_2)
            | Improvement.Better b -> 
                let new_lamp = data.Ruleset.LampName (!lamp).Lamp
                let old_lamp = data.Ruleset.LampName( (!lamp).Lamp - b )
                sprintf "%s  •  %s > %s" new_record old_lamp new_lamp, (Colors.text_green_2)
            | Improvement.FasterBetter (r, b) -> 
                let new_lamp = data.Ruleset.LampName (!lamp).Lamp
                let old_lamp = data.Ruleset.LampName( (!lamp).Lamp - b )
                sprintf "%s  •  %s > %s  •  +%gx" new_record old_lamp new_lamp (System.MathF.Round(r, 2)), (Colors.text_pink_2)
            | Improvement.None ->
                match (!previous_personal_bests) with
                | Some pbs ->
                    match PersonalBests.get_best_above_with_rate data.ScoreInfo.rate pbs.Lamp with
                    | Some (v, r) ->

                        let summary = 
                            if r > data.ScoreInfo.rate then sprintf "%s (%.2fx)" (data.Ruleset.LampName v) r
                            else data.Ruleset.LampName v

                        sprintf "Your record: %s" summary, (Colors.grey_2.O2, Colors.black)

                    | None -> "--", (Colors.grey_2.O2, Colors.black)
                | None -> "--", (Colors.grey_2.O2, Colors.black)
        Text.drawFillB(Style.font, text, this.Bounds.Shrink(10.0f, 0.0f).SliceBottom(LOWER_SIZE), color, Alignment.CENTER)
        base.Draw()
        
type Results(grade, lamp, improvements, previous_personal_bests, scoreData) =
    inherit StaticContainer(NodeType.None)

    override this.Init(parent) =
        StaticContainer(NodeType.None, Position = { Position.Default with Left = 0.35f %+ 0.0f })
        |+ Grade(grade, scoreData, 
            Position = Position.Box(0.0f, 0.0f, 40.0f, 40.0f, 160.0f, 160.0f))
        |+ Accuracy(grade, improvements, previous_personal_bests, scoreData,
            Position = {
                Left = 0.0f %+ 200.0f ^+ 40.0f
                Right = 0.5f %+ 100.0f ^- 20.0f
                Top = 0.0f %+ 40.0f
                Bottom = 0.0f %+ 200.0f
            })
        |+ Lamp(lamp, improvements, previous_personal_bests, scoreData,
            Position = {
                Left = 0.5f %+ 100.0f ^+ 20.0f
                Right = 1.0f %- 40.0f
                Top = 0.0f %+ 40.0f
                Bottom = 0.0f %+ 200.0f
            })
        |> this.Add
        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.SliceTop(160.0f).TrimTop(5.0f)) Colors.shadow_2.O2
        Draw.rect (this.Bounds.TrimTop(160.0f).SliceTop(5.0f)) Colors.white
        base.Draw()

type BottomBanner(stats: ScoreScreenStats ref, data: ScoreInfoProvider, graph: ScoreGraph, refresh: unit -> unit) as this =
    inherit StaticContainer(NodeType.None)
    
    do
        graph.Position <- { Left = 0.35f %+ 30.0f; Top = 0.0f %+ 25.0f; Right = 1.0f %- 20.0f; Bottom = 1.0f %- 65.0f }
        this
        |+ graph
        |+ Text(version, Position = Position.SliceBottom(50.0f).Margin(20.0f, 5.0f), Color = K Colors.text_subheading, Align = Alignment.LEFT)
        |+ StylishButton(
            (fun () -> { new ScoreGraphSettingsPage() with override this.OnClose() = graph.Refresh() }.Show()),
            sprintf "%s %s" Icons.edit (L"score.graph.settings") |> K,
            !%Palette.MAIN_100,
            Position = { Left = 0.55f %+ 0.0f; Top = 1.0f %- 50.0f; Right = 0.7f %- 25.0f; Bottom = 1.0f %- 0.0f })
        |+ StylishButton(
            (fun () -> ScoreScreenHelpers.watchReplay (data.ModChart, data.ScoreInfo.rate, data.ReplayData)),
            sprintf "%s %s" Icons.watch (L"score.watch_replay.name") |> K,
            !%Palette.DARK_100,
            Position = { Left = 0.7f %+ 0.0f; Top = 1.0f %- 50.0f; Right = 0.85f %- 25.0f; Bottom = 1.0f %- 0.0f })
        |* Rulesets.QuickSwitcher(
            options.SelectedRuleset
            |> Setting.trigger (fun _ -> data.Ruleset <- Rulesets.current; refresh()),
            Position = { Left = 0.85f %+ 0.0f; Top = 1.0f %- 50.0f; Right = 1.0f %- 0.0f; Bottom = 1.0f %- 0.0f })

    override this.Draw() =

        Draw.rect (this.Bounds.TrimTop 5.0f) (Palette.color(127, 0.5f, 0.0f))
        Draw.rect (this.Bounds.SliceTop 5.0f) Colors.white.O2

        // graph background
        Draw.rect (graph.Bounds.Expand(5.0f, 5.0f)) Color.White
        Background.draw (graph.Bounds, Color.FromArgb(127, 127, 127), 1.0f)

        base.Draw()