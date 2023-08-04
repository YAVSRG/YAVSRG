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

        Draw.rect (this.Bounds.TrimBottom 5.0f) (Style.color(127, 0.5f, 0.0f))
        Draw.rect (this.Bounds.SliceBottom 5.0f) (Color.FromArgb(127, Color.White))

        base.Draw()

type Sidebar(stats: ScoreScreenStats ref, data: ScoreInfoProvider) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        Draw.rect (this.Bounds.Expand(5.0f, -10.0f)) Colors.white.O2
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)

        let title = this.Bounds.SliceTop(100.0f).Shrink(5.0f, 20.0f)
        Draw.rect title Colors.shadow_2.O2
        Text.drawFillB(Style.baseFont, sprintf "%iK Results  •  %s" data.Chart.Keys data.Ruleset.Name, title, Colors.text, Alignment.CENTER)
        let mods = title.Translate(0.0f, 70.0f)
        Draw.rect mods Colors.shadow_2.O2
        Text.drawFillB(Style.baseFont, data.Mods, mods, Colors.text, Alignment.CENTER)

        // accuracy info
        let counters = Rect.Box(this.Bounds.Left + 5.0f, this.Bounds.Top + 160.0f, this.Bounds.Width - 10.0f, 350.0f)
        Draw.rect counters (Color.FromArgb(127, Color.Black))

        let judgeCounts = data.Scoring.State.Judgements
        let judgements = data.Ruleset.Judgements |> Array.indexed
        let h = (counters.Height - 20.0f) / float32 judgements.Length
        let mutable y = counters.Top + 10.0f
        for i, j in judgements do
            let b = Rect.Create(counters.Left + 10.0f, y, counters.Right - 10.0f, y + h)
            Draw.rect b (Color.FromArgb(40, j.Color))
            Draw.rect (b.SliceLeft((counters.Width - 20.0f) * (float32 judgeCounts.[i] / float32 (!stats).JudgementCount))) (Color.FromArgb(127, j.Color))
            Text.drawFillB(Style.baseFont, sprintf "%s: %i" j.Name judgeCounts.[i], b.Shrink(5.0f, 2.0f), Colors.text, 0.0f)
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
        Text.drawFillB(Style.baseFont, data.Scoring.FormatAccuracy(), this.Bounds.Shrink(10.0f, 0.0f).TrimBottom(LOWER_SIZE), (grade_color, Colors.black), Alignment.CENTER)
        let text, color =
            match (!improvements).Accuracy with
            | Improvement.New -> new_record, Colors.text_yellow_2
            | Improvement.Faster r -> sprintf "%s  •  +%gx" new_record (System.MathF.Round(r, 2)), Colors.text_cyan_2
            | Improvement.Better b -> sprintf "%s  •  +%.2f%%" new_record (b * 100.0), Colors.text_green_2
            | Improvement.FasterBetter (r, b) ->  sprintf "%s  •  +%.2f%%  •  +%gx" new_record (b * 100.0) (System.MathF.Round(r, 2)), Colors.text_pink_2
            | Improvement.None ->
                match (!previous_personal_bests) with
                | Some pbs -> 
                    let rb, r = pbs.Accuracy.Fastest
                    let summary, distance_from_pb = 
                        if r < data.ScoreInfo.rate then sprintf "%.2f%% (%.2fx)" (rb * 100.0) r, (rb - data.Scoring.Value)
                        elif r = data.ScoreInfo.rate then sprintf "%.2f%%" (rb * 100.0), (rb - data.Scoring.Value)
                        else 
                            let rb, r = pbs.Accuracy.Best
                            if r <> data.ScoreInfo.rate then sprintf "%.2f%% (%.2fx)" (rb * 100.0) r, (rb - data.Scoring.Value)
                            else sprintf "%.2f%%" (rb * 100.0), (rb - data.Scoring.Value)
                    if distance_from_pb < 0.001 then
                        sprintf "Your record: %s" summary, (Colors.grey_2.O2, Colors.black)
                    else
                        sprintf "%.2f%% from record: %s" (distance_from_pb * 100.0) summary, (Colors.grey_2.O2, Colors.black)
                | None -> "--", (Colors.grey_2.O2, Colors.black)
        Text.drawFillB(Style.baseFont, text, this.Bounds.Shrink(10.0f, 0.0f).SliceBottom(LOWER_SIZE), color, Alignment.CENTER)
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
                    let rb, r = pbs.Lamp.Fastest
                    let summary = 
                        if r < data.ScoreInfo.rate then sprintf "%s (%.2fx)" (data.Ruleset.LampName rb) r
                        elif r = data.ScoreInfo.rate then data.Ruleset.LampName rb
                        else 
                            let rb, r = pbs.Lamp.Best
                            if r <> data.ScoreInfo.rate then sprintf "%s (%.2fx)" (data.Ruleset.LampName rb) r
                            else data.Ruleset.LampName rb
                    sprintf "Your record: %s" summary, (Colors.grey_2.O2, Colors.black)
                | None -> "--", (Colors.grey_2.O2, Colors.black)
        Text.drawFillB(Style.baseFont, text, this.Bounds.Shrink(10.0f, 0.0f).SliceBottom(LOWER_SIZE), color, Alignment.CENTER)
        base.Draw()
        
type Results(grade, lamp, improvements, previous_personal_bests, scoreData) =
    inherit StaticContainer(NodeType.None)

    override this.Init(parent) =
        this
        |+ Grade(grade, scoreData, 
            Position = Position.Box(0.0f, 0.0f, 40.0f, 40.0f, 160.0f, 160.0f))
        |+ Accuracy(grade, improvements, previous_personal_bests, scoreData,
            Position = {
                Left = 0.0f %+ 200.0f ^+ 40.0f
                Right = 0.5f %+ 100.0f ^- 20.0f
                Top = 0.0f %+ 40.0f
                Bottom = 0.0f %+ 200.0f
            })
        |* Lamp(lamp, improvements, previous_personal_bests, scoreData,
            Position = {
                Left = 0.5f %+ 100.0f ^+ 20.0f
                Right = 1.0f %- 40.0f
                Top = 0.0f %+ 40.0f
                Bottom = 0.0f %+ 200.0f
            })
        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.TrimLeft(5.0f).SliceTop(160.0f).TrimTop(5.0f)) Colors.shadow_2.O2
        Draw.rect (this.Bounds.TrimLeft(5.0f).TrimTop(160.0f).SliceTop(5.0f)) Colors.white
        base.Draw()

type BottomBanner(stats: ScoreScreenStats ref, data: ScoreInfoProvider, graph: ScoreGraph, refresh: unit -> unit) as this =
    inherit StaticContainer(NodeType.None)
    
    do
        graph.Position <- { Left = 0.35f %+ 20.0f; Top = 0.0f %+ 25.0f; Right = 1.0f %- 20.0f; Bottom = 1.0f %- 65.0f }
        this
        |+ graph
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

        Draw.rect (this.Bounds.TrimTop 5.0f) (Style.color(127, 0.5f, 0.0f))
        Draw.rect (this.Bounds.SliceTop 5.0f) Colors.white.O2
        
        // stats
        let spacing = (this.Bounds.Height - 40.0f - 180.0f) / 2.0f
        let b = this.Bounds.SliceLeft(this.Bounds.Width * 0.35f).SliceTop(100.0f)
        let l = b.SliceLeft(b.Width * 0.5f).Shrink(5.0f, 20.0f)
        let r = b.SliceRight(b.Width * 0.5f).Shrink(5.0f, 20.0f)

        Draw.rect l Colors.shadow_2.O2
        let hit, total = (!stats).Notes
        Text.drawFillB(Style.baseFont, sprintf "Notes: %i/%i" hit total, l.Shrink(5.0f), Colors.text, Alignment.CENTER)

        let l = l.Translate(0.0f, 60.0f + spacing)
        Draw.rect l Colors.shadow_2.O2
        let hit, total = (!stats).Holds
        Text.drawFillB(Style.baseFont, sprintf "Holds: %i/%i" hit total, l.Shrink(5.0f), Colors.text, Alignment.CENTER)
        
        let l = l.Translate(0.0f, 60.0f + spacing)
        Draw.rect l Colors.shadow_2.O2
        let hit, total = (!stats).Releases
        Text.drawFillB(Style.baseFont, sprintf "Releases: %i/%i" hit total, l.Shrink(5.0f), Colors.text, Alignment.CENTER)

        Draw.rect r Colors.shadow_2.O2
        Text.drawFillB(Style.baseFont, sprintf "Combo: %ix" data.Scoring.State.BestCombo, r.Shrink(5.0f), Colors.text, Alignment.CENTER)
        
        let r = r.Translate(0.0f, 60.0f + spacing)
        Draw.rect r Colors.shadow_2.O2
        Text.drawFillB(Style.baseFont, sprintf "Mean: %.1fms (%.1f - %.1f)" (!stats).Mean (!stats).EarlyMean (!stats).LateMean, r.Shrink(5.0f),Colors.text, Alignment.CENTER)
                
        let r = r.Translate(0.0f, 60.0f + spacing)
        Draw.rect r Colors.shadow_2.O2
        Text.drawFillB(Style.baseFont, sprintf "Stdev: %.1fms" (!stats).StandardDeviation, r.Shrink(5.0f), Colors.text, Alignment.CENTER)

        // graph background
        Draw.rect (graph.Bounds.Expand(5.0f, 5.0f)) Color.White
        Background.draw (graph.Bounds, Color.FromArgb(127, 127, 127), 1.0f)

        base.Draw()