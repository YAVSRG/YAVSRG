namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Scoring
open Prelude.Scoring.Grading
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
            Position = { Left = 0.0f %+ 20.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ 100.0f })
        |+ Text(data.Chart.Header.DiffName,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 20.0f; Top = 0.0f %+ 90.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ 145.0f })
        |+ Text(sprintf "From %s" data.Chart.Header.SourcePack,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 20.0f; Top = 0.0f %+ 140.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ 180.0f })
        |+ Text(data.ScoreInfo.time.ToString(),
            Align = Alignment.RIGHT,
            Position = { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 90.0f; Right = 1.0f %- 20.0f; Bottom = 0.0f %+ 150.0f })
        |* Text((fun () -> "Current session: " + Stats.session_length()),
            Align = Alignment.RIGHT,
            Position = { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 140.0f; Right = 1.0f %- 20.0f; Bottom = 0.0f %+ 180.0f })

    override this.Draw() =

        Draw.rect (this.Bounds.TrimBottom 5.0f) (Style.color(127, 0.5f, 0.0f))
        Draw.rect (this.Bounds.SliceBottom 5.0f) (Color.FromArgb(127, Color.White))

        base.Draw()

type BottomBanner(stats: ScoreScreenStats ref, data: ScoreInfoProvider, graph: ScoreGraph, refresh: unit -> unit) as this =
    inherit StaticContainer(NodeType.None)
    
    do
        graph.Position <- { Left = 0.35f %+ 20.0f; Top = 0.0f %+ 20.0f; Right = 1.0f %- 20.0f; Bottom = 1.0f %- 70.0f }
        this
        |+ graph
        |+ StylishButton(
            ignore,
            sprintf "%s %s" Icons.edit (L"score.graph.settings") |> K,
            Style.main 100,
            Position = { Left = 0.55f %+ 0.0f; Top = 1.0f %- 50.0f; Right = 0.7f %- 25.0f; Bottom = 1.0f %- 0.0f })
        |+ StylishButton(
            (fun () -> ScoreScreenHelpers.watchReplay (data.ModChart, data.ScoreInfo.rate, data.ReplayData)),
            sprintf "%s %s" Icons.preview (L"score.watch_replay") |> K,
            Style.dark 100,
            Position = { Left = 0.7f %+ 0.0f; Top = 1.0f %- 50.0f; Right = 0.85f %- 25.0f; Bottom = 1.0f %- 0.0f })
        |* Rulesets.QuickSwitcher(
            options.SelectedRuleset
            |> Setting.trigger (fun _ -> data.Ruleset <- Rulesets.current; refresh()),
            Position = { Left = 0.85f %+ 0.0f; Top = 1.0f %- 50.0f; Right = 1.0f %- 0.0f; Bottom = 1.0f %- 0.0f })

    override this.Draw() =

        Draw.rect (this.Bounds.TrimTop 5.0f) (Style.color(127, 0.5f, 0.0f))
        Draw.rect (this.Bounds.SliceTop 5.0f) (Color.FromArgb(127, Color.White))
        
        // stats
        let spacing = (this.Bounds.Height - 40.0f - 180.0f) / 2.0f
        let b = this.Bounds.SliceLeft(this.Bounds.Width * 0.35f).SliceTop(100.0f)
        let l = b.SliceLeft(b.Width * 0.5f).Shrink(5.0f, 20.0f)
        let r = b.SliceRight(b.Width * 0.5f).Shrink(5.0f, 20.0f)

        Draw.rect l (Color.FromArgb(127, Color.Black))
        let hit, total = (!stats).Notes
        Text.drawFillB(Style.baseFont, sprintf "Notes: %i/%i" hit total, l.Shrink(5.0f), (Color.White, Color.Black), Alignment.CENTER)

        let l = l.Translate(0.0f, 60.0f + spacing)
        Draw.rect l (Color.FromArgb(127, Color.Black))
        let hit, total = (!stats).Holds
        Text.drawFillB(Style.baseFont, sprintf "Holds: %i/%i" hit total, l.Shrink(5.0f), (Color.White, Color.Black), Alignment.CENTER)
        
        let l = l.Translate(0.0f, 60.0f + spacing)
        Draw.rect l (Color.FromArgb(127, Color.Black))
        let hit, total = (!stats).Releases
        Text.drawFillB(Style.baseFont, sprintf "Releases: %i/%i" hit total, l.Shrink(5.0f), (Color.White, Color.Black), Alignment.CENTER)

        Draw.rect r (Color.FromArgb(127, Color.Black))
        Text.drawFillB(Style.baseFont, sprintf "Combo: %ix" data.Scoring.State.BestCombo, r.Shrink(5.0f), (Color.White, Color.Black), Alignment.CENTER)
        
        let r = r.Translate(0.0f, 60.0f + spacing)
        Draw.rect r (Color.FromArgb(127, Color.Black))
        Text.drawFillB(Style.baseFont, sprintf "Mean: %.1fms (%.1f - %.1f)" (!stats).Mean (!stats).EarlyMean (!stats).LateMean, r.Shrink(5.0f), (Color.White, Color.Black), Alignment.CENTER)
                
        let r = r.Translate(0.0f, 60.0f + spacing)
        Draw.rect r (Color.FromArgb(127, Color.Black))
        Text.drawFillB(Style.baseFont, sprintf "Stdev: %.1fms" (!stats).StandardDeviation, r.Shrink(5.0f), (Color.White, Color.Black), Alignment.CENTER)

        // graph background
        Draw.rect (graph.Bounds.Expand(5.0f, 5.0f)) Color.White
        Background.draw (graph.Bounds, Color.FromArgb(127, 127, 127), 3.0f)

        base.Draw()

type Sidebar(stats: ScoreScreenStats ref, data: ScoreInfoProvider) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        Draw.rect (this.Bounds.Expand(5.0f, 0.0f)) (Color.FromArgb(127, Color.White))
        Background.draw (this.Bounds, (Color.FromArgb(80, 80, 80)), 2.0f)

        let title = this.Bounds.SliceTop(100.0f).Shrink(5.0f, 20.0f)
        Draw.rect title (Color.FromArgb(127, Color.Black))
        Text.drawFillB(Style.baseFont, sprintf "%iK Results  •  %s" data.Chart.Keys data.Ruleset.Name, title, (Color.White, Color.Black), Alignment.CENTER)
        let mods = title.Translate(0.0f, 70.0f)
        Draw.rect mods (Color.FromArgb(127, Color.Black))
        Text.drawFillB(Style.baseFont, data.Mods, mods, (Color.White, Color.Black), Alignment.CENTER)

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
            Text.drawFill(Style.baseFont, sprintf "%s: %i" j.Name judgeCounts.[i], b.Shrink(5.0f, 2.0f), Color.White, 0.0f)
            y <- y + h
        
type Grade(grade: Grade.GradeResult ref, lamp: Lamp.LampResult ref, data: ScoreInfoProvider) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        let x = this.Bounds.CenterX
        let y = this.Bounds.CenterY - 35.0f
        let size = (min this.Bounds.Height this.Bounds.Width) * 0.5f - 50.0f
        let borderSize = size + 15.0f
        Draw.quad
            ( Quad.createv
                (x, y - borderSize)
                (x + borderSize, y)
                (x, y + borderSize)
                (x - borderSize, y)
            )
            (Quad.colorOf (data.Ruleset.GradeColor (!grade).Grade))
            Sprite.DefaultQuad
        Background.drawq ( 
            ( Quad.createv
                (x, y - size)
                (x + size, y)
                (x, y + size)
                (x - size, y)
            ), Color.FromArgb(60, 60, 60), 2.0f
        )
        Draw.quad
            ( Quad.createv
                (x, y - size)
                (x + size, y)
                (x, y + size)
                (x - size, y)
            )
            (Quad.colorOf (Color.FromArgb(40, (data.Ruleset.GradeColor (!grade).Grade))))
            Sprite.DefaultQuad

        // grade stuff
        let gradeBounds = Rect.Box(x - 270.0f, y - 270.0f, 540.0f, 540.0f)
        Text.drawFill(Style.baseFont, data.Ruleset.GradeName (!grade).Grade, gradeBounds.Shrink 100.0f, data.Ruleset.GradeColor (!grade).Grade, 0.5f)
        Draw.quad (Quad.ofRect gradeBounds) (Quad.colorOf Color.White) (Sprite.gridUV (0, (!grade).Grade) <| getTexture "grade-base")
        if (!lamp).Lamp >= 0 then Draw.quad (Quad.ofRect gradeBounds) (Quad.colorOf Color.White) (Sprite.gridUV (0, (!lamp).Lamp) <| getTexture "grade-lamp-overlay")
        Draw.quad (Quad.ofRect gradeBounds) (Quad.colorOf Color.White) (Sprite.gridUV (0, (!grade).Grade) <| getTexture "grade-overlay")
        
type InfoBar(color: unit -> System.Drawing.Color, label: string, text: unit -> string, pb: unit -> PersonalBestType, hint: unit -> string, existingPb: unit -> string) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        let color = color()
        let pb = pb()
        let header = this.Bounds.SliceLeft 200.0f
        let body = this.Bounds.TrimLeft 200.0f
        Draw.rect this.Bounds (Color.FromArgb(80, color))
        let header_card = header.SliceBottom (header.Height * 0.35f)
        Draw.rect header_card (Color.FromArgb(80, color))
        Draw.rect (body.SliceBottom 5.0f) (Color.FromArgb(80, color))
        Text.drawFillB(Style.baseFont, label, header.TrimBottom (header.Height * 0.35f), (Color.White, Color.Black), 0.5f)
        Text.drawFillB(Style.baseFont, text(), body.TrimLeft(10.0f).TrimBottom(header.Height * 0.3f), (color, Color.Black), 0.0f)
        Text.drawFillB(Style.baseFont, hint(), body.TrimLeft(10.0f).SliceBottom(header.Height * 0.35f).TrimBottom(5.0f), (Color.White, Color.Black), 0.0f)
        if pb = PersonalBestType.None then
            Text.drawFillB(Style.baseFont, existingPb(), header_card, (Color.FromArgb(180, 180, 180, 180), Color.Black), 0.5f)
        else
            Text.drawFillB(Style.baseFont, sprintf "%s %s " Icons.sparkle (L"score.new_record"), header_card, (themeConfig().PBColors.[int pb], Color.Black), 0.5f)
