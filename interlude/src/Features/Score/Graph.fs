namespace Interlude.Features.Score

open System.Drawing
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude.Gameplay
open Prelude.Data.Scores
open Interlude.UI
open Interlude.Utils
open Interlude.UI.Menu
open Interlude.Options

module GraphSettings =

    let only_releases = Setting.simple false

type ScoreGraphSettingsPage() as this =
    inherit Page()

    do 
        this.Content(
            column()
            |+ PageSetting("score.graph.settings.graph_mode", Selector<_>.FromEnum options.ScoreGraphMode)
                .Pos(200.0f)
            |+ PageSetting("score.graph.settings.only_releases", Selector<_>.FromBool GraphSettings.only_releases)
                .Pos(300.0f)
        )

    override this.Title = L"score.graph.settings"
    override this.OnClose() = ()

type ScoreGraph(data: ScoreInfoProvider) =
    inherit StaticWidget(NodeType.None)

    let fbo = FBO.create()
    let mutable refresh = true

    let THICKNESS = 5f
    let HTHICKNESS = THICKNESS * 0.5f

    do fbo.Unbind()

    member this.Refresh() = refresh <- true

    member private this.Redraw() =
        refresh <- false
        let h = 0.5f * this.Bounds.Height
        let width = this.Bounds.Width
        fbo.Bind true

        Draw.rect this.Bounds (Colors.black.O3)
        Draw.rect (Rect.Create(this.Bounds.Left, (this.Bounds.Top + h - 2.5f), this.Bounds.Right, (this.Bounds.Top + h + 2.5f))) (Colors.white.O2)

        let events = data.Scoring.HitEvents
        assert (events.Count > 0)

        // line graph
        if options.ScoreGraphMode.Value <> ScoreGraphMode.None then
            let snapshots = data.Scoring.Snapshots
            assert (snapshots.Count > 0)
            let hscale = (width - 10.0f) / snapshots.[snapshots.Count - 1].Time
            for i = 1 to snapshots.Count - 1 do
                let l, r = 
                    if options.ScoreGraphMode.Value = ScoreGraphMode.Combo then 
                        float32 snapshots.[i-1].Combo / float32 data.Scoring.State.BestCombo, float32 snapshots.[i].Combo / float32 data.Scoring.State.BestCombo
                    else float32 snapshots.[i-1].HP, float32 snapshots.[i].HP
                let x1 = this.Bounds.Left + snapshots.[i-1].Time * hscale
                let x2 = this.Bounds.Left + snapshots.[i].Time * hscale
                let y1 = this.Bounds.Bottom - HTHICKNESS - (this.Bounds.Height - THICKNESS) * l
                let y2 = this.Bounds.Bottom - HTHICKNESS - (this.Bounds.Height - THICKNESS) * r
                let theta = System.MathF.Atan((y2 - y1) / (x2 - x1))
                let dy = -HTHICKNESS * System.MathF.Cos theta
                let dx = HTHICKNESS * System.MathF.Sin theta
                Draw.quad (Quad.createv(x1 + dx, y1 + dy)(x2 + dx, y2 + dy)(x2 - dx, y2 - dy)(x1 - dx, y1 - dy)) (Quad.colorOf Colors.green.O3) Sprite.DefaultQuad

        // draw dots
        let hscale = (width - 10.0f) / events.[events.Count - 1].Time
        for ev in events do
            let y, col =
                match ev.Guts with
                | Hit evData ->
                    match evData.Judgement with
                    | Some judgement when not GraphSettings.only_releases.Value -> h - evData.Delta / data.Scoring.MissWindow * (h - THICKNESS - HTHICKNESS), data.Ruleset.JudgementColor judgement
                    | _ -> 0.0f, Color.Transparent
                | Release evData ->
                    match evData.Judgement with
                    | Some judgement -> h - 0.5f * evData.Delta / data.Scoring.MissWindow * (h - THICKNESS - HTHICKNESS), Color.FromArgb(127, data.Ruleset.JudgementColor judgement)
                    | None -> 0.0f, Color.Transparent
            if col.A > 0uy then
                let x = this.Bounds.Left + 5.0f + ev.Time * hscale
                Draw.rect(Rect.Box(x - HTHICKNESS, this.Bounds.Top + y - HTHICKNESS, THICKNESS, THICKNESS)) col

        // early/late
        Text.draw(Style.baseFont, "Early", 16.0f, this.Bounds.Left + 5.0f, this.Bounds.Bottom - 33.0f, Colors.white.O3)
        Text.draw(Style.baseFont, "Late", 16.0f, this.Bounds.Left + 5.0f, this.Bounds.Top + 3.0f, Colors.white.O3)

        fbo.Unbind()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        if moved then refresh <- true

    override this.Draw() =
        if refresh then this.Redraw()
        Draw.sprite Viewport.bounds Color.White fbo.sprite
        if this.Bounds.Contains(Mouse.pos()) then
            let sss = data.Scoring.Snapshots
            let pc = (Mouse.x() - this.Bounds.Left) / this.Bounds.Width
            let snapshot_index = pc * float32 sss.Count |> int |> max 0 |> min (sss.Count - 1)
            let ss = sss.[snapshot_index]
            let box_h = this.Bounds.Height - 40.0f
            let box = Rect.Box(this.Bounds.Left + 20.0f + pc * (this.Bounds.Width - 200.0f - 40.0f), this.Bounds.Top + 20.0f, 200.0f, box_h)
            Draw.rect box Colors.shadow_2.O2
            Text.drawFillB(Style.baseFont, (if ss.MaxPointsScored = 0.0 then 100.0 else 100.0 * ss.PointsScored / ss.MaxPointsScored) |> sprintf "%.2f%%", box.SliceTop(box_h / 3f).Expand(-20.0f, -5.0f), Colors.text, Alignment.LEFT)
            Text.drawFillB(Style.baseFont, ss.Combo |> sprintf "%ix", box.TrimBottom(box_h / 3f).SliceBottom(box_h / 3f).Expand(-20.0f, -5.0f), Colors.text, Alignment.LEFT)
            Text.drawFillB(Style.baseFont, 100.0 * ss.HP |> sprintf "%.0f HP", box.SliceBottom(box_h / 3f).Expand(-20.0f, -5.0f), Colors.text, Alignment.LEFT)

    member this.Dispose() = fbo.Dispose()