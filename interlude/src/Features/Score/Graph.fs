namespace Interlude.Features.Score

open System.Drawing
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude.Gameplay
open Prelude.Data
open Interlude.Utils
open Interlude.UI.Menu
open Interlude.Options

module GraphSettings =

    let only_releases = Setting.simple false

type ScoreGraphSettingsPage() as this =
    inherit Page()

    do
        this.Content(
            page_container()
            |+ PageSetting("score.graph.settings.graph_mode", Selector<_>.FromEnum options.ScoreGraphMode)
                .Pos(0)
            |+ PageSetting("score.graph.settings.only_releases", Selector<_>.FromBool GraphSettings.only_releases)
                .Pos(3)
        )

    override this.Title = %"score.graph.settings"
    override this.OnClose() = ()

type ScoreGraph(score_info: ScoreInfo) =
    inherit StaticWidget(NodeType.None)

    let fbo = FBO.create ()
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

        Draw.rect
            (Rect.Create(
                this.Bounds.Left,
                (this.Bounds.Top + h - 2.5f),
                this.Bounds.Right,
                (this.Bounds.Top + h + 2.5f)
            ))
            (Colors.white.O2)

        let events = score_info.Scoring.HitEvents
        assert (events.Count > 0)

        // line graph
        if
            options.ScoreGraphMode.Value = ScoreGraphMode.Combo
            && score_info.Scoring.Snapshots.Count > 0
        then
            let snapshots = score_info.Scoring.Snapshots
            let hscale = (width - 10.0f) / snapshots.[snapshots.Count - 1].Time

            for i = 1 to snapshots.Count - 1 do
                let l, r =
                    float32 snapshots.[i - 1].Combo / float32 score_info.Scoring.State.BestCombo,
                    float32 snapshots.[i].Combo / float32 score_info.Scoring.State.BestCombo

                let color = score_info.Ruleset.LampColor snapshots.[i].Lamp
                let x1 = this.Bounds.Left + snapshots.[i - 1].Time * hscale
                let x2 = this.Bounds.Left + snapshots.[i].Time * hscale
                let y1 = this.Bounds.Bottom - HTHICKNESS - (this.Bounds.Height - THICKNESS) * l
                let y2 = this.Bounds.Bottom - HTHICKNESS - (this.Bounds.Height - THICKNESS) * r
                let theta = System.MathF.Atan((y2 - y1) / (x2 - x1))
                let dy = -HTHICKNESS * System.MathF.Cos theta
                let dx = HTHICKNESS * System.MathF.Sin theta

                Draw.untextured_quad
                    (Quad.createv (x1 + dx, y1 + dy) (x2 + dx, y2 + dy) (x2 - dx, y2 - dy) (x1 - dx, y1 - dy))
                    (Quad.color color.O3)

        // draw dots
        let hscale = (width - 10.0f) / events.[events.Count - 1].Time

        for ev in events do
            let y, col =
                match ev.Guts with
                | Hit evData ->
                    match evData.Judgement with
                    | Some judgement when not GraphSettings.only_releases.Value ->
                        h - evData.Delta / score_info.Scoring.MissWindow * (h - THICKNESS - HTHICKNESS),
                        score_info.Ruleset.JudgementColor judgement
                    | _ -> 0.0f, Color.Transparent
                | Release evData ->
                    match evData.Judgement with
                    | Some judgement ->
                        h
                        - 0.5f * evData.Delta / score_info.Scoring.MissWindow
                          * (h - THICKNESS - HTHICKNESS),
                        Color.FromArgb(127, score_info.Ruleset.JudgementColor judgement)
                    | None -> 0.0f, Color.Transparent

            if col.A > 0uy then
                let x = this.Bounds.Left + 5.0f + ev.Time * hscale
                Draw.rect (Rect.Box(x - HTHICKNESS, this.Bounds.Top + y - HTHICKNESS, THICKNESS, THICKNESS)) col

        // early/late
        Text.draw (Style.font, "Early", 16.0f, this.Bounds.Left + 5.0f, this.Bounds.Bottom - 33.0f, Colors.white.O3)
        Text.draw (Style.font, "Late", 16.0f, this.Bounds.Left + 5.0f, this.Bounds.Top + 3.0f, Colors.white.O3)

        fbo.Unbind()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if moved then
            refresh <- true

    override this.Draw() =
        if refresh then
            this.Redraw()

        Draw.sprite Viewport.bounds Color.White fbo.sprite

        if this.Bounds.Contains(Mouse.pos ()) && score_info.Scoring.Snapshots.Count > 0 then
            let sss = score_info.Scoring.Snapshots
            let pc = (Mouse.x () - this.Bounds.Left) / this.Bounds.Width
            let snapshot_index = pc * float32 sss.Count |> int |> max 0 |> min (sss.Count - 1)
            let ss = sss.[snapshot_index]
            let box_h = this.Bounds.Height - 80.0f

            let box =
                Rect.Box(
                    this.Bounds.Left + 40.0f + pc * (this.Bounds.Width - 200.0f - 80.0f),
                    this.Bounds.Top + 40.0f,
                    200.0f,
                    box_h
                )

            Draw.rect box Colors.shadow_2.O2

            Text.fill_b (
                Style.font,
                (if ss.MaxPointsScored = 0.0 then
                     100.0
                 else
                     100.0 * ss.PointsScored / ss.MaxPointsScored)
                |> sprintf "%.2f%%",
                box.SliceTop(box_h * 0.5f).Expand(-20.0f, -5.0f),
                Colors.text,
                Alignment.LEFT
            )

            Text.fill_b (
                Style.font,
                ss.Combo |> sprintf "%ix",
                box.TrimTop(box_h * 0.5f).Expand(-20.0f, -5.0f),
                Colors.text,
                Alignment.LEFT
            )

    member this.Dispose() = fbo.Dispose()
