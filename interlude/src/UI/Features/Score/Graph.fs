namespace Interlude.UI.Features.Score

open System.Drawing
open Percyqaz.Flux.Graphics
open Prelude.Scoring
open Prelude.Data.Scores
open Interlude
open Interlude.UI

type ScoreGraph(data: ScoreInfoProvider) =
    inherit Widget1()

    let fbo = FBO.create()
    let mutable refresh = true

    do fbo.Unbind()

    member this.Refresh() = refresh <- true

    member private this.Redraw() =
        refresh <- false
        let width = this.Bounds.Width
        let h = 0.5f * this.Bounds.Height
        fbo.Bind true

        Draw.rect this.Bounds (Color.FromArgb(160, 0, 0, 0))
        Draw.rect (Rect.Create(this.Bounds.Left, (this.Bounds.Top + h - 2.5f), this.Bounds.Right, (this.Bounds.Top + h + 2.5f))) (Color.FromArgb(127, 255, 255, 255))
        
        // todo: graph stuff like hp/accuracy over time
        // todo: let you filter to just release timing

        let events = data.Scoring.HitEvents
        assert (events.Count > 0)

        let hscale = (width - 10.0f) / events.[events.Count - 1].Time
        for ev in events do
            let y, col =
                match ev.Guts with
                | Hit evData ->
                    match evData.Judgement with
                    | Some judgement -> h - evData.Delta / data.Scoring.MissWindow * h, data.Ruleset.JudgementColor judgement
                    | None -> 0.0f, Color.Transparent
                | Release evData ->
                    match evData.Judgement with
                    | Some judgement -> h - 0.5f * evData.Delta / data.Scoring.MissWindow * h, Color.FromArgb(127, data.Ruleset.JudgementColor judgement)
                    | None -> 0.0f, Color.Transparent
            if col.A > 0uy then
                let x = this.Bounds.Left + 5.0f + ev.Time * hscale
                Draw.rect(Rect.Box(x - 2.5f, this.Bounds.Top + y - 2.5f, 5f, 5f)) col
        Text.draw(Content.font, "Early", 18.0f, this.Bounds.Left + 5.0f, this.Bounds.Bottom - 35.0f, Color.FromArgb(127, Color.White))
        Text.draw(Content.font, "Late", 18.0f, this.Bounds.Left + 5.0f, this.Bounds.Top + 5.0f, Color.FromArgb(127, Color.White))

        fbo.Unbind()

    override this.Draw() =
        if refresh then this.Redraw()
        Draw.sprite Viewport.bounds Color.White fbo.sprite

    override this.Dispose() =
        base.Dispose()
        fbo.Dispose()