namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Gameplay
open Prelude.Data
open Interlude.Options
open Interlude.UI

module GraphSettings =

    let only_releases = Setting.simple false
    let column_filter = Array.create 10 true

type ScoreGraphSettingsPage(graph: ScoreGraph) =
    inherit Page()

    let column_filter_setting k = Setting.make (fun v -> GraphSettings.column_filter.[k] <- v) (fun () -> GraphSettings.column_filter.[k])
    let column_filter_ui, _ = refreshable_row (fun () -> graph.Keys) (fun k _ -> Checkbox(column_filter_setting k, Position = Position.SliceLeft(100.0f).Translate(float32 k * 100.0f, 0.0f)))

    override this.Content() = 
        page_container()
        |+ PageSetting(%"score.graph.settings.graph_mode", 
            SelectDropdown(
                [|
                    ScoreGraphMode.None, %"score.graph.settings.graph_mode.none"
                    ScoreGraphMode.Combo, %"score.graph.settings.graph_mode.combo"
                    ScoreGraphMode.Mean, %"score.graph.settings.graph_mode.mean"
                    ScoreGraphMode.StandardDeviation, %"score.graph.settings.graph_mode.standard_deviation"
                |],
                options.ScoreGraphMode
            )
        )
            .Pos(0)
        |+ PageSetting(%"score.graph.settings.only_releases", Checkbox GraphSettings.only_releases)
            .Pos(3)
        |+ PageSetting(%"score.graph.settings.column_filter", column_filter_ui)
            .Pos(5, 2, PageWidth.Full)
        :> Widget

    override this.Title = %"score.graph.settings"
    override this.OnClose() = graph.Refresh()

and ScoreGraph(score_info: ScoreInfo) =
    inherit StaticWidget(NodeType.None)

    let fbo = FBO.create ()
    let mutable refresh = true

    let THICKNESS = 5f
    let HTHICKNESS = THICKNESS * 0.5f

    let duration = (score_info.WithMods.LastNote - score_info.WithMods.FirstNote) / score_info.Rate |> Interlude.Utils.format_duration_ms

    let draw_snapshot_info (bounds: Rect) (info: ScoreMetricSnapshot) =
        Draw.rect bounds Colors.shadow_2.O2
        let row_height = bounds.Height / 4.0f
        let text_b = bounds.SliceTop(row_height).Shrink(20.0f, 5.0f)

        let accuracy = 
            if info.MaxPointsScored = 0.0 then
                100.0
            else
                100.0 * info.PointsScored / info.MaxPointsScored

        Text.fill_b (
            Style.font,
            sprintf "%.4f%%, %ix" accuracy info.Combo,
            text_b,
            Colors.text,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            info.Mean |> sprintf "M: %.2fms",
            text_b.Translate(0.0f, row_height),
            Colors.text,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            info.StandardDeviation |> sprintf "SD: %.2fms",
            text_b.Translate(0.0f, row_height * 2.0f),
            Colors.text,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            info.Judgements |> Seq.map (sprintf "%i") |> String.concat "  |  ",
            text_b.Translate(0.0f, row_height * 3.0f).Shrink(0.0f, 5.0f),
            Colors.text,
            Alignment.LEFT
        )

    do fbo.Unbind()

    member this.Refresh() = refresh <- true

    member this.Keys : int = score_info.WithMods.Keys

    member private this.Redraw() =
        refresh <- false
        let h = 0.5f * this.Bounds.Height
        let width = this.Bounds.Width
        fbo.Bind true

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

        let draw_line (x1, y1) (x2, y2) (color: Color) =
            let theta = System.MathF.Atan((y2 - y1) / (x2 - x1))
            let dy = -HTHICKNESS * System.MathF.Cos theta
            let dx = HTHICKNESS * System.MathF.Sin theta

            Draw.untextured_quad
                (Quad.createv (x1 + dx, y1 + dy) (x2 + dx, y2 + dy) (x2 - dx, y2 - dy) (x1 - dx, y1 - dy))
                color.O3.AsQuad

        // line graph
        match options.ScoreGraphMode.Value with
        | ScoreGraphMode.Combo when score_info.Scoring.Snapshots.Count > 0 ->
            let snapshots = score_info.Scoring.Snapshots
            let xscale = (width - 10.0f) / snapshots.[snapshots.Count - 1].Time

            for i = 1 to snapshots.Count - 1 do
                let l, r =
                    float32 snapshots.[i - 1].Combo / float32 score_info.Scoring.State.BestCombo,
                    float32 snapshots.[i].Combo / float32 score_info.Scoring.State.BestCombo

                let color = score_info.Ruleset.LampColor snapshots.[i].Lamp
                let x1 = this.Bounds.Left + snapshots.[i - 1].Time * xscale
                let x2 = this.Bounds.Left + snapshots.[i].Time * xscale
                let y1 = this.Bounds.Bottom - HTHICKNESS - (this.Bounds.Height - THICKNESS) * l
                let y2 = this.Bounds.Bottom - HTHICKNESS - (this.Bounds.Height - THICKNESS) * r
                
                draw_line (x1, y1) (x2, y2) color
        
        | ScoreGraphMode.Mean when score_info.Scoring.Snapshots.Count > 0 ->
            let snapshots = score_info.Scoring.Snapshots
            let xscale = (width - 10.0f) / snapshots.[snapshots.Count - 1].Time
            let yscale = 0.5f / 10.0f<ms>
            let height = this.Bounds.Height

            for i = 1 to snapshots.Count - 1 do
                let l, r =
                    0.5f + snapshots.[i - 1].Mean * yscale |> min 1.0f |> max 0.0f,
                    0.5f + snapshots.[i].Mean * yscale |> min 1.0f |> max 0.0f

                let color = score_info.Ruleset.LampColor snapshots.[i].Lamp
                let x1 = this.Bounds.Left + snapshots.[i - 1].Time * xscale
                let x2 = this.Bounds.Left + snapshots.[i].Time * xscale
                let y1 = this.Bounds.Bottom - HTHICKNESS - (height - THICKNESS) * l
                let y2 = this.Bounds.Bottom - HTHICKNESS - (height - THICKNESS) * r
                
                draw_line (x1, y1) (x2, y2) color

        | ScoreGraphMode.StandardDeviation when score_info.Scoring.Snapshots.Count > 0 ->
            let snapshots = score_info.Scoring.Snapshots
            let xscale = (width - 10.0f) / snapshots.[snapshots.Count - 1].Time
            let yscale = 1.0f / 25.0f<ms>
            let height = this.Bounds.Height

            for i = 1 to snapshots.Count - 1 do
                let l, r =
                    snapshots.[i - 1].StandardDeviation * yscale |> min 1.0f,
                    snapshots.[i].StandardDeviation * yscale |> min 1.0f

                let color = score_info.Ruleset.LampColor snapshots.[i].Lamp
                let x1 = this.Bounds.Left + snapshots.[i - 1].Time * xscale
                let x2 = this.Bounds.Left + snapshots.[i].Time * xscale
                let y1 = this.Bounds.Bottom - HTHICKNESS - (height - THICKNESS) * l
                let y2 = this.Bounds.Bottom - HTHICKNESS - (height - THICKNESS) * r
                
                draw_line (x1, y1) (x2, y2) color

        | _ -> ()

        // draw dots
        let hscale = (width - 10.0f) / events.[events.Count - 1].Time

        for ev in events do
            let dot : (float32 * Color) voption =
                match ev.Guts with
                | Hit evData ->

                    match evData.Judgement with
                    | Some judgement when not GraphSettings.only_releases.Value && GraphSettings.column_filter.[ev.Column] ->
                        let y = h - evData.Delta / score_info.Scoring.MissWindow * (h - THICKNESS - HTHICKNESS)
                        ValueSome(y, score_info.Ruleset.JudgementColor judgement)

                    | _ -> ValueNone

                | Release evData ->

                    match evData.Judgement with
                    | Some judgement when GraphSettings.column_filter.[ev.Column] ->
                        let y = h - 0.5f * evData.Delta / score_info.Scoring.MissWindow * (h - THICKNESS - HTHICKNESS)
                        ValueSome(y, score_info.Ruleset.JudgementColor(judgement).O2)

                    | _ -> ValueNone

            match dot with
            | ValueNone -> ()
            | ValueSome (y, col) ->
                let x = this.Bounds.Left + 5.0f + ev.Time * hscale
                Draw.rect (Rect.Box(x - HTHICKNESS, this.Bounds.Top + y - HTHICKNESS, THICKNESS, THICKNESS)) col


        fbo.Unbind()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if moved then
            refresh <- true

        if Mouse.hover this.Bounds && Mouse.left_click() then
            ScoreGraphSettingsPage(this).Show()

    override this.Draw() =
        if refresh then
            this.Redraw()

        Draw.rect this.Bounds Colors.black.O2

        Draw.sprite Viewport.bounds Color.White fbo.sprite

        if this.Bounds.Contains(Mouse.pos ()) && score_info.Scoring.Snapshots.Count > 0 then
            let snapshots = score_info.Scoring.Snapshots
            let percent = (Mouse.x () - this.Bounds.Left) / this.Bounds.Width
            let snapshot_index = percent * float32 snapshots.Count |> int |> max 0 |> min (snapshots.Count - 1)
            let current_snapshot = snapshots.[snapshot_index]
            let BOX_PADDING_X = 10.0f
            let BOX_PADDING_Y = 0.0f
            let BOX_WIDTH = 300.0f

            let box =
                Rect.Box(
                    this.Bounds.Left + BOX_PADDING_X + percent * (this.Bounds.Width - BOX_WIDTH - BOX_PADDING_X - BOX_PADDING_X),
                    this.Bounds.Top + BOX_PADDING_Y,
                    BOX_WIDTH,
                    this.Bounds.Height - BOX_PADDING_Y - BOX_PADDING_Y
                )

            draw_snapshot_info box current_snapshot

            Text.draw (Style.font, %"score.graph.early", 24.0f, this.Bounds.Left + 10.0f, this.Bounds.Bottom - 40.0f, Colors.white.O1)
            Text.draw (Style.font, %"score.graph.late", 24.0f, this.Bounds.Left + 10.0f, this.Bounds.Top + 3.0f, Colors.white.O1)
            Text.draw_aligned (Style.font, duration, 24.0f, this.Bounds.Right - 10.0f, this.Bounds.Bottom - 40.0f, Colors.white.O1, Alignment.RIGHT)

        else
            Text.draw_b (Style.font, %"score.graph.early", 24.0f, this.Bounds.Left + 10.0f, this.Bounds.Bottom - 40.0f, Colors.text)
            Text.draw_b (Style.font, %"score.graph.late", 24.0f, this.Bounds.Left + 10.0f, this.Bounds.Top + 3.0f, Colors.text)
            Text.draw_aligned_b (Style.font, duration, 24.0f, this.Bounds.Right - 10.0f, this.Bounds.Bottom - 40.0f, Colors.text, Alignment.RIGHT)

    member this.Dispose() = fbo.Dispose()
