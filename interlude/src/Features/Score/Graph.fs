namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Gameplay
open Prelude.Data.User
open Interlude.Options
open Interlude.UI

module GraphSettings =

    let only_releases = Setting.simple false
    let column_filter = Array.create 10 true
    let scale = Setting.bounded 1.0f 1.0f 6.0f

type ScoreGraphSettingsPage(graph: ScoreGraph) =
    inherit Page()

    let mutable column_filter_changed = false
    let column_filter_setting k = Setting.make (fun v -> column_filter_changed <- true; GraphSettings.column_filter.[k] <- v) (fun () -> GraphSettings.column_filter.[k])
    let column_filter_ui, _ = refreshable_row (fun () -> graph.Keys) (fun k _ -> Checkbox(column_filter_setting k, Position = Position.SliceL(50.0f).Translate(float32 k * 100.0f, 0.0f)))

    override this.Content() = 
        page_container()
        |+ PageSetting(%"score.graph.settings.line_mode", 
            SelectDropdown(
                [|
                    ScoreGraphLineMode.None, %"score.graph.settings.line_mode.none"
                    ScoreGraphLineMode.Combo, %"score.graph.settings.line_mode.combo"
                    ScoreGraphLineMode.Mean, %"score.graph.settings.line_mode.mean"
                    ScoreGraphLineMode.StandardDeviation, %"score.graph.settings.line_mode.standard_deviation"
                    ScoreGraphLineMode.Accuracy, %"score.graph.settings.line_mode.accuracy"
                    ScoreGraphLineMode.MA, %"score.graph.settings.line_mode.ma"
                    ScoreGraphLineMode.PA, %"score.graph.settings.line_mode.pa"
                |],
                options.ScoreGraphLineMode
            )
        )
            .Pos(0)
        |+ PageSetting(%"score.graph.settings.line_color", 
            SelectDropdown(
                [|
                    ScoreGraphLineColor.White, %"score.graph.settings.line_color.white"
                    ScoreGraphLineColor.Lamp, %"score.graph.settings.line_color.lamp"
                    ScoreGraphLineColor.Grade, %"score.graph.settings.line_color.grade"
                |],
                options.ScoreGraphLineColor
            )
        )
            .Conditional(fun () -> options.ScoreGraphLineMode.Value <> ScoreGraphLineMode.None)
            .Pos(2)
        |+ PageSetting(%"score.graph.settings.line_on_top", Checkbox(options.ScoreGraphLineOnTop))
            .Conditional(fun () -> options.ScoreGraphLineMode.Value <> ScoreGraphLineMode.None)
            .Pos(4)
        |+ PageSetting(%"score.graph.settings.only_releases", Checkbox GraphSettings.only_releases)
            .Pos(7)
        |+ PageSetting(%"score.graph.settings.column_filter", column_filter_ui)
            .Pos(9, 2, PageWidth.Full)
        |+ PageSetting(%"score.graph.settings.scale", Slider(GraphSettings.scale, Step = 0.25f, Format = fun x -> sprintf "%.0f%%" (x * 100.0f)))
            .Pos(11)
        |+ PageSetting(%"score.graph.settings.windows_background", Checkbox(options.ScoreGraphWindowBackground))
            .Pos(13)
        :> Widget

    override this.Title = %"score.graph.settings"
    override this.OnClose() =
        if column_filter_changed then graph.ApplyColumnFilter()
        graph.Refresh()

and ScoreGraph(score_info: ScoreInfo, stats: ScoreScreenStats ref) =
    inherit StaticWidget(NodeType.None)

    let fbo = FBO.create ()
    let mutable refresh = true
    let mutable expanded = false

    let THICKNESS = 5f
    let HTHICKNESS = THICKNESS * 0.5f

    let BOX_HEIGHT = 200.0f
    let BOX_WIDTH = 350.0f

    let NORMAL_POSITION =
        {
            Left = 0.35f %+ 30.0f
            Top = 0.0f %+ 25.0f
            Right = 1.0f %- 20.0f
            Bottom = 1.0f %- 65.0f
        }

    let EXPANDED_POSITION =
        {
            Left = 0.0f %+ 20.0f
            Top = -(1.0f / 0.35f - 1.0f) %+ 390.0f
            Right = 1.0f %- 20.0f
            Bottom = 1.0f %- 65.0f
        }

    let duration = (score_info.WithMods.LastNote - score_info.WithMods.FirstNote) / score_info.Rate |> format_duration_ms

    do fbo.Unbind()

    override this.Init(parent) =
        this.Position <- NORMAL_POSITION
        base.Init parent

    member this.Refresh() = refresh <- true

    member this.ApplyColumnFilter() =
        stats.Value <- ScoreScreenStats.Generate score_info.Scoring.State.Judgements score_info.Scoring.HitEvents GraphSettings.column_filter

    member this.Keys : int = score_info.WithMods.Keys

    member private this.DrawSnapshotInfo(bounds: Rect, info: ScoreMetricSnapshot) =
        let outline_thickness = 5.0f 
    
        let outline_bounds = bounds.Expand(outline_thickness)

        Draw.rect outline_bounds Colors.white.O4
        Draw.rect bounds Colors.shadow_2.O4
        let mouse_x = Mouse.x()
        let start_y = outline_bounds.Bottom
        let end_y = this.Bounds.Bottom
        let line_rect =
            Rect.Box(
                mouse_x - outline_thickness / 2.0f, 
                start_y,               
                outline_thickness,                   
                end_y - start_y
        )
        Draw.rect line_rect Colors.white.O1
        let row_height = bounds.Height / 4.0f
        let text_b = bounds.SliceT(row_height).Shrink(20.0f, 5.0f)

        Text.fill_b (
            Style.font,
            sprintf "%.4f%%, %ix" (info.Accuracy * 100.0) info.Combo,
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
            text_b.Translate(0.0f, row_height * 3.0f),
            Colors.text,
            Alignment.LEFT
        )

    member private this.PlotLine(y_func: ScoreMetricSnapshot -> float32, color_func: ScoreMetricSnapshot -> Color) =

        let draw_line (x1, y1) (x2, y2) (color: Color) =
            let theta = System.MathF.Atan((y2 - y1) / (x2 - x1))
            let dy = -HTHICKNESS * System.MathF.Cos theta
            let dx = HTHICKNESS * System.MathF.Sin theta

            Draw.untextured_quad
                (Quad.createv (x1 + dx, y1 + dy) (x2 + dx, y2 + dy) (x2 - dx, y2 - dy) (x1 - dx, y1 - dy))
                color.AsQuad

        let snapshots = score_info.Scoring.Snapshots
        let xscale = (this.Bounds.Width - 10.0f) / snapshots.[snapshots.Count - 1].Time

        for i = 1 to snapshots.Count - 1 do
            let l, r =
                y_func snapshots.[i - 1],
                y_func snapshots.[i]

            let x1 = this.Bounds.Left + snapshots.[i - 1].Time * xscale
            let x2 = this.Bounds.Left + snapshots.[i].Time * xscale
            let y1 = this.Bounds.Bottom - HTHICKNESS - (this.Bounds.Height - THICKNESS) * l
            let y2 = this.Bounds.Bottom - HTHICKNESS - (this.Bounds.Height - THICKNESS) * r
                
            draw_line (x1, y1) (x2, y2) (color_func snapshots.[i]).O3

    member private this.DrawWindows() =
        let h = 0.5f * this.Bounds.Height
        let c = this.Bounds.CenterY
        let ms_to_y (time: Time) = c - System.Math.Clamp(time * GraphSettings.scale.Value / score_info.Ruleset.Accuracy.MissWindow, -1.0f, 1.0f) * h

        let mutable time = -score_info.Ruleset.Accuracy.MissWindow
        for (time2, j) in score_info.Ruleset.Accuracy.Timegates do
            Draw.rect (Rect.Create(this.Bounds.Left, ms_to_y time2, this.Bounds.Right, ms_to_y time)) (score_info.Ruleset.JudgementColor(j).O4a 100)
            time <- time2
        Draw.rect
            (Rect.Create(this.Bounds.Left, ms_to_y score_info.Ruleset.Accuracy.MissWindow, this.Bounds.Right, ms_to_y time))
            (score_info.Ruleset.JudgementColor(score_info.Ruleset.Judgements.Length - 1).O4a 100)

    member private this.DrawLabels(color: Color * Color) =
        if expanded && options.ScoreGraphWindowBackground.Value then
            let h = 0.5f * this.Bounds.Height
            let c = this.Bounds.CenterY
            let ms_to_y (time: Time) = c - System.Math.Clamp(time * GraphSettings.scale.Value / score_info.Ruleset.Accuracy.MissWindow, -1.0f, 1.0f) * h

            for (time, _) in score_info.Ruleset.Accuracy.Timegates do
                let label = if time < 0.0f<ms> then sprintf "%gms" time else sprintf "+%gms" time
                Text.draw_b(Style.font, label, 15.0f, this.Bounds.Left + 5.0f, ms_to_y time - (if time < 0.0f<ms> then 24.0f else 0.0f), color)
        else
            Text.draw_b (
                    Style.font, 
                    sprintf "%s (-%.0fms)" (%"score.graph.early") (score_info.Ruleset.Accuracy.MissWindow / GraphSettings.scale.Value), 
                    24.0f,
                    this.Bounds.Left + 10.0f,
                    this.Bounds.Bottom - 40.0f,
                    color
                )
            Text.draw_b (
                Style.font,
                sprintf "%s (+%.0fms)" (%"score.graph.late") (score_info.Ruleset.Accuracy.MissWindow / GraphSettings.scale.Value), 
                24.0f,
                this.Bounds.Left + 10.0f,
                this.Bounds.Top + 3.0f,
                color
            )
        Text.draw_aligned_b (Style.font, duration, 24.0f, this.Bounds.Right - 10.0f, this.Bounds.Bottom - 40.0f, color, Alignment.RIGHT)

    member private this.DrawLineGraph() =
        let line_color =
            match options.ScoreGraphLineColor.Value with
            | ScoreGraphLineColor.Lamp ->
                fun (snapshot: ScoreMetricSnapshot) -> score_info.Ruleset.LampColor snapshot.Lamp
            | ScoreGraphLineColor.Grade ->
                fun (snapshot: ScoreMetricSnapshot) -> 
                    let grade = (snapshot.Accuracy |> Grade.calculate_with_target score_info.Ruleset.Grading.Grades).Grade
                    score_info.Ruleset.GradeColor grade
            | _ -> K Colors.white

        match options.ScoreGraphLineMode.Value with
        | ScoreGraphLineMode.Combo when score_info.Scoring.Snapshots.Count > 0 ->

            let y_func (snapshot: ScoreMetricSnapshot) = float32 snapshot.Combo / float32 score_info.Scoring.State.BestCombo
            this.PlotLine(y_func, line_color)
        
        | ScoreGraphLineMode.Mean when score_info.Scoring.Snapshots.Count > 0 ->

            let yscale = 0.5f / 10.0f<ms>
            let y_func (snapshot: ScoreMetricSnapshot) = 0.5f + snapshot.Mean * yscale |> min 1.0f |> max 0.0f
            this.PlotLine(y_func, line_color)

        | ScoreGraphLineMode.StandardDeviation when score_info.Scoring.Snapshots.Count > 0 ->
        
            let max_sd = score_info.Scoring.Snapshots |> Seq.map _.StandardDeviation |> Seq.max
            let y_func (snapshot: ScoreMetricSnapshot) = snapshot.StandardDeviation / max_sd
            this.PlotLine(y_func, line_color)

        | ScoreGraphLineMode.Accuracy when score_info.Scoring.Snapshots.Count > 0 ->

            let accuracies = score_info.Scoring.Snapshots |> Seq.map _.Accuracy
            let max_acc = accuracies |> Seq.max
            let min_acc = accuracies |> Seq.min
            
            let y_func (snapshot: ScoreMetricSnapshot) = (snapshot.Accuracy - min_acc) / (max_acc - min_acc) |> float32
            this.PlotLine(y_func, line_color)

        | ScoreGraphLineMode.MA when score_info.Scoring.Snapshots.Count > 0 && score_info.Scoring.State.Judgements.Length > 1 ->

            let ma (ss: ScoreMetricSnapshot) = if ss.Judgements.[1] = 0 then float32 ss.Judgements.[0] else float32 ss.Judgements.[0] / float32 ss.Judgements.[1]
            let ratios = score_info.Scoring.Snapshots |> Seq.map ma
            let max_ratio = ratios |> Seq.max
            let min_ratio = ratios |> Seq.min
            
            let y_func (snapshot: ScoreMetricSnapshot) = (ma snapshot - min_ratio) / (max_ratio - min_ratio) |> float32
            this.PlotLine(y_func, line_color)

        | ScoreGraphLineMode.PA when score_info.Scoring.Snapshots.Count > 0 && score_info.Scoring.State.Judgements.Length > 2 ->

            let pa (ss: ScoreMetricSnapshot) = if ss.Judgements.[2] = 0 then float32 ss.Judgements.[1] else float32 ss.Judgements.[1] / float32 ss.Judgements.[2]
            let ratios = score_info.Scoring.Snapshots |> Seq.map pa
            let max_ratio = ratios |> Seq.max
            let min_ratio = ratios |> Seq.min
            
            let y_func (snapshot: ScoreMetricSnapshot) = (pa snapshot - min_ratio) / (max_ratio - min_ratio) |> float32
            this.PlotLine(y_func, line_color)
            
        | _ -> ()
    
    member private this.DrawHits() =
        let events = score_info.Scoring.HitEvents
        assert (events.Count > 0)

        let h = 0.5f * this.Bounds.Height
        let width = this.Bounds.Width
        let xscale = (width - 10.0f) / events.[events.Count - 1].Time

        for ev in events do
            let dot : (float32 * Color) voption =
                match ev.Guts with
                | Hit evData ->

                    match evData.Judgement with
                    | Some judgement when not GraphSettings.only_releases.Value && GraphSettings.column_filter.[ev.Column] ->
                        let y = h - System.Math.Clamp(h * evData.Delta / score_info.Scoring.MissWindow * GraphSettings.scale.Value, -h + THICKNESS, h - THICKNESS)
                        ValueSome(y, score_info.Ruleset.JudgementColor judgement)

                    | _ -> ValueNone

                | Release evData ->

                    match evData.Judgement with
                    | Some judgement when GraphSettings.column_filter.[ev.Column] ->
                        let y = h - System.Math.Clamp(evData.Delta / score_info.Scoring.MissWindow * GraphSettings.scale.Value * 0.5f, -1.0f, 1.0f) * (h - THICKNESS - HTHICKNESS)
                        ValueSome(y, score_info.Ruleset.JudgementColor(judgement).O2)

                    | _ -> ValueNone

            match dot with
            | ValueNone -> ()
            | ValueSome (y, col) ->
                let x = this.Bounds.Left + 5.0f + ev.Time * xscale
                Draw.rect (Rect.Box(x - HTHICKNESS, this.Bounds.Top + y - HTHICKNESS, THICKNESS, THICKNESS)) col

    member private this.Redraw() =
        refresh <- false
        let h = 0.5f * this.Bounds.Height
        fbo.Bind true

        if options.ScoreGraphWindowBackground.Value then
            this.DrawWindows()
        else 
            Draw.rect this.Bounds Colors.black.O1

        Draw.rect
            (Rect.Create(
                this.Bounds.Left,
                (this.Bounds.Top + h - 2.5f),
                this.Bounds.Right,
                (this.Bounds.Top + h + 2.5f)
            ))
            (Colors.white.O2)

        if options.ScoreGraphLineOnTop.Value then
            this.DrawHits()
            this.DrawLineGraph()
        else
            this.DrawLineGraph()
            this.DrawHits()

        fbo.Unbind()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if moved then
            this.Refresh()

        if Mouse.hover this.Bounds then
            if Mouse.right_click() then
                ScoreGraphSettingsPage(this).Show()
            elif Mouse.left_click() then
                expanded <- not expanded
                this.Position <- if expanded then EXPANDED_POSITION else NORMAL_POSITION
                refresh <- true

            let s = Mouse.scroll()
            if s <> 0.0f then
                GraphSettings.scale.Value <- GraphSettings.scale.Value + 0.25f * s
                refresh <- true

        if expanded && (%%"exit").Tapped() then
            expanded <- false
            this.Position <- NORMAL_POSITION
            refresh <- true

    override this.Draw() =
        if refresh then
            this.Redraw()
            
        Draw.rect (this.Bounds.BorderL Style.PADDING) Colors.white
        Draw.rect (this.Bounds.BorderCornersT Style.PADDING) Colors.white
        Draw.rect (this.Bounds.BorderR Style.PADDING) Colors.white
        Draw.rect (this.Bounds.BorderCornersB Style.PADDING) Colors.white

        Draw.rect this.Bounds Colors.black.O3
        Draw.sprite Viewport.bounds Color.White fbo.sprite

        if this.Bounds.Contains(Mouse.pos ()) && score_info.Scoring.Snapshots.Count > 0 then
            let snapshots = score_info.Scoring.Snapshots
            let percent = (Mouse.x () - this.Bounds.Left) / this.Bounds.Width
            let snapshot_index = percent * float32 snapshots.Count |> int |> max 0 |> min (snapshots.Count - 1)
            let current_snapshot = snapshots.[snapshot_index]

            let box =
                Rect.Box(
                    this.Bounds.Left + percent * (this.Bounds.Width - BOX_WIDTH),
                    this.Bounds.Top - BOX_HEIGHT - 20.0f,
                    BOX_WIDTH,
                    BOX_HEIGHT
                )

            this.DrawSnapshotInfo(box, current_snapshot)

            this.DrawLabels((Colors.white.O1, Color.Transparent))
        else
            this.DrawLabels(Colors.text)

    interface System.IDisposable with
        override this.Dispose() = 
            for i = 0 to 9 do GraphSettings.column_filter.[i] <- true
            fbo.Dispose()
