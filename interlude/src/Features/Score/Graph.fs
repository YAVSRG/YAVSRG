namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Data.User
open Interlude.Options
open Interlude.UI

module GraphSettings =

    let only_releases = Setting.simple false
    let column_filter = Array.create 10 true
    let scale = 1.0f |> Setting.bounded (1.0f, 6.0f)
    let show_slice = Setting.simple false
    let slice_size = 0.04f |> Setting.bounded (0.01f, 1.0f)

    let COLUMN_FILTER_KEYS =
        [|
            Keys.D1
            Keys.D2
            Keys.D3
            Keys.D4
            Keys.D5
            Keys.D6
            Keys.D7
            Keys.D8
            Keys.D9
            Keys.D0
        |]
        |> Array.map Bind.mk

type ScoreGraphSettingsPage(graph: ScoreGraph) =
    inherit Page()

    let mutable column_filter_changed = false
    let column_filter_setting k = Setting.make (fun v -> column_filter_changed <- true; GraphSettings.column_filter.[k] <- v) (fun () -> GraphSettings.column_filter.[k])
    let column_filter_ui, _ =
        refreshable_row
            (fun () -> graph.Keys)
            (fun k _ ->
                Checkbox(column_filter_setting k)
                    .Position(Position.SliceL(50.0f).Translate(float32 k * 80.0f, 0.0f))
            )

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
        |+ PageSetting(%"score.graph.settings.hover_info",
            SelectDropdown(
                [|
                    false, %"score.graph.settings.hover_info.cumulative"
                    true, %"score.graph.settings.hover_info.slice"
                |],
                GraphSettings.show_slice
            )
        )
            .Help(Help.Info("score.graph.settings.hover_info").Hotkey(%"score.graph.settings.hover_info_hint", "graph_alt_info"))
            .Pos(16)
        |+ PageSetting(%"score.graph.settings.slice_size", Slider.Percent GraphSettings.slice_size)
            .Help(Help.Info("score.graph.settings.slice_size"))
            .Pos(18)
        :> Widget

    override this.Title = %"score.graph.settings"
    override this.OnClose() =
        if column_filter_changed then graph.ApplyColumnFilter()
        graph.Refresh()

and ScoreGraph(score_info: ScoreInfo, stats: ScoreScreenStats ref) =
    inherit StaticWidget(NodeType.None)

    let fbo = Render.borrow_fbo()
    let mutable refresh = true
    let mutable expanded = false
    let mutable snapshot_index = 0
    let mutable show_slice_info = false

    let THICKNESS = 5f
    let HTHICKNESS = THICKNESS * 0.5f

    let BOX_HEIGHT = 250.0f
    let BOX_WIDTH = 400.0f

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

    let mutable MAX_WINDOW = score_info.Ruleset.LargestWindow

    do fbo.Unbind()

    override this.Init(parent) =
        this.Position <- NORMAL_POSITION
        base.Init parent

    member this.Refresh() = refresh <- true

    member this.ApplyColumnFilter() =
        stats.Value <- ScoreScreenStats.calculate score_info.Scoring GraphSettings.column_filter

    member this.Keys : int = score_info.WithMods.Keys

    member private this.DrawCumulativeInfo(bounds: Rect, info: GraphPoint) =

        let black_cutoff_area = this.Bounds.SlicePercentR(1.0f - info.Time / stats.Value.GraphPoints.[stats.Value.GraphPoints.Length - 1].Time)
        Render.rect black_cutoff_area Colors.black.O2

        let white_line =
            Rect.FromEdges(
                black_cutoff_area.Left,
                bounds.Bottom,
                black_cutoff_area.Left + Style.PADDING,
                this.Bounds.Bottom
            )
        Render.rect white_line Colors.white.O1

        let outline_bounds = bounds.Expand(Style.PADDING)
        Render.rect outline_bounds Colors.white.O4
        Render.rect bounds Colors.shadow_2.O4

        let row_height = bounds.Height / 5.0f
        let text_b = bounds.SliceT(row_height).Shrink(20.0f, 5.0f)
        let text_color = if stats.Value.ColumnFilterApplied then Colors.text_green else Colors.text
        let judgement_count = Array.sum info.Judgements

        Text.fill_b (
            Style.font,
            sprintf "%.4f%%, %ix" (info.Accuracy * 100.0) info.Combo,
            text_b,
            text_color,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            info.Mean |> sprintf "M: %.2fms",
            text_b.TranslateY(row_height),
            text_color,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            info.StandardDeviation |> sprintf "SD: %.2fms",
            text_b.TranslateY(row_height * 2.0f),
            text_color,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            (info.Judgements |> Seq.map (sprintf "%i") |> String.concat "  |  ") + "   [" + judgement_count.ToString() + "]",
            text_b.TranslateY(row_height * 3.0f),
            text_color,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            (
                if score_info.Rate <> 1.0f<rate> then
                    sprintf "%s (%s)"
                        (format_duration_ms (info.Time / score_info.Rate))
                        (format_duration_ms info.Time)
                else
                    sprintf "%s" (format_duration_ms info.Time)
            ),
            text_b.Translate(0.0f, row_height * 4.0f),
            Colors.text_subheading,
            Alignment.LEFT
        )

    member private this.DrawSliceInfo(bounds: Rect, index: int, width: int) =

        let pre = stats.Value.GraphPoints.[index - width |> max 0]
        let post = stats.Value.GraphPoints.[index + width |> min (stats.Value.GraphPoints.Length - 1)]

        let black_cutoff_left = this.Bounds.SlicePercentL(pre.Time / stats.Value.GraphPoints.[stats.Value.GraphPoints.Length - 1].Time)
        Render.rect black_cutoff_left Colors.black.O2
        let black_cutoff_right = this.Bounds.SlicePercentR(1.0f - post.Time / stats.Value.GraphPoints.[stats.Value.GraphPoints.Length - 1].Time)
        Render.rect black_cutoff_right Colors.black.O2

        let outline_bounds = bounds.Expand(Style.PADDING)
        Render.rect outline_bounds Colors.white.O4
        Render.rect bounds Colors.shadow_2.O4

        let row_height = bounds.Height / 5.0f
        let text_b = bounds.SliceT(row_height).Shrink(20.0f, 5.0f)
        let text_color = if stats.Value.ColumnFilterApplied then Colors.text_green else Colors.text

        let combo = stats.Value.GraphPoints.[index].Combo
        let max_points = post.MaxPointsScored - pre.MaxPointsScored
        let points = post.PointsScored - pre.PointsScored
        let accuracy = if max_points = 0.0 then 1.0 else points / max_points
        let judgement_diff = post.Judgements |> Array.mapi (fun i v -> (v - pre.Judgements.[i]))
        let judgement_count_post = Array.sum post.Judgements
        let judgement_count_pre = Array.sum pre.Judgements
        let mean = ((post.Mean * float32 judgement_count_post) - (pre.Mean * float32 judgement_count_pre)) / float32 (judgement_count_post - judgement_count_pre)
        let standard_deviation =
            ((post.StandardDeviation * post.StandardDeviation * float32 judgement_count_post)
            - (pre.StandardDeviation * pre.StandardDeviation * float32 judgement_count_pre))
            / float32 (judgement_count_post - judgement_count_pre)
            |> sqrt

        Text.fill_b (
            Style.font,
            sprintf "%.4f%% %ix" (accuracy * 100.0) combo,
            text_b,
            text_color,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            mean |> sprintf "M: %.2fms",
            text_b.Translate(0.0f, row_height),
            text_color,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            standard_deviation |> sprintf "SD: %.2fms",
            text_b.Translate(0.0f, row_height * 2.0f),
            text_color,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            (judgement_diff |> Seq.map (sprintf "%i") |> String.concat "  |  ") + "   [" + (judgement_count_post - judgement_count_pre).ToString() + "]",
            text_b.Translate(0.0f, row_height * 3.0f),
            text_color,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            (
                if score_info.Rate <> 1.0f<rate> then
                    sprintf "%s (%s) - %s (%s)"
                        (format_duration_ms (pre.Time / score_info.Rate))
                        (format_duration_ms pre.Time)
                        (format_duration_ms (post.Time / score_info.Rate))
                        (format_duration_ms post.Time)
                else
                    sprintf "%s - %s"
                        (format_duration_ms pre.Time)
                        (format_duration_ms post.Time)
            ),
            text_b.Translate(0.0f, row_height * 4.0f),
            Colors.text_subheading,
            Alignment.LEFT
        )

    member private this.PlotLine(y_func: GraphPoint -> float32, color_func: GraphPoint -> Color) =

        let mutable filler : ((float32 * float32) * (float32 * float32) * Color) voption = ValueNone
        let draw_line (x1, y1) (x2, y2) (color: Color) =
            let theta = System.MathF.Atan2(y2 - y1, x2 - x1)
            let dy = -HTHICKNESS * System.MathF.Cos theta
            let dx = HTHICKNESS * System.MathF.Sin theta

            let p1 = x1 + dx, y1 + dy
            let p2 = x2 + dx, y2 + dy
            let p3 = x2 - dx, y2 - dy
            let p4 = x1 - dx, y1 - dy

            match filler with
            | ValueSome (_p2, _p3, _color) ->
                Render.quad_points p1 _p2 _p3 p4 _color
            | ValueNone -> ()

            Render.quad_points p1 p2 p3 p4 color

            filler <- ValueSome (p2, p3, color)

        let snapshots = stats.Value.GraphPoints
        let xscale = (this.Bounds.Width - 10.0f) / snapshots.[snapshots.Length - 1].Time

        for i = 1 to snapshots.Length - 1 do
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
        let ms_to_y (time: GameplayTime) = c - System.Math.Clamp(time * GraphSettings.scale.Value / MAX_WINDOW, -1.0f, 1.0f) * h

        let mutable previous_early = 0.0f<ms / rate>
        let mutable previous_late = 0.0f<ms / rate>

        for j in score_info.Ruleset.Judgements do
            match j.TimingWindows with
            | Some (early, late) ->
                Render.rect_edges
                    this.Bounds.Left
                    (ms_to_y previous_early)
                    this.Bounds.Right
                    (ms_to_y early)
                    (j.Color.O4a 100)
                previous_early <- early
                Render.rect_edges
                    this.Bounds.Left
                    (ms_to_y late)
                    this.Bounds.Right
                    (ms_to_y previous_late)
                    (j.Color.O4a 100)
                previous_late <- late
            | None -> ()

    member private this.DrawLabels(translucent: bool) =
        let color = if translucent then Colors.white.O1, Color.Transparent else Colors.text

        if expanded && options.ScoreGraphWindowBackground.Value then
            let h = 0.5f * this.Bounds.Height
            let c = this.Bounds.CenterY
            let mutable y1 = c - h - 48.0f
            let mutable y2 = c + h + 24.0f
            let ms_to_y (time: GameplayTime) = c - System.Math.Clamp(time * GraphSettings.scale.Value / MAX_WINDOW, -1.0f, 1.0f) * h

            for j in score_info.Ruleset.Judgements do
                match j.TimingWindows with
                | Some (early, late) ->
                    let early_y = ms_to_y early - 24.0f
                    if early_y - 24.0f > y1 then
                        Text.draw_b(Style.font, sprintf "%gms" early, 15.0f, this.Bounds.Left + 5.0f, early_y, color)
                        y1 <- early_y
                    let late_y = ms_to_y late
                    if late_y + 24.0f < y2 then
                        Text.draw_b(Style.font, sprintf "+%gms" late, 15.0f, this.Bounds.Left + 5.0f, late_y, color)
                        y2 <- late_y
                | None -> ()

        else
            Text.draw_b (
                    Style.font,
                    sprintf "%s (-%.0fms)" (%"score.graph.early") (MAX_WINDOW / GraphSettings.scale.Value),
                    24.0f,
                    this.Bounds.Left + 10.0f,
                    this.Bounds.Bottom - 40.0f,
                    color
                )
            Text.draw_b (
                Style.font,
                sprintf "%s (+%.0fms)" (%"score.graph.late") (MAX_WINDOW / GraphSettings.scale.Value),
                24.0f,
                this.Bounds.Left + 10.0f,
                this.Bounds.Top + 3.0f,
                color
            )
        Text.draw_aligned_b (Style.font, duration, 24.0f, this.Bounds.Right - 10.0f, this.Bounds.Bottom - 40.0f, color, Alignment.RIGHT)
        if score_info.IsFailed then
            let color = if translucent then Colors.red.O1, Color.Transparent else Colors.text_red
            Text.draw_aligned_b (Style.font, %"score.graph.failed", 24.0f, this.Bounds.Right - 10.0f, this.Bounds.Top + 3.0f, color, Alignment.RIGHT)

        if stats.Value.ColumnFilterApplied then
            let color = if translucent then Colors.green_accent.O1, Color.Transparent else Colors.text_green
            let text =
                GraphSettings.column_filter
                |> Seq.indexed
                |> Seq.choose (fun (i, b) -> if b && i < score_info.WithMods.Keys then Some ((i + 1).ToString()) else None)
                |> String.concat " "
            Text.draw_aligned_b (Style.font, sprintf "%s: %s" %"score.graph.settings.column_filter" text, 24.0f, this.Bounds.CenterX, this.Bounds.Bottom - 40.0f, color, Alignment.CENTER)

    member private this.DrawLineGraph() =
        let line_color =
            match options.ScoreGraphLineColor.Value with
            | ScoreGraphLineColor.Lamp ->
                fun (snapshot: GraphPoint) -> score_info.Ruleset.LampColor snapshot.Lamp
            | ScoreGraphLineColor.Grade ->
                fun (snapshot: GraphPoint) ->
                    let grade = Grade.calculate score_info.Ruleset.Grades snapshot.Accuracy
                    score_info.Ruleset.GradeColor grade
            | _ -> K Colors.white

        match options.ScoreGraphLineMode.Value with
        | ScoreGraphLineMode.Combo when stats.Value.GraphPoints.Length > 0 ->

            let best_combo = stats.Value.GraphPoints |> Seq.map _.Combo |> Seq.max |> float32
            let y_func (snapshot: GraphPoint) = float32 snapshot.Combo / best_combo
            this.PlotLine(y_func, line_color)

        | ScoreGraphLineMode.Mean when stats.Value.GraphPoints.Length > 0 ->

            let yscale = 0.5f / 10.0f<ms / rate>
            let y_func (snapshot: GraphPoint) = 0.5f + snapshot.Mean * yscale |> min 1.0f |> max 0.0f
            this.PlotLine(y_func, line_color)

        | ScoreGraphLineMode.StandardDeviation when stats.Value.GraphPoints.Length > 0 ->

            let max_sd = stats.Value.GraphPoints |> Seq.map _.StandardDeviation |> Seq.max
            let y_func (snapshot: GraphPoint) = snapshot.StandardDeviation / max_sd
            this.PlotLine(y_func, line_color)

        | ScoreGraphLineMode.Accuracy when stats.Value.GraphPoints.Length > 0 ->

            let accuracies = stats.Value.GraphPoints |> Seq.map _.Accuracy
            let max_acc = accuracies |> Seq.max
            let min_acc = accuracies |> Seq.min

            let y_func (snapshot: GraphPoint) = (snapshot.Accuracy - min_acc) / (max_acc - min_acc) |> float32
            this.PlotLine(y_func, line_color)

        | ScoreGraphLineMode.MA when stats.Value.GraphPoints.Length > 0 && score_info.Scoring.JudgementCounts.Length > 1 ->

            let ma (ss: GraphPoint) = if ss.Judgements.[1] = 0 then float32 ss.Judgements.[0] else float32 ss.Judgements.[0] / float32 ss.Judgements.[1]
            let ratios = stats.Value.GraphPoints |> Seq.map ma
            let max_ratio = ratios |> Seq.max
            let min_ratio = ratios |> Seq.min

            let y_func (snapshot: GraphPoint) = (ma snapshot - min_ratio) / (max_ratio - min_ratio) |> float32
            this.PlotLine(y_func, line_color)

        | ScoreGraphLineMode.PA when stats.Value.GraphPoints.Length > 0 && score_info.Scoring.JudgementCounts.Length > 2 ->

            let pa (ss: GraphPoint) = if ss.Judgements.[2] = 0 then float32 ss.Judgements.[1] else float32 ss.Judgements.[1] / float32 ss.Judgements.[2]
            let ratios = stats.Value.GraphPoints |> Seq.map pa
            let max_ratio = ratios |> Seq.max
            let min_ratio = ratios |> Seq.min

            let y_func (snapshot: GraphPoint) = (pa snapshot - min_ratio) / (max_ratio - min_ratio) |> float32
            this.PlotLine(y_func, line_color)

        | _ -> ()

    member private this.DrawHits() =
        let events = score_info.Scoring.Events
        assert (events.Count > 0)

        let h = 0.5f * this.Bounds.Height
        let width = this.Bounds.Width
        let xscale = (width - 10.0f) / events.[events.Count - 1].Time

        for ev in events do
            let dot : (float32 * Color) voption =
                match ev.Action with
                | Hit e
                | Hold e ->

                    match e.Judgement with
                    | Some (judgement, _) when not GraphSettings.only_releases.Value && GraphSettings.column_filter.[ev.Column] ->
                        let y =
                            if e.Missed then THICKNESS
                            else h - System.Math.Clamp(h * e.Delta / MAX_WINDOW * GraphSettings.scale.Value, -h + THICKNESS, h - THICKNESS)
                        ValueSome(y, score_info.Ruleset.JudgementColor judgement)

                    | _ -> ValueNone

                | Release e ->

                    match e.Judgement with
                    | Some (judgement, _) when GraphSettings.column_filter.[ev.Column] ->
                        // todo: figure something out about half-scale releases
                        let y =
                            if e.Missed then THICKNESS
                            else h - System.Math.Clamp(e.Delta / MAX_WINDOW * GraphSettings.scale.Value * 0.5f, -1.0f, 1.0f) * (h - THICKNESS - HTHICKNESS)
                        ValueSome(y, score_info.Ruleset.JudgementColor(judgement).O2)

                    | _ -> ValueNone

                | GhostTap e ->
                    match e.Judgement with
                    | Some (judgement, _) when not GraphSettings.only_releases.Value && GraphSettings.column_filter.[ev.Column] ->
                        let y = 2f * h - THICKNESS
                        ValueSome(y, score_info.Ruleset.JudgementColor judgement)
                    | _ -> ValueNone

                | DropHold
                | RegrabHold -> ValueNone

            match dot with
            | ValueNone -> ()
            | ValueSome (y, col) ->
                let x = this.Bounds.Left + 5.0f + ev.Time * xscale
                Render.rect_size
                    (x - HTHICKNESS)
                    (this.Bounds.Top + y - HTHICKNESS)
                    THICKNESS
                    THICKNESS
                    col

    member private this.Redraw() =
        refresh <- false
        MAX_WINDOW <- score_info.Ruleset.LargestWindow
        let h = 0.5f * this.Bounds.Height
        fbo.Bind true

        if options.ScoreGraphWindowBackground.Value then
            this.DrawWindows()
        else
            Render.rect this.Bounds Colors.black.O1

        Render.rect_edges
            this.Bounds.Left
            (this.Bounds.Top + h - 2.5f)
            this.Bounds.Right
            (this.Bounds.Top + h + 2.5f)
            Colors.white.O2

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
            if Mouse.right_clicked() then
                ScoreGraphSettingsPage(this).Show()
            elif Mouse.left_clicked() then
                expanded <- not expanded
                this.Position <- if expanded then EXPANDED_POSITION else NORMAL_POSITION
                refresh <- true

            let s = Mouse.scroll()
            if GraphSettings.show_slice.Value <> (%%"graph_alt_info").Held() then
                show_slice_info <- true
                GraphSettings.slice_size.Value <- GraphSettings.slice_size.Value + s * 0.005f
            else
                show_slice_info <- false
                if s <> 0.0f then
                    GraphSettings.scale.Value <- GraphSettings.scale.Value + 0.25f * s
                    refresh <- true

        for k = 0 to score_info.WithMods.Keys - 1 do
            if GraphSettings.COLUMN_FILTER_KEYS.[k].Pressed() then
                GraphSettings.column_filter.[k] <- not GraphSettings.column_filter.[k]
                this.ApplyColumnFilter()
                this.Refresh()

        if expanded && (%%"exit").Pressed() then
            expanded <- false
            this.Position <- NORMAL_POSITION
            refresh <- true

    override this.Draw() =
        if refresh then
            this.Redraw()

        Render.border Style.PADDING this.Bounds Colors.white
        Render.rect this.Bounds Colors.black.O3
        Render.sprite (Render.bounds()) Color.White fbo.Sprite

        let snapshots = stats.Value.GraphPoints
        if this.Bounds.Contains(Mouse.pos ()) && snapshots.Length > 0 then
            let percent = (Mouse.x () - this.Bounds.Left) / this.Bounds.Width
            let time = percent * snapshots.[snapshots.Length - 1].Time

            if snapshot_index >= snapshots.Length then
                snapshot_index <- int (percent * float32 snapshots.Length) |> max 0 |> min (snapshots.Length - 1)

            while snapshot_index > 0 && snapshots.[snapshot_index].Time > time do
                snapshot_index <- snapshot_index - 1
            while snapshot_index + 1 < snapshots.Length && snapshots.[snapshot_index + 1].Time <= time do
                snapshot_index <- snapshot_index + 1

            let current_snapshot = snapshots.[snapshot_index]

            let box =
                Rect.FromSize(
                    this.Bounds.Left + percent * (this.Bounds.Width - BOX_WIDTH),
                    this.Bounds.Top - BOX_HEIGHT - 20.0f,
                    BOX_WIDTH,
                    BOX_HEIGHT
                )

            if show_slice_info then
                this.DrawSliceInfo(box, snapshot_index, int (GraphSettings.slice_size.Value * 0.5f * float32 ScoreScreenStats.GRAPH_POINT_COUNT))
            else
                this.DrawCumulativeInfo(box, current_snapshot)

            this.DrawLabels true
        else
            this.DrawLabels false

    interface System.IDisposable with
        override this.Dispose() =
            for i = 0 to 9 do GraphSettings.column_filter.[i] <- true
            (fbo :> System.IDisposable).Dispose()