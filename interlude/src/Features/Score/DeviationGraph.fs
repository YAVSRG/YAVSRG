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
module DeviationGraphSettings =
    let hit_size = 1.0f |> Setting.bounded (0.01f, 5.0f)
    let show_releases = Setting.simple true
    let show_hits = Setting.simple true
    let column_filter = Array.create 10 true
    let scale = 1.0f |> Setting.bounded (1.0f, 6.0f)
    let show_slice = Setting.simple true
    let slice_size = 0.04f |> Setting.bounded (0.01f, 1.0f)
    let sqrt_scale = Setting.simple true

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

type DeviationGraphSettingsPage(deviationgraph: DeviationGraph) =
    inherit Page()
    let mutable column_filter_changed = false
    let column_filter_setting k = Setting.make (fun v -> column_filter_changed <- true; DeviationGraphSettings.column_filter.[k] <- v) (fun () -> DeviationGraphSettings.column_filter.[k])
    let column_filter_ui, _ =
        refreshable_row
            (fun () -> deviationgraph.Keys)
            (fun k _ ->
                Checkbox(column_filter_setting k)
                    .Position(Position.SliceL(50.0f).Translate(float32 k * 80.0f, 0.0f))
            )
    override this.Content() =
        page_container()
        |+ PageSetting(%"deviation.graph.settings.hit_size", Slider (DeviationGraphSettings.hit_size, Format = sprintf "%.2fms"))
            .Pos(2)
        |+ PageSetting(%"deviation.graph.settings.show_releases", Checkbox DeviationGraphSettings.show_releases)
            .Pos(4)
        |+ PageSetting(%"deviation.graph.settings.show_hits", Checkbox DeviationGraphSettings.show_hits)
            .Pos(6)
        |+ PageSetting(%"deviation.graph.settings.column_filter", column_filter_ui)
            .Pos(8, 2, PageWidth.Full)
        |+ PageSetting(%"deviation.graph.settings.scale", Slider(DeviationGraphSettings.scale, Step = 0.25f, Format = fun x -> sprintf "%.0f%%" (x * 100.0f)))
            .Pos(10)
        |+ PageSetting(%"deviation.graph.settings.slice_size", Slider.Percent DeviationGraphSettings.slice_size)
            .Help(Help.Info("score.graph.settings.slice_size"))
            .Pos(12)
            |+ PageSetting(%"deviation.graph.settings.windows_background", Checkbox(options.DeviationGraphWindowBackground))
            .Pos(14)
        |+ PageSetting(%"deviation.graph.settings.log_scale", Checkbox DeviationGraphSettings.sqrt_scale)
            .Pos(16)
        :> Widget
    override this.Title = %"deviation.graph.settings"
    override this.OnClose() =
        if column_filter_changed then deviationgraph.ApplyColumnFilter()
        deviationgraph.Refresh()

and DeviationGraph(score_info: ScoreInfo, stats: ScoreScreenStats ref) =
    inherit StaticWidget(NodeType.None)
    let fbo = Render.borrow_fbo()
    let mutable refresh = true
    let mutable expanded = false
    let mutable snapshot_index = 0
    let mutable show_slice_info = true

    let THICKNESS = 5f
    let HTHICKNESS = THICKNESS * 0.5f

    let BOX_HEIGHT = 250.0f
    let BOX_WIDTH = 400.0f

    let NORMAL_POSITION =
        {
            Left = 0.35f %+ 30.0f
            Top = 0.0f %- 275f
            Right = 1.0f %- 20.0f
            Bottom = 1.0f %- 365f
        }

    let EXPANDED_POSITION =
        {
            Left = 0.0f %+ 20.0f
            Top = -(1.0f / 0.35f - 1.0f) %+ 390.0f
            Right = 1.0f %- 20.0f
            Bottom = 1.0f %- 65.0f
        }

    let mutable MAX_WINDOW = score_info.Ruleset.LargestWindow

    do fbo.Unbind()

    override this.Init(parent) =
        this.Position <- NORMAL_POSITION
        base.Init parent

    member this.Refresh() = refresh <- true

    member this.ApplyColumnFilter() =
        stats.Value <- ScoreScreenStats.calculate score_info.Scoring DeviationGraphSettings.column_filter

    member this.Keys : int = score_info.WithMods.Keys

    member private this.DrawWindows() =
        let w = 0.5f * this.Bounds.Width
        let c = this.Bounds.CenterX
        let ms_to_x (time: GameplayTime) =
            c + System.Math.Clamp(time * DeviationGraphSettings.scale.Value / MAX_WINDOW, -1.0f, 1.0f) * w

        let mutable previous_early = 0.0f<ms / rate>
        let mutable previous_late = 0.0f<ms / rate>

        for j in score_info.Ruleset.Judgements do
            match j.TimingWindows with
            | Some (early, late) ->

                Render.rect_edges
                    (ms_to_x previous_early)
                    this.Bounds.Top
                    (ms_to_x early)
                    this.Bounds.Bottom
                    (j.Color.O4a 100)
                previous_early <- early

                Render.rect_edges
                    (ms_to_x late)
                    this.Bounds.Top
                    (ms_to_x previous_late)
                    this.Bounds.Bottom
                    (j.Color.O4a 100)
                previous_late <- late
            | None -> ()

    member private this.DrawHits() =
        let events = score_info.Scoring.Events
        assert (events.Count > 0)

        let h = 0.5f * this.Bounds.Height
        let width = this.Bounds.Width
        let xscale = (width - 10.0f) / events.[events.Count - 1].Time
        let size = float32 DeviationGraphSettings.hit_size.Value
        let invsize = 1f / size
        let scale = DeviationGraphSettings.scale.Value
        let hitWidth = this.Bounds.Width / float32 MAX_WINDOW * scale / 2f * size
        let bars =
            seq {
                for ev in events do
                    match ev.Action with
                    | Hit e
                    | Hold e when DeviationGraphSettings.column_filter.[ev.Column] && DeviationGraphSettings.show_hits.Value ->
                        yield e.Delta |> float32 |> (*) invsize |> round |> (*) size 
                    | Release e when DeviationGraphSettings.column_filter.[ev.Column] && DeviationGraphSettings.show_releases.Value ->
                        yield e.Delta |> float32 |> (*) invsize |> round |> (*) size 
                    | _ -> ()
            }
            |> Seq.countBy id
            |> Array.ofSeq 

        let biggest_bar =
            bars
            // this crashes if there are no hits btw
            |> Seq.maxBy (fun (_, count) -> count) |> snd

        Text.draw_aligned_b(Style.font, sprintf "Tallest = %u hits" biggest_bar, 25.0f, this.Bounds.CenterX, this.Bounds.Top - 45f, Colors.text, Alignment.CENTER)

        for (ms, count) in bars do
            let clampedX =
                System.Math.Clamp(
                    this.Bounds.CenterX + (float32 ms * this.Bounds.Width / float32 MAX_WINDOW * scale / 2f),
                    this.Bounds.Left,
                    this.Bounds.Right - hitWidth
                )
            let barHeight = if DeviationGraphSettings.sqrt_scale.Value then -sqrt(float32 count  / float32 biggest_bar) * this.Bounds.Height else -(float32 count  / float32 biggest_bar) * this.Bounds.Height

            Render.rect (
                Rect.FromSize(
                    clampedX,
                    this.Bounds.Bottom,
                    hitWidth,
                    barHeight
                )
            ) Colors.white.O4

    member private this.DrawLabels(translucent: bool) =
        let color = if translucent then Colors.white.O4, Color.Transparent else Colors.text

        if expanded && options.DeviationGraphWindowBackground.Value then
            let w = 0.5f * this.Bounds.Width
            let c = this.Bounds.CenterX
            let mutable x2 = c - w - 48.0f
            let mutable x1 = c + w + 24.0f
            let ms_to_x (time: GameplayTime) =
                c - System.Math.Clamp(time * DeviationGraphSettings.scale.Value / MAX_WINDOW, -1.0f, 1.0f) * w

            for j in score_info.Ruleset.Judgements do
                match j.TimingWindows with
                | Some (early, late) ->
                    let early_x = ms_to_x early
                    if early_x - 24.0f > x1 then
                        Text.draw_b(Style.font, sprintf "+%gms" late, 15.0f, early_x, this.Bounds.Top + 5f, color)
                        x1 <- early_x
                    let late_x = ms_to_x late
                    if late_x + 24.0f < x2 then
                        Text.draw_b(Style.font, sprintf "-%gms" early, 15.0f, late_x, this.Bounds.Top + 5f, color)
                        x2 <- late_x
                | None -> ()

        else
            Text.draw_b (
                Style.font,
                sprintf "%s (-%.0fms)" (%"deviation.graph.early") (MAX_WINDOW / DeviationGraphSettings.scale.Value),
                24.0f,
                this.Bounds.Left + 10.0f,
                this.Bounds.Top + 3.0f,
                color
            )
            Text.draw_aligned_b (
                Style.font,
                sprintf "%s (+%.0fms)" (%"deviation.graph.late") (MAX_WINDOW / DeviationGraphSettings.scale.Value),
                24.0f,
                this.Bounds.Right - 10.0f,
                this.Bounds.Top + 3.0f,
                color,
                Alignment.RIGHT
            )

    member private this.Redraw() =
        refresh <- false
        MAX_WINDOW <- score_info.Ruleset.LargestWindow
        let h = 0.5f * this.Bounds.Height
        fbo.Bind true

        this.DrawHits()
        if options.DeviationGraphWindowBackground.Value then
            this.DrawWindows()
        else
            Render.rect this.Bounds Colors.black.O1

        Render.rect_edges
            this.Bounds.Left
            (this.Bounds.Top + h - 2.5f)
            this.Bounds.Right
            (this.Bounds.Top + h + 2.5f)
            Colors.white.O2

        this.DrawLabels true
        fbo.Unbind() 

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if moved then
            this.Refresh()
        if Mouse.hover this.Bounds then
            if Mouse.right_clicked() then
                DeviationGraphSettingsPage(this).Show()
            elif Mouse.left_clicked() then
                expanded <- not expanded
                this.Position <- if expanded then EXPANDED_POSITION else NORMAL_POSITION
                refresh <- true

            let s = Mouse.scroll()

            if s <> 0.0f then
                DeviationGraphSettings.scale.Value <- DeviationGraphSettings.scale.Value + 0.25f * s
                refresh <- true

        for k = 0 to score_info.WithMods.Keys - 1 do
            if DeviationGraphSettings.COLUMN_FILTER_KEYS.[k].Pressed() then
                DeviationGraphSettings.column_filter.[k] <- not DeviationGraphSettings.column_filter.[k]
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

    interface System.IDisposable with
        override this.Dispose() =
            for i = 0 to 9 do DeviationGraphSettings.column_filter.[i] <- true
            (fbo :> System.IDisposable).Dispose()