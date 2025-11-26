namespace Interlude.Features.Play.HUD

open System
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Skins.HudLayouts
open Interlude.Features.Gameplay
open Interlude.Features.Play

type ColumnErrorBars(ctx: HudContext) =
    inherit StaticWidget(NodeType.None)

    let hits = Array.init ctx.State.WithColors.Keys (fun _ -> ResizeArray<ErrorBarEvent>())
    let mutable h = 0.0f

    let mutable last_seen_time = -Time.infinity

    let animation_time = ctx.Config.ColumnErrorBarsFadeTime * SelectedChart.rate.Value
    let moving_average_sensitivity = ctx.Config.ColumnErrorBarsMovingAverageSensitivity * 0.5f
    let moving_average_fade_time = float (1.0f - ctx.Config.ColumnErrorBarsMovingAverageSensitivity) * 3000.0 |> min 1500.0 |> max 75.0
    let moving_averages = Array.init ctx.State.WithColors.Keys (fun _ -> Animation.Fade(0.0f, moving_average_fade_time))

    let window_opacity = ctx.Config.ColumnErrorBarsWindowsOpacity * 255.0f |> int |> min 255 |> max 0

    let MAX_WINDOW = ctx.State.Ruleset.LargestWindow

    override this.Init(parent: Widget) =
        if ctx.Config.ColumnErrorBarsMovingAverage then
            ctx.State.Subscribe(fun ev ->
                match ev.Action with
                | Hit e ->
                    if not e.Missed then
                        moving_averages.[ev.Column].Target <-
                            lerp
                                moving_average_sensitivity
                                moving_averages.[ev.Column].Target
                                (e.Delta / MAX_WINDOW * h * 0.5f)
                | Hold e ->
                    if not e.Missed then
                        moving_averages.[ev.Column].Target <-
                            lerp
                                moving_average_sensitivity
                                moving_averages.[ev.Column].Target
                                (e.Delta / MAX_WINDOW * h * 0.5f)
                | Release e ->
                    if not e.Missed then
                        moving_averages.[ev.Column].Target <-
                            lerp
                                moving_average_sensitivity
                                moving_averages.[ev.Column].Target
                                (e.Delta / MAX_WINDOW * h * ctx.Config.ColumnErrorBarsReleasesYScale)
                | GhostTap _
                | DropHold
                | RegrabHold -> ()
            )
            |> ignore
        else
            ctx.State.Subscribe(fun ev ->
                match ev.Action with
                | Hit e ->
                    hits.[ev.Column].Add
                        {
                            Time = ev.Time
                            Position = e.Delta / MAX_WINDOW * h * 0.5f
                            IsRelease = false
                            Judgement = e.Judgement |> Option.map fst
                        }
                | Hold e ->
                    hits.[ev.Column].Add
                        {
                            Time = ev.Time
                            Position = e.Delta / MAX_WINDOW * h * 0.5f
                            IsRelease = false
                            Judgement = e.Judgement |> Option.map fst
                        }
                | Release e ->
                    hits.[ev.Column].Add
                        {
                            Time = ev.Time
                            Position = e.Delta / MAX_WINDOW * h * ctx.Config.ColumnErrorBarsReleasesYScale
                            IsRelease = true
                            Judgement = e.Judgement |> Option.map fst
                        }
                | GhostTap e ->
                    match e.Judgement with
                    | Some (j, _) ->
                        hits.[ev.Column].Add
                            {
                                Time = ev.Time
                                Position = -h * 0.5f
                                IsRelease = false
                                Judgement = Some j
                            }
                    | None -> ()
                | DropHold
                | RegrabHold -> ()
            )
            |> ignore
        base.Init(parent)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        for moving_average in moving_averages do
            moving_average.Update elapsed_ms

        if h = 0.0f || moved then
            h <- this.Bounds.Height

        let now = ctx.State.CurrentChartTime()

        if now < last_seen_time then
            for column in hits do column.Clear()

        last_seen_time <- now

        for column in hits do
            while column.Count > 0 && column.[0].Time + animation_time < now do
                column.RemoveAt(0)

    member this.DrawWindows(opacity: int) =

        let draw (r: int -> float32<ms/rate> -> float32<ms/rate> -> Rect) =
            let mutable previous_early = 0.0f<ms / rate>
            let mutable previous_late = 0.0f<ms / rate>

            for j in ctx.State.Scoring.Ruleset.Judgements do
                match j.TimingWindows with
                | Some (early, late) ->
                    for k = 0 to ctx.State.WithColors.Keys - 1 do
                        Render.rect
                            (r k early previous_early)
                            (j.Color.O4a opacity)
                        Render.rect
                            (r k previous_late late)
                            (j.Color.O4a opacity)
                    previous_early <- early
                    previous_late <- late
                | None -> ()

        let center = this.Bounds.CenterY
        let ms_to_y =
            let h = this.Bounds.Height * 0.5f
            fun time -> center + time / MAX_WINDOW * h
        let r k time1 time2 =
            let left_edge = ctx.Playfield.Bounds.Left + ctx.Playfield.ColumnPositions.[k]
            Rect.FromEdges(left_edge, ms_to_y time1, left_edge + ctx.Playfield.ColumnWidth, ms_to_y time2).SliceX(ctx.Config.ColumnErrorBarsWidth)
        draw r

    override this.Draw() =
        if window_opacity > 0 then
            this.DrawWindows window_opacity

        let bar k p1 p2 =
            let center = this.Bounds.CenterY
            let left_edge = ctx.Playfield.Bounds.Left + ctx.Playfield.ColumnPositions.[k]
            Rect.FromEdges(left_edge, center + p1, left_edge + ctx.Playfield.ColumnWidth, center + p2).SliceX(ctx.Config.ColumnErrorBarsWidth)

        if ctx.Config.ColumnErrorBarsShowGuide then
            for k = 0 to ctx.State.WithColors.Keys - 1 do
            Render.rect
                (bar k (-ctx.Config.ColumnErrorBarsThickness * ctx.Config.ColumnErrorBarsGuideThickness) (ctx.Config.ColumnErrorBarsThickness * ctx.Config.ColumnErrorBarsGuideThickness))
                Color.White.O2

        if ctx.Config.ColumnErrorBarsMovingAverage then
            for k = 0 to ctx.State.WithColors.Keys - 1 do
                let moving_average = moving_averages.[k]
                Render.rect
                    (bar k (moving_average.Value - ctx.Config.ColumnErrorBarsThickness) (moving_average.Value + ctx.Config.ColumnErrorBarsThickness))
                    ctx.Config.ColumnErrorBarsMovingAverageColor
        else
            let now = ctx.State.CurrentChartTime()
            for k = 0 to ctx.State.WithColors.Keys - 1 do
                let hits = hits.[k]
                for hit in hits do
                    let rect = bar k (hit.Position - ctx.Config.ColumnErrorBarsThickness) (hit.Position + ctx.Config.ColumnErrorBarsThickness)
                    let color =
                        match hit.Judgement with
                        | None ->
                            Color.FromArgb(
                                Math.Clamp(127 - int (127.0f * (now - hit.Time) / animation_time), 0, 127),
                                Color.Silver
                            )
                        | Some j ->
                            Color.FromArgb(
                                Math.Clamp(255 - int (255.0f * (now - hit.Time) / animation_time), 0, 255),
                                ctx.State.Ruleset.JudgementColor j
                            )

                    if ctx.Config.ColumnErrorBarsShowNonJudgements || hit.Judgement.IsSome then
                        Render.rect
                            (
                                if hit.IsRelease then
                                    rect.ExpandPercentX(ctx.Config.ColumnErrorBarsReleasesXScale - 1.0f)
                                else
                                    rect
                            )
                            color