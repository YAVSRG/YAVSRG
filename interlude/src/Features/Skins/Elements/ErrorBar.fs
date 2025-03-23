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

[<Struct>]
type private ErrorBarEvent =
    {
        Time: Time
        Position: float32
        IsRelease: bool
        Judgement: int option
    }

type ErrorBar(config: HudConfig, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let hits = ResizeArray<ErrorBarEvent>()
    let mutable w = 0.0f

    let mutable last_seen_time = -Time.infinity

    let ln_mult =
        if config.TimingDisplayHalfScaleReleases then
            0.5f
        else
            1.0f

    let moving_average_sensitivity = config.TimingDisplayMovingAverageSensitivity * 0.5f
    let animation_time = config.TimingDisplayFadeTime * SelectedChart.rate.Value
    let moving_average = Animation.Fade(0.0f)

    let window_opacity = config.TimingDisplayWindowsOpacity * 255.0f |> int |> min 255 |> max 0

    let MAX_WINDOW = state.Ruleset.LargestWindow
    let IS_ROTATED = config.TimingDisplayRotation <> ErrorBarRotation.Normal

    do
        if config.TimingDisplayMovingAverageType <> ErrorBarMovingAverageType.None then
            state.SubscribeEvents(fun ev ->
                match ev.Action with
                | Hit e ->
                    if not e.Missed then
                        moving_average.Target <-
                            lerp
                                moving_average_sensitivity
                                moving_average.Target
                                (e.Delta / MAX_WINDOW * w * 0.5f)
                | Hold e ->
                    if not e.Missed then
                        moving_average.Target <-
                            lerp
                                moving_average_sensitivity
                                moving_average.Target
                                (e.Delta / MAX_WINDOW * w * 0.5f)
                | Release e ->
                    if not e.Missed then
                        moving_average.Target <-
                            lerp
                                moving_average_sensitivity
                                moving_average.Target
                                (e.Delta / MAX_WINDOW * w * ln_mult)
                | GhostTap _
                | DropHold
                | RegrabHold -> ()
            )
        if config.TimingDisplayMovingAverageType <> ErrorBarMovingAverageType.ReplaceBars then
            state.SubscribeEvents(fun ev ->
                match ev.Action with
                | Hit e ->
                    hits.Add
                        {
                            Time = ev.Time
                            Position = e.Delta / MAX_WINDOW * w * 0.5f
                            IsRelease = false
                            Judgement = e.Judgement |> Option.map fst
                        }
                | Hold e ->
                    hits.Add
                        {
                            Time = ev.Time
                            Position = e.Delta / MAX_WINDOW * w * 0.5f
                            IsRelease = false
                            Judgement = e.Judgement |> Option.map fst
                        }
                | Release e ->
                    hits.Add
                        {
                            Time = ev.Time
                            Position = e.Delta / MAX_WINDOW * w * ln_mult
                            IsRelease = true
                            Judgement = e.Judgement |> Option.map fst
                        }
                | GhostTap e ->
                    match e.Judgement with
                    | Some (j, _) ->
                        hits.Add
                            {
                                Time = ev.Time
                                Position = -w * 0.5f
                                IsRelease = false
                                Judgement = Some j
                            }
                    | None -> ()
                | DropHold
                | RegrabHold -> ()
            )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        moving_average.Update elapsed_ms

        if w = 0.0f || moved then
            w <- if IS_ROTATED then this.Bounds.Height else this.Bounds.Width

        let now = state.CurrentChartTime()

        if now < last_seen_time then
            hits.Clear()

        last_seen_time <- now

        while hits.Count > 0 && hits.[0].Time + animation_time < now do
            hits.RemoveAt(0)

    member this.DrawWindows(opacity: int) =

        let draw (r: float32<ms/rate> -> float32<ms/rate> -> Rect) =
            let mutable previous_early = 0.0f<ms / rate>
            let mutable previous_late = 0.0f<ms / rate>

            for j in state.Scoring.Ruleset.Judgements do
                match j.TimingWindows with
                | Some (early, late) ->
                    Render.rect
                        (r early previous_early)
                        (j.Color.O4a opacity)
                    previous_early <- early
                    Render.rect
                        (r previous_late late)
                        (j.Color.O4a opacity)
                    previous_late <- late
                | None -> ()

        match config.TimingDisplayRotation with
        | ErrorBarRotation.Clockwise ->
            let center = this.Bounds.CenterY
            let ms_to_y =
                let h = this.Bounds.Height * 0.5f
                fun time -> center + time / MAX_WINDOW * h
            let r time1 time2 = Rect.FromEdges(this.Bounds.Left, ms_to_y time1, this.Bounds.Right, ms_to_y time2)
            draw r
        | ErrorBarRotation.Anticlockwise ->
            let center = this.Bounds.CenterY
            let ms_to_y =
                let h = this.Bounds.Height * 0.5f
                fun time -> center - time / MAX_WINDOW * h
            let r time1 time2 = Rect.FromEdges(this.Bounds.Left, ms_to_y time1, this.Bounds.Right, ms_to_y time2)
            draw r
        | _ ->
            let center = this.Bounds.CenterX
            let ms_to_x =
                let w = this.Bounds.Width * 0.5f
                fun time -> center + time / MAX_WINDOW * w
            let r time1 time2 = Rect.FromEdges(ms_to_x time1, this.Bounds.Top, ms_to_x time2, this.Bounds.Bottom)
            draw r

    override this.Draw() =
        if window_opacity > 0 then
            this.DrawWindows window_opacity

        let r =
            match config.TimingDisplayRotation with
            | ErrorBarRotation.Clockwise ->
                fun p1 p2 ->
                let center = this.Bounds.CenterY
                Rect.FromEdges(this.Bounds.Left, center + p1, this.Bounds.Right, center + p2)
            | ErrorBarRotation.Anticlockwise ->
                fun p1 p2 ->
                let center = this.Bounds.CenterY
                Rect.FromEdges(this.Bounds.Left, center - p1, this.Bounds.Right, center - p2)
            | _ ->
                fun p1 p2 ->
                let center = this.Bounds.CenterX
                Rect.FromEdges(center + p1, this.Bounds.Top, center + p2, this.Bounds.Bottom)

        if config.TimingDisplayShowGuide then
            Render.rect
                (r (-config.TimingDisplayThickness * config.TimingDisplayGuideThickness) (config.TimingDisplayThickness * config.TimingDisplayGuideThickness))
                Color.White

        let now = state.CurrentChartTime()

        match config.TimingDisplayMovingAverageType with
        | ErrorBarMovingAverageType.ReplaceBars ->
            Render.rect
                (r (moving_average.Value - config.TimingDisplayThickness) (moving_average.Value + config.TimingDisplayThickness))
                config.TimingDisplayMovingAverageColor
        | ErrorBarMovingAverageType.Arrow ->
            let quad =
                match config.TimingDisplayRotation with
                | ErrorBarRotation.Clockwise ->
                    let center = this.Bounds.CenterY
                    let arrow_height = this.Bounds.Width * 0.5f
                    Quad.from_points(
                        (this.Bounds.Right + 10.0f, center + moving_average.Value),
                        (this.Bounds.Right + 10.0f + arrow_height, center + moving_average.Value - arrow_height),
                        (this.Bounds.Right + 10.0f + arrow_height, center + moving_average.Value + arrow_height),
                        (this.Bounds.Right + 10.0f, center + moving_average.Value)
                    )
                | ErrorBarRotation.Anticlockwise ->
                    let center = this.Bounds.CenterY
                    let arrow_height = this.Bounds.Width * 0.5f
                    Quad.from_points(
                        (this.Bounds.Left - 10.0f, center - moving_average.Value),
                        (this.Bounds.Left - 10.0f - arrow_height, center - moving_average.Value + arrow_height),
                        (this.Bounds.Left - 10.0f - arrow_height, center - moving_average.Value - arrow_height),
                        (this.Bounds.Left - 10.0f, center - moving_average.Value)
                    )
                | _ ->
                    let center = this.Bounds.CenterX
                    let arrow_height = this.Bounds.Height * 0.5f
                    Quad.from_points(
                        (center + moving_average.Value, this.Bounds.Top - 10.0f),
                        (center + moving_average.Value - arrow_height, this.Bounds.Top - 10.0f - arrow_height),
                        (center + moving_average.Value + arrow_height, this.Bounds.Top - 10.0f - arrow_height),
                        (center + moving_average.Value, this.Bounds.Top - 10.0f)
                    )
            Render.quad quad config.TimingDisplayMovingAverageColor
        | _ -> ()

        for hit in hits do
            let rect = r (hit.Position - config.TimingDisplayThickness) (hit.Position + config.TimingDisplayThickness)
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
                        state.Ruleset.JudgementColor j
                    )

            if config.TimingDisplayShowNonJudgements || hit.Judgement.IsSome then
                Render.rect
                    (
                        if hit.IsRelease then
                            if IS_ROTATED then
                                rect.ExpandX(config.TimingDisplayReleasesExtraHeight)
                            else
                                rect.ExpandY(config.TimingDisplayReleasesExtraHeight)
                        else
                            rect
                    )
                    color