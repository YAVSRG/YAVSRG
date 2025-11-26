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

type ErrorBar(ctx: HudContext) =
    inherit StaticWidget(NodeType.None)
    let hits = ResizeArray<ErrorBarEvent>()
    let mutable w = 0.0f

    let mutable last_seen_time = -Time.infinity

    let ln_mult =
        if ctx.Config.TimingDisplayHalfScaleReleases then
            0.5f
        else
            1.0f

    let animation_time = ctx.Config.TimingDisplayFadeTime * SelectedChart.rate.Value
    let moving_average_sensitivity = ctx.Config.TimingDisplayMovingAverageSensitivity * 0.5f
    let moving_average_fade_time = float (1.0f - ctx.Config.TimingDisplayMovingAverageSensitivity) * 3000.0 |> min 1500.0 |> max 75.0
    let moving_average = Animation.Fade(0.0f, moving_average_fade_time)

    let window_opacity = ctx.Config.TimingDisplayWindowsOpacity * 255.0f |> int |> min 255 |> max 0

    let MAX_WINDOW = ctx.State.Ruleset.LargestWindow

    override this.Init(parent: Widget) =
        if ctx.Config.TimingDisplayMovingAverageType <> ErrorBarMovingAverageType.None then
            ctx.State.Subscribe(fun ev ->
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
            |> ignore
        if ctx.Config.TimingDisplayMovingAverageType <> ErrorBarMovingAverageType.ReplaceBars then
            ctx.State.Subscribe(fun ev ->
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
            |> ignore
        base.Init(parent)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        moving_average.Update elapsed_ms

        if w = 0.0f || moved then
            w <- this.Bounds.Width

        let now = ctx.State.CurrentChartTime()

        if now < last_seen_time then
            hits.Clear()

        last_seen_time <- now

        while hits.Count > 0 && hits.[0].Time + animation_time < now do
            hits.RemoveAt(0)

    member this.DrawWindows(opacity: int) =

        let draw (r: float32<ms/rate> -> float32<ms/rate> -> Rect) =
            let mutable previous_early = 0.0f<ms / rate>
            let mutable previous_late = 0.0f<ms / rate>

            for j in ctx.State.Scoring.Ruleset.Judgements do
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

        let center = this.Bounds.CenterX
        let ms_to_x =
            let w = this.Bounds.Width * 0.5f
            fun time -> center + time / MAX_WINDOW * w
        let r time1 time2 = Rect.FromEdges(ms_to_x time1, this.Bounds.Top, ms_to_x time2, this.Bounds.Bottom)
        draw r

    override this.Draw() =
        if window_opacity > 0 then
            this.DrawWindows window_opacity

        let bar p1 p2 =
            let center = this.Bounds.CenterX
            Rect.FromEdges(center + p1, this.Bounds.Top, center + p2, this.Bounds.Bottom)

        if ctx.Config.TimingDisplayShowGuide then
            Render.rect
                (bar (-ctx.Config.TimingDisplayThickness * ctx.Config.TimingDisplayGuideThickness) (ctx.Config.TimingDisplayThickness * ctx.Config.TimingDisplayGuideThickness))
                Color.White

        let now = ctx.State.CurrentChartTime()

        match ctx.Config.TimingDisplayMovingAverageType with
        | ErrorBarMovingAverageType.ReplaceBars ->
            Render.rect
                (bar (moving_average.Value - ctx.Config.TimingDisplayThickness) (moving_average.Value + ctx.Config.TimingDisplayThickness))
                ctx.Config.TimingDisplayMovingAverageColor
        | ErrorBarMovingAverageType.Arrow ->
            let center = this.Bounds.CenterX
            let arrow_height = this.Bounds.Height * 0.5f
            let arrow =
                Quad.from_points(
                    (center + moving_average.Value, this.Bounds.Top - 10.0f),
                    (center + moving_average.Value - arrow_height, this.Bounds.Top - 10.0f - arrow_height),
                    (center + moving_average.Value + arrow_height, this.Bounds.Top - 10.0f - arrow_height),
                    (center + moving_average.Value, this.Bounds.Top - 10.0f)
                )
            Render.quad arrow ctx.Config.TimingDisplayMovingAverageColor
        | _ -> ()

        for hit in hits do
            let rect = bar (hit.Position - ctx.Config.TimingDisplayThickness) (hit.Position + ctx.Config.TimingDisplayThickness)
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

            if ctx.Config.TimingDisplayShowNonJudgements || hit.Judgement.IsSome then
                Render.rect
                    (
                        if hit.IsRelease then
                            rect.ExpandY(ctx.Config.TimingDisplayReleasesExtraHeight)
                        else
                            rect
                    )
                    color