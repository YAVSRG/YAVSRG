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
type private TimingDisplayHit =
    {
        Time: Time
        Position: float32
        IsRelease: bool
        Judgement: int option
    }

type TimingDisplay(config: HudConfig, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let hits = ResizeArray<TimingDisplayHit>()
    let mutable w = 0.0f

    let mutable last_seen_time = -Time.infinity

    let ln_mult =
        if config.TimingDisplayHalfScaleReleases then
            0.5f
        else
            1.0f

    let animation_time = config.TimingDisplayFadeTime * SelectedChart.rate.Value
    let moving_average = Animation.Fade(0.0f)

    let window_opacity = config.TimingDisplayWindowsOpacity * 255.0f |> int |> min 255 |> max 0

    let MAX_WINDOW = state.Ruleset.LargestWindow

    do
        if config.TimingDisplayMovingAverageType <> TimingDisplayMovingAverageType.None then
            state.SubscribeEvents(fun ev ->
                match ev.Action with
                | Hit e ->
                    if not e.Missed then
                        moving_average.Target <- 
                            lerp 
                                config.TimingDisplayMovingAverageSensitivity
                                moving_average.Target 
                                (e.Delta / MAX_WINDOW * w * 0.5f)
                | Hold e ->
                    if not e.Missed then
                        moving_average.Target <- 
                            lerp 
                                config.TimingDisplayMovingAverageSensitivity
                                moving_average.Target 
                                (e.Delta / MAX_WINDOW * w * 0.5f)
                | Release e ->
                    if not e.Missed then
                        moving_average.Target <- 
                            lerp 
                                config.TimingDisplayMovingAverageSensitivity
                                moving_average.Target 
                                (e.Delta / MAX_WINDOW * w * ln_mult)
                | GhostTap _
                | DropHold
                | RegrabHold -> ()
            )
        if config.TimingDisplayMovingAverageType <> TimingDisplayMovingAverageType.ReplaceBars then
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
            w <- this.Bounds.Width

        let now = state.CurrentChartTime()

        if now < last_seen_time then
            hits.Clear()

        last_seen_time <- now

        while hits.Count > 0 && hits.[0].Time + animation_time < now do
            hits.RemoveAt(0)

    member this.DrawWindows(opacity: int) =
        let centre = this.Bounds.CenterX

        let ms_to_x =
            let w = this.Bounds.Width * 0.5f
            fun time -> centre + time / MAX_WINDOW * w

        let mutable previous_early = 0.0f<ms / rate>
        let mutable previous_late = 0.0f<ms / rate>

        for j in state.Scoring.Ruleset.Judgements do
            match j.TimingWindows with
            | Some (early, late) ->
                Draw.rect 
                    (Rect.Create(ms_to_x early, this.Bounds.Top, ms_to_x previous_early, this.Bounds.Bottom))
                    (j.Color.O4a opacity)
                previous_early <- early
                Draw.rect 
                    (Rect.Create(ms_to_x previous_late, this.Bounds.Top, ms_to_x late, this.Bounds.Bottom))
                    (j.Color.O4a opacity)
                previous_late <- late
            | None -> ()

    override this.Draw() =
        if window_opacity > 0 then
            this.DrawWindows window_opacity

        let centre = this.Bounds.CenterX

        if config.TimingDisplayShowGuide then
            Draw.rect
                (Rect.Create(
                    centre - config.TimingDisplayThickness * config.TimingDisplayGuideThickness,
                    this.Bounds.Top,
                    centre + config.TimingDisplayThickness * config.TimingDisplayGuideThickness,
                    this.Bounds.Bottom
                ))
                Color.White

        let now = state.CurrentChartTime()

        match config.TimingDisplayMovingAverageType with
        | TimingDisplayMovingAverageType.ReplaceBars ->
            let r = 
                Rect.Create(
                    centre + moving_average.Value - config.TimingDisplayThickness,
                    this.Bounds.Top,
                    centre + moving_average.Value + config.TimingDisplayThickness,
                    this.Bounds.Bottom
                )
            Draw.rect r config.TimingDisplayMovingAverageColor
        | TimingDisplayMovingAverageType.Arrow ->
            let arrow_height = this.Bounds.Height * 0.5f
            Draw.untextured_quad 
                (
                    Quad.createv 
                        (centre + moving_average.Value, this.Bounds.Top - 10.0f)
                        (centre + moving_average.Value - arrow_height, this.Bounds.Top - 10.0f - arrow_height)
                        (centre + moving_average.Value + arrow_height, this.Bounds.Top - 10.0f - arrow_height)
                        (centre + moving_average.Value, this.Bounds.Top - 10.0f)
                )
                config.TimingDisplayMovingAverageColor.AsQuad
        | _ -> ()

        for hit in hits do
            let r =
                Rect.Create(
                    centre + hit.Position - config.TimingDisplayThickness,
                    this.Bounds.Top,
                    centre + hit.Position + config.TimingDisplayThickness,
                    this.Bounds.Bottom
                )

            let c =
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
                Draw.rect
                    (if hit.IsRelease then
                         r.Expand(0.0f, config.TimingDisplayReleasesExtraHeight)
                     else
                         r)
                    c