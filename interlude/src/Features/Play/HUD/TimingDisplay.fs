namespace Interlude.Features.Play.HUD

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Skinning.Noteskins
open Interlude.Features
open Interlude.Features.Play

[<Struct>]
type private TimingDisplayHit =
    {
        Time: Time
        Position: float32
        IsRelease: bool
        Judgement: JudgementId option
    }

type TimingDisplay(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let hits = ResizeArray<TimingDisplayHit>()
    let mutable w = 0.0f

    let mutable last_seen_time = -Time.infinity

    let ln_mult =
        if user_options.TimingDisplayHalfScaleReleases then
            0.5f
        else
            1.0f

    let animation_time = user_options.TimingDisplayFadeTime * Gameplay.rate.Value

    do
        state.SubscribeToHits(fun ev ->
            match ev.Guts with
            | Hit e ->
                hits.Add
                    {
                        Time = ev.Time
                        Position = e.Delta / state.Scoring.MissWindow * w * 0.5f
                        IsRelease = false
                        Judgement = e.Judgement
                    }
            | Release e ->
                hits.Add
                    {
                        Time = ev.Time
                        Position = e.Delta / state.Scoring.MissWindow * w * ln_mult
                        IsRelease = true
                        Judgement = e.Judgement
                    }
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if w = 0.0f || moved then
            w <- this.Bounds.Width

        let now = state.CurrentChartTime()

        if now < last_seen_time then
            hits.Clear()

        last_seen_time <- now

        while hits.Count > 0 && hits.[0].Time + animation_time * 1.0f<ms> < now do
            hits.RemoveAt(0)

    override this.Draw() =
        let centre = this.Bounds.CenterX

        if user_options.TimingDisplayShowGuide then
            Draw.rect
                (Rect.Create(
                    centre - user_options.TimingDisplayThickness,
                    this.Bounds.Top,
                    centre + user_options.TimingDisplayThickness,
                    this.Bounds.Bottom
                ))
                Color.White

        let now = state.CurrentChartTime()

        for hit in hits do
            let r =
                Rect.Create(
                    centre + hit.Position - user_options.TimingDisplayThickness,
                    this.Bounds.Top,
                    centre + hit.Position + user_options.TimingDisplayThickness,
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

            if user_options.TimingDisplayShowNonJudgements || hit.Judgement.IsSome then
                Draw.rect
                    (if hit.IsRelease then
                         r.Expand(0.0f, user_options.TimingDisplayReleasesExtraHeight)
                     else
                         r)
                    c