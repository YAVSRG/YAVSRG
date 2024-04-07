namespace Interlude.Features.Play.HUD

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Content.Noteskins
open Interlude.Content
open Interlude.Features.Play
open Interlude.Features.Gameplay

type JudgementMeter(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let mutable tier = 0
    let mutable time = -Time.infinity

    let texture = Content.Texture "judgements"
    let display = noteskin_options.GetJudgementMeterDisplay state.Ruleset
    let animated = not noteskin_options.JudgementMeterUseTexture || noteskin_options.JudgementMeterUseBuiltInAnimation
    let duration = 
        (
            if animated then 
                noteskin_options.JudgementMeterDuration
            else
                noteskin_options.JudgementMeterFrameTime * float32 texture.Columns
        ) * rate.Value * 1.0f<ms>

    do
        state.SubscribeToHits(fun ev ->
            let (judge, _) =
                match ev.Guts with
                | Hit e -> (e.Judgement, e.Delta)
                | Release e -> (e.Judgement, e.Delta)

            if
                judge.IsSome
                && (not user_options.JudgementMeterIgnorePerfect || judge.Value > 0)
            then
                let j = judge.Value in

                if
                    not user_options.JudgementMeterPrioritiseLower
                    || j >= tier
                    || ev.Time - duration > time
                    || ev.Time < time
                then
                    tier <- j
                    time <- ev.Time
        )

    override this.Draw() =
        if time > -Time.infinity then

            let time_ago = state.CurrentChartTime() - time
            let percent = Math.Clamp(time_ago / duration, 0.0f, 1.0f)

            if percent < 1.0f then
            
                let pop = if animated then max (1f + percent - 5.0f * percent * percent) (1f - 128f * MathF.Pow(percent - 0.5f, 8.0f)) else 1.0f
                let alpha = Math.Clamp(255.0f * (((pop - 1.0f) * 2.0f) + 1.0f) |> int, 0, 255)
                let bounds = this.Bounds.Expand((pop - 1.0f) * this.Bounds.Width, (pop - 1.0f) * this.Bounds.Height)

                match display.[tier] with
                | JudgementDisplayType.Name ->
                    Text.fill (
                        Style.font,
                        state.Ruleset.JudgementName tier,
                        bounds,
                        state.Ruleset.JudgementColor(tier).O4a alpha,
                        Alignment.CENTER
                    )
                | JudgementDisplayType.Texture y ->
                    Draw.quad 
                        ((Sprite.fill bounds texture).AsQuad)
                        (Quad.color (Color.White.O4a alpha))
                        (Sprite.pick_texture (float32 time_ago / noteskin_options.JudgementMeterFrameTime |> floor |> int, y) texture)