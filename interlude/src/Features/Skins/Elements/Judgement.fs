namespace Interlude.Features.Play.HUD

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.Features.Play
open Interlude.Features.Gameplay

type Judgement(config: HudConfig, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let mutable tier = 0
    let mutable time = -Time.infinity

    let texture = Content.Texture "judgements"
    let display = config.GetJudgementMeterDisplay state.Ruleset
    let animated = not config.JudgementMeterUseTexture || config.JudgementMeterUseBuiltInAnimation
    let duration =
        (
            if animated then
                config.JudgementMeterDuration
            else
                config.JudgementMeterFrameTime * float32 texture.Columns
        ) * SelectedChart.rate.Value

    do
        state.SubscribeEvents(fun ev ->
            let judge = ev.Action.Judgement |> Option.map fst

            match judge with
            | Some j when not config.JudgementMeterIgnorePerfect || j > 0 ->
                if
                    not config.JudgementMeterPrioritiseLower
                    || j >= tier
                    || ev.Time - duration > time
                    || ev.Time < time
                then
                    tier <- j
                    time <- ev.Time
            | _ -> ()
        )

    override this.Draw() =
        if time > -Time.infinity then

            let time_ago = state.CurrentChartTime() - time
            let percent = Math.Clamp(time_ago / duration, 0.0f, 1.0f)

            if percent < 1.0f then

                let pop = if animated then max (percent - 5.0f * percent * percent) (- 128f * MathF.Pow(percent - 0.5f, 8.0f)) else 0.0f
                let alpha = Math.Clamp(255.0f * ((pop * 2.0f) + 1.0f) |> int, 0, 255)
                let bounds = this.Bounds.Expand(pop * this.Bounds.Width, pop * this.Bounds.Height)

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
                    Render.tex_quad
                        ((Sprite.fill bounds texture).AsQuad)
                        (Color.White.O4a alpha).AsQuad
                        (Sprite.pick_texture (time_ago / config.JudgementMeterFrameTime / SelectedChart.rate.Value |> floor |> int, y) texture)