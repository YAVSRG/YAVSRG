namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Skinning.HudLayouts
open Interlude.Content
open Interlude.Features.Play
open Interlude.Features.Gameplay

type EarlyLateMeter(config: HudConfig, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let duration = config.EarlyLateMeterDuration * SelectedChart.rate.Value * 1.0f<ms>
    let mutable early = false
    let mutable time = -Time.infinity

    let texture = Content.Texture "early-late"

    do
        state.SubscribeToHits(fun ev ->
            let (judge, delta) =
                match ev.Guts with
                | Hit e -> (e.Judgement, e.Delta)
                | Release e -> (e.Judgement, e.Delta)

            if judge.IsSome && judge.Value > 0 then
                early <- delta < 0.0f<ms>
                time <- ev.Time
        )

    override this.Draw() =
        if time > -Time.infinity then

            let time_ago = state.CurrentChartTime() - time

            if time_ago < duration then

                if config.EarlyLateMeterUseTexture then
                    Draw.quad 
                        ((Sprite.fill this.Bounds texture).AsQuad)
                        Color.White.AsQuad
                        (Sprite.pick_texture (float32 time_ago / config.EarlyLateMeterFrameTime |> floor |> int, if early then 0 else 1) texture)
                else
                    Text.fill (
                        Style.font,
                        (if early then
                             config.EarlyLateMeterEarlyText
                         else
                             config.EarlyLateMeterLateText),
                        this.Bounds,
                        (if early then
                             config.EarlyLateMeterEarlyColor
                         else
                             config.EarlyLateMeterLateColor),
                        Alignment.CENTER
                    )