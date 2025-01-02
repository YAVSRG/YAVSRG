namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.Features.Play
open Interlude.Features.Gameplay

type EarlyLate(config: HudConfig, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let duration = config.EarlyLateMeterDuration * SelectedChart.rate.Value
    let mutable early = false
    let mutable time = -Time.infinity

    let texture = Content.Texture "early-late"

    do
        state.SubscribeEvents(fun ev ->
            let x =
                match ev.Action with
                | Hit e when not e.Missed -> match e.Judgement with Some (j, _) -> ValueSome (j, e.Delta) | None -> ValueNone
                | Hold e when not e.Missed -> match e.Judgement with Some (j, _) -> ValueSome (j, e.Delta) | None -> ValueNone
                | Release e when not e.Missed -> match e.Judgement with Some (j, _) -> ValueSome (j, e.Delta) | None -> ValueNone
                | _ -> ValueNone

            match x with
            | ValueSome (judge, delta) when judge > 0 ->
                early <- delta < 0.0f<ms / rate>
                time <- ev.Time
            | _ -> ()
        )

    override this.Draw() =
        if time > -Time.infinity then

            let time_ago = state.CurrentChartTime() - time

            if time_ago < duration then

                if config.EarlyLateMeterUseTexture then
                    Render.tex_quad 
                        ((Sprite.fill this.Bounds texture).AsQuad)
                        Color.White.AsQuad
                        (Sprite.pick_texture (time_ago / config.EarlyLateMeterFrameTime / SelectedChart.rate.Value |> floor |> int, if early then 0 else 1) texture)
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