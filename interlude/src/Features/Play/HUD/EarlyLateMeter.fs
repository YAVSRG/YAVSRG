namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Content.Noteskins
open Interlude.Content
open Interlude.Features.Play
open Interlude.Features.Gameplay

type EarlyLateMeter(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let duration = noteskin_options.EarlyLateMeterDuration * rate.Value * 1.0f<ms>
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

                if noteskin_options.EarlyLateMeterUseTexture then
                    Draw.quad 
                        ((Sprite.fill this.Bounds texture).AsQuad)
                        Color.White.AsQuad
                        (Sprite.pick_texture (float32 time_ago / noteskin_options.EarlyLateMeterFrameTime |> floor |> int, if early then 0 else 1) texture)
                else
                    Text.fill (
                        Style.font,
                        (if early then
                             noteskin_options.EarlyLateMeterEarlyText
                         else
                             noteskin_options.EarlyLateMeterLateText),
                        this.Bounds,
                        (if early then
                             noteskin_options.EarlyLateMeterEarlyColor
                         else
                             noteskin_options.EarlyLateMeterLateColor),
                        Alignment.CENTER
                    )