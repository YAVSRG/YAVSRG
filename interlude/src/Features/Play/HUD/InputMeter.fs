namespace Interlude.Features.Play.HUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skinning.Noteskins
open Interlude.Features.Gameplay
open Interlude.Features.Play

type InputMeter(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let counters = Array.zeroCreate state.Chart.Keys
    let fades = Array.init state.Chart.Keys (fun _ -> Animation.Fade(0.5f))

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        for k = 0 to state.Chart.Keys - 1 do
            fades.[k].Update elapsed_ms
            if Bitmask.has_key k state.Scoring.KeyState then
                fades.[k].Value <- 1.0f

    override this.Draw() =
        let box_height = this.Bounds.Width / float32 state.Chart.Keys

        let mutable box = this.Bounds.SliceBottom(box_height).SliceLeft(box_height).Shrink(Style.PADDING)
        for k = 0 to state.Chart.Keys - 1 do
            Draw.rect box (Colors.white.O2a fades.[k].Alpha)
            box <- box.Translate(box_height, 0.0f)