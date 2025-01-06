namespace Interlude.UI

open Percyqaz.Common
open Interlude.Options
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI

type Volume() =
    inherit StaticWidget(NodeType.None)
    let fade = Animation.Fade 0.0f
    let slider = Animation.Fade 0.0f

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        fade.Update elapsed_ms
        slider.Update elapsed_ms

        if Screen.current_type = Screen.Type.EditHud && not (Dialog.exists()) then
            fade.Target <- 0.0f

        elif (%%"volume_up").Pressed() then
            fade.Target <- 1.0f
            Setting.app ((+) (0.0003 * elapsed_ms)) options.AudioVolume
            Audio.change_volume (options.AudioVolume.Value, options.AudioVolume.Value)
            slider.Target <- float32 options.AudioVolume.Value

        elif (%%"volume_down").Pressed() then
            fade.Target <- 1.0f
            Setting.app ((+) (-0.0003 * elapsed_ms)) options.AudioVolume
            Audio.change_volume (options.AudioVolume.Value, options.AudioVolume.Value)
            slider.Target <- float32 options.AudioVolume.Value

        elif (%%"volume").Pressed() then
            fade.Target <- 1.0f
            Setting.app ((+) (float (Mouse.scroll ()) * 0.02)) options.AudioVolume

            Audio.change_volume (options.AudioVolume.Value, options.AudioVolume.Value)
            slider.Target <- float32 options.AudioVolume.Value

        else
            fade.Target <- 0.0f

    override this.Draw() =
        let r = this.Bounds.SliceB 5.0f
        Render.rect r (Palette.color (fade.Alpha, 0.4f, 0.0f))
        Render.rect (r.SliceL(slider.Value * r.Width)) (Palette.color (fade.Alpha, 1.0f, 0.0f))