namespace Interlude.UI.Menu

open Percyqaz.Common
open Interlude.Options
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Interlude.UI

type Volume() =
    inherit StaticWidget(NodeType.None)
    let fade = Animation.Fade 0.0f
    let slider = Animation.Fade 0.0f

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)

        fade.Update elapsedTime
        slider.Update elapsedTime

        if (!|"volume").Pressed() then
            fade.Target <- 1.0f
            Setting.app ((+) (float (Mouse.scroll()) * 0.02)) options.AudioVolume
            Devices.change_volume (options.AudioVolume.Value, options.AudioVolume.Value)
            slider.Target <- float32 options.AudioVolume.Value
        else fade.Target <- 0.0f

    override this.Draw() =
        let r = this.Bounds.SliceBottom 5.0f
        Draw.rect r (Style.color(fade.Alpha, 0.4f, 0.0f))
        Draw.rect (r.SliceLeft(slider.Value * r.Width)) (Style.color(fade.Alpha, 1.0f, 0.0f))
