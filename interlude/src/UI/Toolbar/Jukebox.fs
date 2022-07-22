namespace Interlude.UI.Toolbar

open Percyqaz.Common
open Interlude.Options
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Interlude.UI

type Jukebox() as this =
    inherit Widget1()
    // todo: right click to seek/tools to pause and play music
    let fade = Animation.Fade 0.0f
    let slider = Animation.Fade 0.0f
    do
        this.Animation.Add fade
        this.Animation.Add slider

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if (!|"volume").Pressed() then
            fade.Target <- 1.0f
            Setting.app ((+) (float (Mouse.scroll()) * 0.02)) options.AudioVolume
            Devices.changeVolume options.AudioVolume.Value
            slider.Target <- float32 options.AudioVolume.Value
        else fade.Target <- 0.0f

    override this.Draw() =
        let r = this.Bounds.SliceBottom 5.0f
        Draw.rect r (Style.accentShade(fade.Alpha, 0.4f, 0.0f))
        Draw.rect (r.SliceLeft(slider.Value * r.Width)) (Style.accentShade(fade.Alpha, 1.0f, 0.0f))
