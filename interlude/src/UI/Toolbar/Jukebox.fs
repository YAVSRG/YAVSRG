namespace Interlude.UI.Toolbar

open Percyqaz.Common
open Interlude
open Interlude.Options
open Interlude.Graphics
open Interlude.UI
open Interlude.UI.Animation
open Interlude.Input

type Jukebox() as this =
    inherit Widget()
    //todo: right click to seek/tools to pause and play music
    let fade = new AnimationFade 0.0f
    let slider = new AnimationFade 0.0f
    do
        this.Animation.Add fade
        this.Animation.Add slider

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if (!|Hotkey.Volume).Pressed() then
            fade.Target <- 1.0f
            Setting.app ((+) (float (Mouse.Scroll()) * 0.02)) options.AudioVolume
            Audio.changeVolume options.AudioVolume.Value
            slider.Target <- float32 options.AudioVolume.Value
        else fade.Target <- 0.0f

    override this.Draw() =
        let r = this.Bounds.SliceBottom 5.0f
        Draw.rect r (Style.accentShade(int (255.0f * fade.Value), 0.4f, 0.0f)) Sprite.Default
        Draw.rect (r.SliceLeft(slider.Value * r.Width)) (Style.accentShade(int (255.0f * fade.Value), 1.0f, 0.0f)) Sprite.Default
