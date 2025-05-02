namespace Interlude.UI

open Percyqaz.Common
open Interlude.Options
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI

type VolumeSlider() =
    inherit StaticWidget(NodeType.None)
    let fade = Animation.Fade 0.0f
    let slider = Animation.Fade 0.0f

    let volume = options.AudioVolume |> Setting.trigger (fun v -> Audio.change_volume(v, v))

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        fade.Update elapsed_ms
        slider.Update elapsed_ms

        if Screen.current_type = ScreenType.EditHud && not (Dialog.exists()) then
            fade.Target <- 0.0f

        elif (%%"volume_up").Held() then
            fade.Target <- 1.0f
            Setting.app ((+) (0.0003 * elapsed_ms)) volume
            slider.Target <- float32 volume.Value

        elif (%%"volume_down").Held() then
            fade.Target <- 1.0f
            Setting.app ((+) (-0.0003 * elapsed_ms)) volume
            slider.Target <- float32 volume.Value

        elif (%%"volume").Held() then
            fade.Target <- 1.0f
            Setting.app ((+) (float (Mouse.scroll ()) * 0.02)) volume

            slider.Target <- float32 volume.Value

        else
            fade.Target <- 0.0f

    override this.Draw() =
        let r = this.Bounds.SliceB 5.0f
        let a = fade.Alpha
        if a > 0 then
            Render.rect r (Palette.color (a, 0.2f, 0.0f))
            Render.rect (r.SliceL(slider.Value * r.Width)) (Palette.color (a, 1.0f, 0.0f))
            let volume_box = r.SliceX(220.0f).BorderT(50.0f).TranslateY(-5.0f)
            Render.rect volume_box (Colors.shadow_1.O1a (a * 3))
            let icon =
                if volume.Value <= 0.005 then Icons.VOLUME_X
                elif volume.Value < 0.05 then Icons.VOLUME
                elif volume.Value < 0.4 then Icons.VOLUME_1
                else Icons.VOLUME_2
            Text.fill_b (Style.font, sprintf "%s Volume: %.0f%%" icon (volume.Value * 100.0), volume_box.Shrink(10.0f, 5.0f), (Colors.white.O4a a, Colors.shadow_2.O4a a), Alignment.CENTER)