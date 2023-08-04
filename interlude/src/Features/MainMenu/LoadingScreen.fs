namespace Interlude.Features.MainMenu

open System
open System.Drawing
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Interlude
open Interlude.UI
open Interlude.Features.Online

// Loading screen

type LoadingScreen() =
    inherit Screen()

    let mutable closing = false
    let audio_fade = Animation.Fade 0.0f
    let animation = Animation.Sequence()

    override this.OnEnter (prev: Screen.Type) =
        Logo.moveCentre()
        Screen.Toolbar.hide()
        match prev with
        | Screen.Type.MainMenu ->
            closing <- true
            audio_fade.Snap()
            animation.Add (Animation.Delay 1000.0)
            animation.Add (Animation.Action (fun () -> audio_fade.Target <- 0.0f))
            animation.Add (Animation.Delay 1200.0)
            animation.Add (Animation.Action (fun () -> Screen.back Transitions.Flags.Default))
        | _ ->
            if Network.target_ip.ToString() <> "0.0.0.0" then Network.connect()
            animation.Add (Animation.Action (fun () -> Content.Sounds.getSound("hello").Play()))
            animation.Add (Animation.Delay 1000.0)
            animation.Add (Animation.Action (fun () -> audio_fade.Target <- 1.0f))
            animation.Add (Animation.Delay 1200.0)
            animation.Add (Animation.Action (fun () -> Screen.change Screen.Type.MainMenu Transitions.Flags.UnderLogo))

    override this.OnExit _ =
        if not closing then Devices.change_volume (Options.options.AudioVolume.Value, Options.options.AudioVolume.Value)

    override this.Update (elapsedTime, bounds) =
        base.Update (elapsedTime, bounds)
        audio_fade.Update elapsedTime
        animation.Update elapsedTime
        Devices.change_volume (Options.options.AudioVolume.Value, Options.options.AudioVolume.Value * float audio_fade.Value)
        
    override this.Draw() =
        let (x, y) = this.Bounds.Center
        Text.drawJust (Style.font, (if closing then "Thank you for playing" else "Loading :)"), 80.f, x, y - 500.0f, Color.White, 0.5f)

    override this.OnBack() = Screen.exit <- true; None