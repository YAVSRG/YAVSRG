namespace Interlude.Features.MainMenu

open System
open System.Drawing
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Interlude
open Interlude.UI

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
            animation.Add (Animation.Action (fun () -> SoundEffect.play (Content.Sounds.getSound "hello") Options.options.AudioVolume.Value))
            animation.Add (Animation.Delay 1000.0)
            animation.Add (Animation.Action (fun () -> audio_fade.Target <- 1.0f))
            animation.Add (Animation.Delay 1200.0)
            animation.Add (Animation.Action (fun () -> Screen.change Screen.Type.MainMenu Transitions.Flags.UnderLogo))

    override this.OnExit _ =
        if not closing then Devices.changeVolume Options.options.AudioVolume.Value

    override this.Update (elapsedTime, bounds) =
        base.Update (elapsedTime, bounds)
        audio_fade.Update elapsedTime
        animation.Update elapsedTime
        Devices.changeVolume (Options.options.AudioVolume.Value * float audio_fade.Value)
        
    override this.Draw() =
        let (x, y) = this.Bounds.Center
        Text.drawJust (Style.baseFont, (if closing then "Thank you for playing" else "Loading :)"), 80.f, x, y - 500.0f, Color.White, 0.5f)