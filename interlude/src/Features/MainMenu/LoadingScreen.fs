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
    let fade = Animation.Fade 1.0f
    let animation = Animation.Sequence()

    override this.OnEnter (prev: Screen.Type) =
        fade.Value <- 0.0f
        Logo.moveCentre()
        Screen.Toolbar.hide()
        match prev with
        | Screen.Type.MainMenu ->
            closing <- true
            animation.Add (Animation.Delay 1500.0)
            animation.Add (Animation.Action (fun () -> Screen.back Transitions.Flags.Default))
        | _ ->
            animation.Add (Animation.Delay 1500.0)
            animation.Add (Animation.Action (fun () -> Screen.change Screen.Type.MainMenu Transitions.Flags.UnderLogo))

    override this.OnExit _ = ()

    override this.Update (elapsedTime, bounds) =
        base.Update (elapsedTime, bounds)
        fade.Update elapsedTime
        animation.Update elapsedTime
        Devices.changeVolume (Options.options.AudioVolume.Value * float (if closing then 1.0f - fade.Value else fade.Value))
        
    override this.Draw() =
        let (x, y) = this.Bounds.Center
        Text.drawJust (Style.baseFont, (if closing then "Bye o/" else "Loading :)"), 80.f, x, y - 500.0f, Color.White, 0.5f)