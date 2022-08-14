namespace Interlude.UI.Features.MainMenu

open System
open System.Drawing
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Interlude
open Interlude.UI
open Interlude.UI.Components

// Loading screen

type LoadingScreen() as this =
    inherit Screen.T()

    let mutable closing = false
    let fade = Animation.Fade 1.0f
    do this.Animation.Add fade

    override this.OnEnter (prev: Screen.Type) =
        fade.Value <- 0.0f
        Logo.moveCentre ()
        Screen.Toolbar.hide()
        match prev with
        | Screen.Type.MainMenu ->
            closing <- true
            let s = Animation.Sequence()
            s.Add (Animation.Delay 1500.0)
            s.Add (Animation.Action (fun () -> Screen.back Screen.TransitionFlags.Default))
            this.Animation.Add s
        | _ -> 
            let s = Animation.Sequence()
            s.Add (Animation.Delay 1500.0)
            s.Add (Animation.Action (fun () -> Screen.change Screen.Type.MainMenu Screen.TransitionFlags.UnderLogo))
            this.Animation.Add s

    override this.OnExit _ = ()

    override this.Update (elapsedTime, bounds) =
        base.Update (elapsedTime, bounds)
        Devices.changeVolume (Options.options.AudioVolume.Value * float (if closing then 1.0f - fade.Value else fade.Value))
        
    override this.Draw() =
        let (x, y) = this.Bounds.Center
        Text.drawJust (Content.font, (if closing then "Bye o/" else "Loading :)"), 80.f, x, y - 500.0f, Color.White, 0.5f)