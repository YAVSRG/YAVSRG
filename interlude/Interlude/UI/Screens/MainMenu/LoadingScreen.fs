namespace Interlude.UI.Screens.MainMenu

open System
open System.Drawing
open Interlude
open Interlude.Graphics
open Interlude.UI
open Interlude.UI.Animation
open Interlude.UI.Components

// Loading screen

type LoadingScreen() as this =
    inherit Screen.T()

    let mutable closing = false
    let fade = new AnimationFade 1.0f
    do this.Animation.Add fade

    override this.OnEnter (prev: Screen.Type) =
        fade.Value <- 0.0f
        Logo.moveCentre ()
        Screen.hideToolbar <- true
        match prev with
        | Screen.Type.MainMenu ->
            closing <- true
            let s = AnimationSequence()
            s.Add (AnimationTimer 1500.0)
            s.Add (AnimationAction (fun () -> Screen.back Screen.TransitionFlag.Default))
            this.Animation.Add s
        | _ -> 
            let s = AnimationSequence()
            s.Add (AnimationTimer 1500.0)
            s.Add (AnimationAction(fun () -> Screen.change Screen.Type.MainMenu Screen.TransitionFlag.UnderLogo))
            this.Animation.Add s

    override this.OnExit _ = ()

    override this.Update (elapsedTime, bounds) =
        base.Update (elapsedTime, bounds)
        Audio.changeVolume (Options.options.AudioVolume.Value * float (if closing then 1.0f - fade.Value else fade.Value))
        
    override this.Draw() =
        let (x, y) = Rect.center this.Bounds
        Text.drawJust (Content.font, (if closing then "Bye o/" else "Loading :)"), 80.f, x, y - 500.0f, Color.White, 0.5f)