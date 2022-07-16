namespace Interlude.UI

open System.Drawing
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics

module rec Dialog =

    [<AbstractClass>]
    type T() as this =
        inherit Widget()
    
        let fade = new Animation.AnimationFade 0.0f
    
        do
            this.Animation.Add(fade)
            fade.Target <- 1.0f
    
        // Begins closing animation
        abstract member BeginClose : unit -> unit
        default this.BeginClose() =
            fade.Target <- 0.0f
    
        // Called when dialog actually closes (end of animation)
        abstract member OnClose: unit -> unit
    
        override this.Draw() =
            Draw.rect this.Bounds (Color.FromArgb(int (200.0f * fade.Value), 0, 0, 0))
            base.Draw()
    
        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if (fade.Value < 0.02f && fade.Target = 0.0f) then
                this.Enabled <- false
                this.OnClose()

        member this.Show() = add this
    
    let private dialogs = ResizeArray<T>()

    let add (d: T) = dialogs.Add d

    let update (elapsedTime, bounds) =
        if dialogs.Count > 0 then
            dialogs.[dialogs.Count - 1].Update(elapsedTime, bounds)
            if not dialogs.[dialogs.Count - 1].Enabled then
                dialogs.[dialogs.Count - 1].Dispose()
                dialogs.RemoveAt(dialogs.Count - 1)
            Input.finish_frame_events()

    let draw () = for d in dialogs do d.Draw()

    let any () = dialogs.Count > 0

type Dialog = Dialog.T