namespace Interlude.UI

open System
open System.Drawing
open System.Collections.Generic

[<AbstractClass>]
type Animation() =
    abstract member Update: float -> bool

module Animation =

    // PERMANENT ANIMATIONS - These will run indefinitely and are used for long term counting/sliding effects

    type AnimationFade(value) =
        inherit Animation()
        let mutable value = value
        let mutable target = value
        let mutable stopped = false
        member this.Value with get() = value and set(v) = value <- v
        member this.Target with get() = target and set(t) = target <- t
        member this.Stop() = stopped <- true
        override this.Update(_) = value <- value * 0.95f + target * 0.05f; stopped

    type AnimationColorMixer(color : Color) =
        inherit Animation()

        let r = AnimationFade(float32 color.R)
        let g = AnimationFade(float32 color.G)
        let b = AnimationFade(float32 color.B)

        member this.SetColor(color: Color) =
            r.Target <- float32 color.R; g.Target <- float32 color.G; b.Target <- float32 color.B
        member this.GetColor(alpha) = Color.FromArgb(alpha, int r.Value, int g.Value, int b.Value)   
        member this.GetColor() = Color.FromArgb(255, int r.Value, int g.Value, int b.Value)    

        override this.Update(t) =
            r.Update(t) |> ignore; g.Update(t) |> ignore; b.Update(t) |> ignore; false
    
    //Animation lasts forever and counts how many of the given time interval have passed
    type AnimationCounter(milliseconds) =
        inherit Animation()
        let mutable elapsed = 0.0
        let mutable loops = 0
        override this.Update(elapsedMillis) =
            elapsed <- elapsed + elapsedMillis
            while (elapsed >= milliseconds) do
                elapsed <- elapsed - milliseconds
                loops <- loops + 1
            false
        member this.Time = elapsed
        member this.Loops = loops

    // TERMINATING ANIMATIONS - These animations can "complete" after a condition and are deleted afterwards

    type AnimationAction(action) =
        inherit Animation()
        override this.Update(_) = action(); true

    type AnimationTimer(milliseconds) =
        inherit Animation()
        let mutable elapsed = 0.0
        let mutable milliseconds = milliseconds
        let mutable frameskip = false
        member this.Elapsed = elapsed
        member this.FrameSkip() = frameskip <- true
        member this.ChangeLength(ms) = milliseconds <- ms
        member this.Reset() = elapsed <- 0.0
        override this.Update(elapsedMillis) =
            if frameskip then frameskip <- false else elapsed <- elapsed + elapsedMillis
            elapsed >= milliseconds

    // COMPOSING ANIMATIONS

    type AnimationGroup() =
        inherit Animation()
        let mutable animations = []
        member this.Add(a: Animation) = animations <- a :: animations
        member this.Complete = animations.IsEmpty
        override this.Update(elapsed) =
            animations <- List.filter (fun (a: Animation) -> a.Update(elapsed) |> not) animations
            this.Complete
        
    type AnimationSequence() =
        inherit Animation()
        let animations: Queue<Animation> = new Queue<Animation>()
        member this.Add(a: Animation) = animations.Enqueue(a)
        member this.Complete = animations.Count = 0
        override this.Update(elapsed) =
            if animations.Count > 0 then
                let a = animations.Peek()
                if a.Update(elapsed) then animations.Dequeue() |> ignore
            this.Complete

open Animation

type Animation with
    static member Fork([<ParamArray>] xs: Animation array) =
        let g = AnimationGroup()
        for a in xs do g.Add(a)
        g

    static member Serial([<ParamArray>] xs: Animation array) =
        let s = AnimationSequence()
        for a in xs do s.Add(a)
        s