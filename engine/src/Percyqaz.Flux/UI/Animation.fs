namespace Percyqaz.Flux.UI

open System
open System.Drawing
open System.Collections.Generic
open Percyqaz.Flux.Utils

[<AbstractClass>]
type Animation() =
    // Returning true means the animation is complete (for sequential animations)

    // Todo: return unit and split completeness into its own member
    abstract member Update: float -> bool

module Animation =

    // PERMANENT ANIMATIONS - These will run indefinitely and are used for long term counting/sliding effects

    type Fade(value) =
        inherit Animation()
        let mutable value = value
        let mutable target = value
        member this.Value with get() = value and set(v) = value <- v
        member this.Target with get() = target and set(t) = target <- t
        member this.Snap() = value <- target
        override this.Update(elapsedTime) = value <- lerp (MathF.Pow(0.994f, float32 elapsedTime)) target value; false

    type Color(color : Drawing.Color) =
        inherit Animation()

        let r = Fade(float32 color.R)
        let g = Fade(float32 color.G)
        let b = Fade(float32 color.B)

        member this.SetColor(color: Drawing.Color) =
            r.Target <- float32 color.R; g.Target <- float32 color.G; b.Target <- float32 color.B
        member this.GetColor(alpha) = Color.FromArgb(alpha, int r.Value, int g.Value, int b.Value)   
        member this.GetColor() = Color.FromArgb(255, int r.Value, int g.Value, int b.Value)    

        override this.Update(t) =
            r.Update t |> ignore
            g.Update t |> ignore
            b.Update t |> ignore
            false
    
    type Counter(milliseconds) =
        inherit Animation()
        let mutable elapsed = 0.0
        let mutable loops = 0
        override this.Update(elapsedMillis) =
            elapsed <- elapsed + elapsedMillis
            while elapsed >= milliseconds do
                elapsed <- elapsed - milliseconds
                loops <- loops + 1
            false
        member this.Time = elapsed
        member this.Loops = loops

    // TERMINATING ANIMATIONS - These animations can "complete" after a condition and are deleted afterwards

    type Action(action) =
        inherit Animation()
        override this.Update(_) = action(); true

    type Delay(milliseconds) =
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

    type Group() =
        inherit Animation()
        let mutable animations = []
        member this.Add(a: Animation) = lock(this) (fun () -> animations <- a :: animations)
        member this.Complete = animations.IsEmpty
        override this.Update(elapsed) =
            let rec filterBackwards (pred: 'T -> bool) (xs: 'T list) =
                match xs with
                | [] -> []
                | x :: xs ->
                    let f = filterBackwards pred xs
                    if pred x then x :: f else f
            if this.Complete then true
            else lock(this) (fun () -> animations <- filterBackwards (fun (a: Animation) -> a.Update(elapsed) |> not) animations); this.Complete
        
    type Sequence() =
        inherit Animation()
        let animations: Queue<Animation> = new Queue<Animation>()
        member this.Add(a: Animation) = animations.Enqueue(a)
        member this.Complete = animations.Count = 0
        override this.Update(elapsed) =
            if animations.Count > 0 then
                let a = animations.Peek()
                if a.Update(elapsed) then animations.Dequeue() |> ignore
            this.Complete

type Animation with
    static member Fork([<ParamArray>] xs: Animation array) =
        let g = Animation.Group()
        for a in xs do g.Add(a)
        g

    static member Serial([<ParamArray>] xs: Animation array) =
        let s = Animation.Sequence()
        for a in xs do s.Add(a)
        s