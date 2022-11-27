namespace Percyqaz.Flux.UI

open System
open System.Collections.Generic
open Percyqaz.Flux.Utils

[<AbstractClass>]
type Animation() =
    /// Called to update the animation each frame
    abstract member Update: float -> unit
    /// Called directly after the animation is updated (if at all)
    /// True => it doesn't need to run any more and can be disposed of
    abstract member Complete: bool

module Animation =

    // PERMANENT ANIMATIONS - These will run indefinitely and are used for long term counting/sliding effects

    type Fade(value) =
        inherit Animation()
        let mutable value = value
        let mutable target = value
        let mutable time = 0.0

        member this.Value 
            with get() = if time > 0.0 then value else target
            and set(v) = value <- v; time <- 1500.0

        member this.Alpha = int(255.0f * value)

        member this.Target 
            with get() = target
            and set(t) = target <- t; time <- 1500.0

        member this.Moving = time > 0.0

        override this.Update(elapsedTime) = 
            if time > 0.0 then
                value <- lerp (float32 <| Math.Pow(0.994, elapsedTime)) target value
                time <- time - elapsedTime

        override this.Complete = false

        member this.Snap() = value <- target; time <- 0.0

    type Color(color : Drawing.Color) =
        inherit Animation()

        let r = Fade(float32 color.R)
        let g = Fade(float32 color.G)
        let b = Fade(float32 color.B)

        member this.SetColor(color: Drawing.Color) =
            r.Target <- float32 color.R; g.Target <- float32 color.G; b.Target <- float32 color.B
        member this.GetColor(alpha) = Color.FromArgb(alpha, int r.Value, int g.Value, int b.Value)

        member this.Target
            with get() = Color.FromArgb(int r.Target, int g.Target, int b.Target)
            and set(color: Drawing.Color) = 
                r.Target <- float32 color.R
                g.Target <- float32 color.G
                b.Target <- float32 color.B

        member this.Value
            with get() = Color.FromArgb(int r.Value, int g.Value, int b.Value)
            and set(color: Drawing.Color) = 
                r.Value <- float32 color.R
                g.Value <- float32 color.G
                b.Value <- float32 color.B

        override this.Update(t) =
            r.Update t; g.Update t; b.Update t

        override this.Complete = false
    
    type Counter(milliseconds) =
        inherit Animation()
        let mutable elapsed = 0.0
        let mutable loops = 0
        override this.Update(elapsedMillis) =
            elapsed <- elapsed + elapsedMillis
            while elapsed >= milliseconds do
                elapsed <- elapsed - milliseconds
                loops <- loops + 1
        override this.Complete = false
        member this.Time = elapsed
        member this.Loops = loops

    // TERMINATING ANIMATIONS - These animations can "complete" after a condition and are deleted afterwards

    type Action(action) =
        inherit Animation()
        override this.Update(_) = action()
        override this.Complete = true

    type Delay(milliseconds) =
        inherit Animation()
        let mutable elapsed = 0.0
        let mutable frameskip = false
        member this.Elapsed = elapsed
        member this.FrameSkip() = frameskip <- true
        member this.Reset() = elapsed <- 0.0
        override this.Update(elapsedMillis) =
            if frameskip then frameskip <- false else elapsed <- elapsed + elapsedMillis
        
        override this.Complete = elapsed >= milliseconds

    // COMPOSING ANIMATIONS

    type Group() =
        inherit Animation()
        let mutable animations = []
        member this.Add(a: Animation) = lock(this) (fun () -> animations <- a :: animations)
        override this.Complete = animations.IsEmpty
        override this.Update(elapsed) =
            let rec loop (xs: Animation list) =
                match xs with
                | [] -> []
                | x :: xs -> let f = loop xs in x.Update elapsed; if x.Complete then f else x :: f
            if not this.Complete then
                lock(this) (fun () -> animations <- loop animations)
        
    type Sequence() =
        inherit Animation()
        let animations: Queue<Animation> = new Queue<Animation>()
        member this.Add(a: Animation) = animations.Enqueue(a)
        override this.Complete = animations.Count = 0
        override this.Update(elapsed) =
            if animations.Count > 0 then
                let a = animations.Peek()
                a.Update elapsed
                if a.Complete then animations.Dequeue() |> ignore

    let fork (xs: Animation seq) = 
        let g = Group()
        for a in xs do g.Add a
        g

    let seq (xs: Animation seq) =
        let s = Sequence()
        for a in xs do s.Add a
        s