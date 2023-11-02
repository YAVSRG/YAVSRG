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
            with get () = if time > 0.0 then value else target
            and set (v) =
                value <- v
                time <- 1500.0

        member this.Alpha = int (MathF.Round(255.0f * value))

        member this.Target
            with get () = target
            and set (t) =
                target <- t
                time <- 1500.0

        member this.Moving = time > 0.0

        override this.Update(elapsed_ms) =
            if time > 0.0 then
                value <- lerp (float32 <| Math.Pow(0.994, elapsed_ms)) target value
                time <- time - elapsed_ms

        override this.Complete = false

        member this.Snap() =
            value <- target
            time <- 0.0

    type Color(color: Drawing.Color) =
        inherit Animation()

        let r = Fade(float32 color.R)
        let g = Fade(float32 color.G)
        let b = Fade(float32 color.B)
        let a = Fade(float32 color.A)

        member this.Target
            with get () = Color.FromArgb(int a.Value, int r.Target, int g.Target, int b.Target)
            and set (color: Drawing.Color) =
                r.Target <- float32 color.R
                g.Target <- float32 color.G
                b.Target <- float32 color.B
                a.Target <- float32 color.A

        member this.Value
            with get () = Color.FromArgb(int a.Value, int r.Value, int g.Value, int b.Value)
            and set (color: Drawing.Color) =
                r.Value <- float32 color.R
                g.Value <- float32 color.G
                b.Value <- float32 color.B
                a.Value <- float32 color.A

        override this.Update(elapsed_ms) =
            r.Update elapsed_ms
            g.Update elapsed_ms
            b.Update elapsed_ms
            a.Update elapsed_ms

        override this.Complete = false

    type Counter(milliseconds) =
        inherit Animation()
        let mutable elapsed = 0.0
        let mutable loops = 0

        override this.Update(elapsed_ms) =
            elapsed <- elapsed + elapsed_ms

            while elapsed >= this.Interval do
                elapsed <- elapsed - this.Interval
                loops <- loops + 1

        override this.Complete = false
        member val Interval = milliseconds with get, set
        member this.Time = elapsed
        member this.Loops = loops

    // TERMINATING ANIMATIONS - These animations can "complete" after a condition and are deleted afterwards

    type Action(action) =
        inherit Animation()
        override this.Update(_) = action ()
        override this.Complete = true

    type ActionLoop(action) =
        inherit Animation()
        override this.Update(_) = action ()
        override this.Complete = false

    type Delay(total_ms) =
        inherit Animation()
        let mutable elapsed = 0.0
        let mutable frameskip = false
        member this.Elapsed = elapsed
        member this.FrameSkip() = frameskip <- true
        member this.Reset() = elapsed <- 0.0

        override this.Update(elapsed_ms) =
            if frameskip then
                frameskip <- false
            else
                elapsed <- elapsed + elapsed_ms

        override this.Complete = elapsed >= total_ms

    // COMPOSING ANIMATIONS

    type Group() =
        inherit Animation()
        let mutable animations = []

        member this.Add(a: Animation) =
            lock (this) (fun () -> animations <- a :: animations)

        override this.Complete = animations.IsEmpty

        override this.Update(elapsed_ms) =
            let rec loop (xs: Animation list) =
                match xs with
                | [] -> []
                | x :: xs ->
                    let f = loop xs in
                    x.Update elapsed_ms
                    if x.Complete then f else x :: f

            if not this.Complete then
                lock (this) (fun () -> animations <- loop animations)

    type Sequence() =
        inherit Animation()
        let animations: Queue<Animation> = new Queue<Animation>()
        member this.Add(a: Animation) = animations.Enqueue(a)
        override this.Complete = animations.Count = 0

        override this.Update(elapsed_ms) =
            if animations.Count > 0 then
                let a = animations.Peek()
                a.Update elapsed_ms

                if a.Complete then
                    animations.Dequeue() |> ignore

    let fork (xs: Animation seq) =
        let g = Group()

        for a in xs do
            g.Add a

        g

    let seq (xs: Animation seq) =
        let s = Sequence()

        for a in xs do
            s.Add a

        s
