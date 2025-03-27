namespace Percyqaz.Flux.UI

open System
open System.Drawing
open System.Collections.Generic
open Percyqaz.Common

[<AbstractClass>]
type Animation() =
    /// Called to update the animation each frame
    abstract member Update: float -> unit
    /// Called directly after the animation is updated (if at all)
    /// True => it doesn't need to run any more and can be disposed of
    abstract member Complete: bool

module Animation =

    // PERMANENT ANIMATIONS - These will run indefinitely and are used for long term counting/sliding effects

    type Fade(initial_value: float32) =
        inherit Animation()

        static let DURATION_MS = 1500.0

        let mutable start_value = initial_value
        let mutable end_value = initial_value
        let mutable time_remaining = 0.0

        member this.Value
            with get () =
                if time_remaining > 0.0 then
                    lerp (float32 <| Math.Pow(0.994, DURATION_MS - time_remaining)) end_value start_value
                else end_value
            and set (v) =
                start_value <- v
                time_remaining <- DURATION_MS

        member this.Alpha = int (MathF.Round(255.0f * this.Value))

        member this.Target
            with get () = end_value
            and set (t) =
                start_value <- this.Value
                end_value <- t
                time_remaining <- DURATION_MS

        member this.Moving = time_remaining > 0.0

        override this.Update(elapsed_ms: float) =
            if time_remaining > 0.0 then
                time_remaining <- time_remaining - elapsed_ms

        override this.Complete = false

        member this.Snap() =
            start_value <- end_value
            time_remaining <- 0.0

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
        member this.Time = min elapsed this.Interval
        member this.Loops = loops
        member this.Progress = this.Time / this.Interval

        member this.Reset() =
            elapsed <- 0.0
            loops <- 0

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
        member val Interval = total_ms with get, set
        member this.Time = min elapsed this.Interval
        member this.FrameSkip() = frameskip <- true
        member this.Reset() = elapsed <- 0.0
        member this.Progress = this.Time / this.Interval

        override this.Update(elapsed_ms) =
            if frameskip then
                frameskip <- false
            else
                elapsed <- elapsed + elapsed_ms

        override this.Complete = elapsed >= this.Interval

    // COMPOSING ANIMATIONS

    type Group() =
        inherit Animation()
        let mutable animations = []
        let mutable looping = false
        let mutable add_after_loop = []

        member this.Add(a: Animation) =
            lock
                this
                (fun () ->
                    if looping then
                        add_after_loop <- a :: add_after_loop
                    else
                        animations <- a :: animations
                )

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
                looping <- true

                lock
                    this
                    (fun () ->
                        animations <- loop animations

                        if not (List.isEmpty add_after_loop) then
                            animations <- add_after_loop @ animations
                            add_after_loop <- []
                    )

                looping <- false

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