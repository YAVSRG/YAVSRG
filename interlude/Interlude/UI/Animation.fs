namespace Interlude.UI

open System.Collections.Generic

[<AbstractClass>]
type Animation() =
    abstract member Complete: bool
    default this.Complete = false
    abstract member Update: float -> unit

module Animation =

    type AnimationFade(value) =
        inherit Animation()
        let mutable value = value
        let mutable target = value
        member this.Value = value
        member this.Target = target
        member this.SetTarget(t) = target <- t
        member this.SetValue(v) = value <- v
        override this.Update(_) = value <- value * 0.95f + target * 0.05f

    type AnimationColorMixer(color : OpenTK.Color) =
        inherit Animation()

        let r = AnimationFade(float32 color.R)
        let g = AnimationFade(float32 color.G)
        let b = AnimationFade(float32 color.B)

        member this.SetColor(color: OpenTK.Color) =
            r.SetTarget(float32 color.R); g.SetTarget(float32 color.G); b.SetTarget(float32 color.B)
        member this.SetColor(color: System.Drawing.Color) =
            r.SetTarget(float32 color.R); g.SetTarget(float32 color.G); b.SetTarget(float32 color.B)
        member this.GetColor(alpha) = OpenTK.Color.FromArgb(alpha, int r.Value, int g.Value, int b.Value)   
        member this.GetColor() = OpenTK.Color.FromArgb(255, int r.Value, int g.Value, int b.Value)    

        override this.Update(t) =
            r.Update(t); g.Update(t); b.Update(t);
    
    //Runs an action and then is complete. Good for use in sequence with AnimationTimer
    type AnimationAction(action) =
        inherit Animation()
        let mutable complete = false
        override this.Complete = complete
        override this.Update(_) =
            do action()
            complete <- true

    //Animation ends after given milliseconds
    type AnimationTimer(milliseconds) =
        inherit Animation()
        let mutable elapsed = 0.0
        override this.Complete = elapsed >= milliseconds
        override this.Update(elapsedMillis) = elapsed <- elapsed + elapsedMillis

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
        member this.Time = elapsed
        member this.Loops = loops

    //Group of animations running in parallel
    type AnimationGroup() =
        inherit Animation()
        let mutable animations = []
        member this.Add(a: Animation) = animations <- a :: animations
        override this.Complete = List.forall (fun (a: Animation) -> a.Complete) animations
        override this.Update(elapsed) = animations <- List.filter (fun (a: Animation) -> a.Update(elapsed); not a.Complete) animations
        
    //Sequence of animations run one by one
    type AnimationSequence() =
        inherit Animation()
        let animations: Queue<Animation> = new Queue<Animation>()
        member this.Add(a: Animation) = animations.Enqueue(a)
        override this.Complete = animations.Count = 0
        override this.Update(elapsed) =
            if animations.Count > 0 then
                let a = animations.Peek()
                a.Update(elapsed)
                if a.Complete then animations.Dequeue() |> ignore

    //possible todo: AnimationLoop which loops a collection of animations, however this requires resettable animations