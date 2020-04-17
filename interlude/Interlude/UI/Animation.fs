namespace Interlude.UI

open System.Collections.Generic

type Animation() =
    member this.Complete = false
    member this.Update(_) = ()

module Animation =

    type AnimationFade(value) =
        inherit Animation()
        let mutable value = value
        let mutable target = value
        member this.Value = value
        member this.Target = target
        member this.SetTarget(t) = target <- t
        member this.SetValue(v) = value <- v
        member this.Update(_) = value <- value * 0.95f + target * 0.05f
    
    //Runs an action and then is complete. Good for use in sequence with AnimationTimer
    type AnimationAction(action) =
        inherit Animation()
        let mutable complete = false
        member this.Complete = complete
        member this.Update(_) = action(); complete <- true

    //Animation ends after given milliseconds
    type AnimationTimer(milliseconds) =
        inherit Animation()
        let mutable elapsed = 0.0
        member this.Complete = elapsed >= milliseconds
        member this.Update(elapsedMillis) = elapsed <- elapsed + elapsedMillis

    //Animation lasts forever and counts how many of the given time interval have passed
    type AnimationCounter(milliseconds) =
        inherit Animation()
        let mutable elapsed = 0.0
        let mutable loops = 0
        member this.Update(elapsedMillis) =
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
        member this.Complete = List.forall (fun (a: Animation) -> a.Complete) animations
        member this.Update(elapsed) = animations <- List.filter (fun (a: Animation) -> a.Update(elapsed); not a.Complete) animations
        
    //Sequence of animations run one by one
    type AnimationSequence() =
        inherit Animation()
        let animations: Queue<Animation> = new Queue<Animation>()
        member this.Complete = animations.Count = 0
        member this.Update(elapsed) =
            if animations.Count > 0 then
                let a = animations.Peek()
                a.Update(elapsed)
                if a.Complete then animations.Dequeue() |> ignore

    //possible todo: AnimationLoop which loops a collection of animations, however this requires resettable animations