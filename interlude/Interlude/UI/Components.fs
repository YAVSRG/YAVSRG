namespace Interlude.UI

open System
open System.Collections.Generic
open Prelude.Common
open Interlude.UI.Animation

type AnchorPoint(value, anchor) =
    inherit AnimationFade(value)
    let mutable anchor = anchor
    member this.Position(min, max) =  min + base.Value + (max - min) * anchor
    member this.Reposition(value, a) = this.SetValue(value); this.SetTarget(value); anchor <- a

type Widget() =

    let left = AnchorPoint(0.f, 0.f)
    let top = AnchorPoint(0.f, 0.f)
    let right = AnchorPoint(0.f, 1.f)
    let bottom = AnchorPoint(0.f, 1.f)

    let animation = AnimationGroup()
    let mutable parent = None
    let children = new List<Widget>()

    member this.Add(c) =
        children.Add(c)
        c.AddTo(this)

    member private this.AddTo(c) =
        match parent with
        | None -> parent <- Some c
        | Some _ -> Logging.Error("Tried to add this widget to a container when it is already in one") ""

    member this.Remove(c) =
        if children.Remove(c) then
            c.RemoveFrom(this)
        else Logging.Error("Tried to remove widget that was not in this container") ""

    member private this.RemoveFrom(c) =
        match parent with
        | None -> Logging.Error("Tried to remove this widget from a container it isn't in one") ""
        | Some p -> if p = c then parent <- None else Logging.Error("Tried to remove this widget from a container when it is in another") ""

    interface IDisposable with
        member this.Dispose() = ()
