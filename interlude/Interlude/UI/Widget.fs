namespace Interlude.UI

open System.Collections.Generic
open Prelude.Common
open Interlude.Graphics
open Interlude.UI.Animation

(*
    AnchorPoints calculate the position of a widget's edge relative to its parent
    They are parameterised by an anchor and an offset

     To calculate the position of an edge (for example the value v for the left edge when given the left and right edges of the parent)
        Anchor is a value from 0-1 representing the percentage of the way across the parent widget to place the edge
        Offset is a value added to that to translate the edge by a fixed number of pixels

        Examples: AnchorPoint(50.0f, 0.0f) places the widget's edge 50 pixels left/down from the parent's left/top edge
                  AnchorPoint(-70.0f, 0.5f) places the widget's edge 70 pixels right/up from the parent's centre
    This can be used to flexibly describe the layout of a UI in terms of parent-child relations and these anchor points
*)

type AnchorPoint(offset, anchor) =
    inherit AnimationFade(offset)
    let mutable anchor_ = anchor
    //calculates the position given lower and upper bounds from the parent
    member this.Position(min, max) = min + base.Value + (max - min) * anchor_
    //snaps to a brand new position as if we constructed a new point
    member this.Reposition(offset, anchor) = this.Value <- offset; this.Target <- offset; anchor_ <- anchor

    member this.MoveRelative(min, max, value) = this.Target <- value - min - (max - min) * anchor_
    member this.RepositionRelative(min, max, value) = this.MoveRelative(min, max, value); this.Value <- this.Target

    member this.Snap() = this.Value <- this.Target

type Position =
    {
        Left: float32 * float32
        Top: float32 * float32
        Right: float32 * float32
        Bottom: float32 * float32
    }

[<AutoOpen>]
module PositionOperators =
    let (%+) percentage offset = (offset, percentage)
    let (%-) percentage offset =  (-offset, percentage)
    let (^+) (percentage, x) offset = (x + offset, percentage)
    let (^-) (percentage, x) offset = (x - offset, percentage)

module Position =
    let min = 0.0f %+ 0.0f
    let max = 1.0f %+ 0.0f

type Position with
    static member Default =
        { 
            Left = Position.min
            Top = Position.min
            Right = Position.max
            Bottom = Position.max
        }
    static member Margin amount =
        { 
            Left = 0.0f %+ amount
            Top = 0.0f %+ amount
            Right = 1.0f %- amount
            Bottom = 1.0f %- amount
        }
    static member SliceLeft amount = { Position.Default with Right = 0.0f %+ amount }
    static member SliceTop amount = { Position.Default with Bottom = 0.0f %+ amount }
    static member SliceRight amount = { Position.Default with Left = 1.0f %- amount }
    static member SliceBottom amount = { Position.Default with Top = 1.0f %- amount }
    static member Row y height =
        {
            Left = Position.min
            Top = 0.0f %+ y
            Right = Position.max
            Bottom = 0.0f %+ (y + height)
        }
    static member Column x width =
        {
            Left = 0.0f %+ x
            Top = Position.min
            Right = 0.0f %+ (x + width)
            Bottom = Position.max
        }
    static member Box (anchorx, anchory, x, y, width, height) =
        {
            Left = anchorx %+ x
            Top = anchory %+ y
            Right = anchorx %+ (x + width)
            Bottom = anchory %+ (y + height)
        }
    static member Box (anchorx, anchory, width, height) =
        {
            Left = anchorx %+ 0.0f
            Top = anchory %+ 0.0f
            Right = anchorx %+ width
            Bottom = anchory %+ height
        }

(*
    Widgets are the atomic components of the UI system.
      All widgets can contain other "child" widgets embedded in them that inherit from their position
      What widgets do with their child widgets can be up to implementation, by default all are drawn and updated with the parent.
*)

type Widget() =

    let children = new List<Widget>()
    let mutable parent = None

    let mutable bounds = Rect.zero
    let mutable visibleBounds = Rect.zero
    let left = AnchorPoint (0.0f, 0.0f)
    let top = AnchorPoint (0.0f, 0.0f)
    let right = AnchorPoint (0.0f, 1.0f)
    let bottom = AnchorPoint (0.0f, 1.0f)

    let animation = Animation.Fork (left, top, right, bottom)
    let mutable enable = true
    let mutable initialised = false

    member this.Children = children
    member this.Parent = parent

    member this.Bounds = bounds

    abstract member VisibleBounds : Rect
    default this.VisibleBounds = visibleBounds

    member this.Anchors = (left, top, right, bottom)
    member this.Animation = animation
    member this.Enabled with get() = enable and set(value) = enable <- value
    member this.Initialised = initialised

    abstract member Add: Widget -> unit
    default this.Add(c: Widget) =
        children.Add c
        c.OnAddedTo this

    abstract member OnAddedTo: Widget -> unit
    default this.OnAddedTo(c: Widget) =
        match parent with
        | None -> parent <- Some c
        | Some parent -> Logging.Debug (sprintf "Tried to add this (%O) to a parent (%O) when parent is already (%O)" this c parent)
        
    /// Removes a child from this widget - Dispose method of the child is not called (sometimes the child will be reused)
    abstract member Remove: Widget -> unit
    default this.Remove(c: Widget) =
        if children.Remove c then
            c.OnRemovedFrom this
        else Logging.Error "Tried to remove widget that was not in this container"

    member private this.OnRemovedFrom(c: Widget) =
        match parent with
        | None -> Logging.Debug (sprintf "Tried to remove this (%O) from parent (%O) but it has no parent" this c)
        | Some p ->
            if p = c then parent <- None
            else Logging.Debug (sprintf "Tried to remove this (%O) from parent (%O) but parent is actually (%O)" this c p)

    /// Queues up the action to take place immediately before the next update loop, making it thread/loop-safe
    ///   When updating widgets from a background task, use this.
    member this.Synchronized(action) =
        animation.Add(new AnimationAction(action))

    /// Destroys a widget by removing it from its parent, then disposing it (will be garbage collected)
    /// Note that this is safe to call inside an update/draw method OR from another thread
    member this.Destroy() =
        match this.Parent with
        | Some parent -> parent.Synchronized(fun () -> (parent.Remove this; this.Dispose()))
        | None -> this.Dispose()

    /// Clears all children from the widget (with the intention of them being garbage collected, not reused)
    abstract member Clear: unit -> unit
    default this.Clear() =
        for c in children do 
            c.OnRemovedFrom this
            c.Dispose()
        children.Clear()

    /// Draw is called at the framerate of the game (normally unlimited) and should be where the widget performs render calls to draw it on screen
    abstract member Draw: unit -> unit
    default this.Draw() = for c in children do if c.Initialised && c.Enabled then c.Draw()

    member this.UpdateBounds(struct (l, t, r, b): Rect) =
        initialised <- true
        bounds <- Rect.create <| left.Position (l, r) <| top.Position (t, b) <| right.Position (l, r) <| bottom.Position (t, b)
        visibleBounds <- Rect.intersect bounds (match this.Parent with None -> bounds | Some (p: Widget) -> p.VisibleBounds)

    /// Update is called at a fixed framerate (120Hz) and should be where the widget handles input and other time-based logic
    abstract member Update: float * Rect -> unit
    default this.Update(elapsedTime, bounds: Rect) =
        animation.Update elapsedTime |> ignore
        this.UpdateBounds bounds
        for i in children.Count - 1 .. -1 .. 0 do
            if children.[i].Enabled then children.[i].Update (elapsedTime, this.Bounds)

    //todo: tear these out and replace with nice idiomatic positioners
    member this.Reposition(l, la, t, ta, r, ra, b, ba) =
        left.Reposition (l, la)
        top.Reposition (t, ta)
        right.Reposition (r, ra)
        bottom.Reposition (b, ba)

    member this.Position (pos: Position) : Widget =
        left.Reposition pos.Left
        top.Reposition pos.Top
        right.Reposition pos.Right
        bottom.Reposition pos.Bottom
        this
    
    member this.Reposition(l, t, r, b) = this.Reposition (l, 0.0f, t, 0.0f, r, 1.0f, b, 1.0f)

    member this.Move(l: float32, t: float32, r: float32, b: float32) =
        left.Target <- l
        top.Target <- t
        right.Target <- r
        bottom.Target <- b

    // Dispose is called when a widget is going out of scope/about to be garbage collected and allows it to release any resources
    abstract member Dispose: unit -> unit
    default this.Dispose() = for c in children do c.Dispose()

    static member (|-+) (parent: #Widget, child: #Widget) = parent.Add child; parent
    static member (|-*) (parent: #Widget, anim: #Animation) = parent.Animation.Add anim; parent
    static member (|=+) (parent: #Widget, child: #Widget) = parent.Add child
    static member (|=*) (parent: #Widget, anim: #Animation) = parent.Animation.Add anim

module Icons = 
    let star = Feather.star
    let back = Feather.arrow_left
    let bpm = Feather.music
    let time = Feather.clock
    let sparkle = Feather.award

    let edit = Feather.edit_2
    let add = Feather.plus_circle
    let remove = Feather.minus_circle
    let delete = Feather.trash
    let selected = Feather.check_circle
    let unselected = Feather.circle

    let goal = Feather.flag
    let playlist = Feather.list
    let tag = Feather.tag
    let order_ascending = Feather.trending_down
    let order_descending = Feather.trending_up

    let system = Feather.airplay
    let themes = Feather.image
    let gameplay = Feather.sliders
    let binds = Feather.link
    let debug = Feather.terminal

    let info = Feather.info
    let alert = Feather.alert_circle
    let system_notification = Feather.alert_octagon