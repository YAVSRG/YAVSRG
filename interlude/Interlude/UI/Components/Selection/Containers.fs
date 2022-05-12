namespace Interlude.UI.Components.Selection.Containers

open Prelude.Common
open Interlude.Options
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection

type NavigateSelectable() =
    inherit Selectable()

    let mutable disposed = false

    abstract member Left: unit -> unit
    default this.Left() = ()

    abstract member Up: unit -> unit
    default this.Up() = ()

    abstract member Right: unit -> unit
    default this.Right() = ()

    abstract member Down: unit -> unit
    default this.Down() = ()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if not disposed && this.Selected && this.SelectedChild.IsNone then
            if (!|Hotkey.Previous).Tapped() then this.Left()
            if (!|Hotkey.Up).Tapped() then this.Up()
            if (!|Hotkey.Next).Tapped() then this.Right()
            if (!|Hotkey.Down).Tapped() then this.Down()
            if (!|Hotkey.Select).Tapped() then this.SelectedChild <- this.HoverChild
            if (!|Hotkey.Exit).Tapped() then this.Selected <- false

    override this.Dispose() = base.Dispose(); disposed <- true

type ListSelectable(horizontal) =
    inherit NavigateSelectable()

    let items = ResizeArray<Selectable>()
    let mutable lastHover = None

    member this.Previous() =
        match this.HoverChild with
        | None -> Logging.Debug "No hoverchild for this ListSelectable, there should always be one"
        | Some w -> let i = (items.IndexOf w - 1 + items.Count) % items.Count in this.HoverChild <- Some items.[i]

    member this.Next() =
        match this.HoverChild with
        | None -> Logging.Debug "No hoverchild for this ListSelectable, there should always be one"
        | Some w -> let i = (items.IndexOf w + 1) % items.Count in this.HoverChild <- Some items.[i]

    override this.Add(c) =
        base.Add(c)
        match c with
        | :? Selectable as c -> items.Add c
        | _ -> ()

    override this.Remove(c) =
        base.Remove(c)
        match c with
        | :? Selectable as c -> items.Remove c |> ignore
        | _ -> ()

    override this.OnSelect() = base.OnSelect(); if (match lastHover with None -> true | Some l -> not (items.Contains l)) then this.HoverChild <- Some items.[0] else this.HoverChild <- lastHover
    override this.OnDeselect() = base.OnDeselect(); lastHover <- this.HoverChild; this.HoverChild <- None
    override this.OnDehover() = base.OnDehover(); for i in items do i.OnDehover()

    override this.Left() = if horizontal then this.Previous()
    override this.Right() = if horizontal then this.Next()
    override this.Up() = if not horizontal then this.Previous()
    override this.Down() = if not horizontal then this.Next()

    override this.Clear() = base.Clear(); items.Clear()

type FlowSelectable(height, spacing) as this =
    inherit Selectable()

    let mutable h = 0.0f
    let fc = new FlowContainer()
    let ls =
        { new ListSelectable(false) with
            override _.SParent = this.SParent
            override this.Up() = base.Up(); Option.iter fc.ScrollTo this.HoverChild
            override this.Down() = base.Down(); Option.iter fc.ScrollTo this.HoverChild }

    do
        fc.Add ls
        this.Add fc

    override this.Add(c) =
        if c = (fc :> Widget) then base.Add c
        else
            c.Position( Position.Row h height ) |> ls.Add
            h <- h + height + spacing
            ls.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, h, 0.0f)

    override this.Remove(c) =
        ls.Remove c
        h <- 0.0f
        for child in ls.Children do
            child.Reposition(0.0f, 0.0f, h, 0.0f, 0.0f, 1.0f, h + height, 0.0f)
            h <- h + height + spacing
        ls.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, h, 0.0f)

    override this.OnSelect() = base.OnSelect(); ls.Selected <- true

    override this.Clear() = ls.Clear()