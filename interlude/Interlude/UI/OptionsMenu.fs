namespace Interlude.UI

open System
open System.Drawing
open System.Collections.Generic
open OpenTK
open Prelude.Common
open Interlude.Options
open Interlude.Render
open Interlude
open Interlude.Utils
open Interlude.Input
open OpenTK.Windowing.GraphicsLibraryFramework
open Interlude.Options
open Interlude.UI.Animation
open Interlude.UI.Components

(*
    Needs of an options screen
    - Navigatable with arrow keys, z for back and x for select
    - Clickable
    - Contains selectable items itself
*)

module OptionsMenu =
    
        [<AbstractClass>]
        type Selectable() =
            inherit Widget()

            let mutable hoverChild: Selectable option = None
            let mutable hoverSelected: bool = false

            member this.SelectedChild
                with get() = if hoverSelected then hoverChild else None
                and set(value) =
                    match value with
                    | Some v ->
                        match this.SelectedChild with
                        | Some c ->
                            if v <> c then
                                c.OnDeselect()
                                hoverChild <- value
                                v.OnSelect()
                        | None ->
                            hoverChild <- value
                            hoverSelected <- true
                            match this.Parent with
                            | Some p ->
                                match p with
                                | :? Selectable as p -> p.SelectedChild <- Some this
                                | _ -> ()
                            | None -> ()
                            v.OnSelect()
                    | None -> this.HoverChild <- None

            member this.HoverChild
                with get() = hoverChild
                and set(value) =
                    match this.SelectedChild with
                    | Some c -> c.OnDeselect()
                    | None -> ()
                    hoverChild <- value
                    hoverSelected <- false
                    if value.IsSome then
                        match this.Parent with
                        | Some p ->
                            match p with
                            | :? Selectable as p -> p.SelectedChild <- Some this
                            | _ -> ()
                        | None -> ()

            member this.Selected
                with get() =
                    match this.Parent with
                    | Some p ->
                        match p with
                        | :? Selectable as p -> p.SelectedChild = Some this
                        | _ -> true
                    | None -> true
                and set(value) =
                    match this.Parent with
                    | Some p ->
                        match p with
                        | :? Selectable as p ->
                            if value then p.SelectedChild <- Some this elif this.Hover then p.HoverChild <- Some this
                        | _ -> ()
                    | None -> ()

            member this.Hover
                with get() =
                    match this.Parent with
                    | Some p ->
                        match p with
                        | :? Selectable as p -> p.HoverChild = Some this
                        | _ -> true
                    | None -> true
                and set(value) =
                    match this.Parent with
                    | Some p ->
                        match p with
                        | :? Selectable as p ->
                            if value then p.HoverChild <- Some this elif this.Hover then p.HoverChild <- None
                        | _ -> ()
                    | None -> ()

            abstract member OnSelect: unit -> unit
            default this.OnSelect() = ()

            abstract member OnDeselect: unit -> unit
            default this.OnDeselect() = ()

        [<AbstractClass>]
        type NavigateSelectable() =
            inherit Selectable()

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
                if this.Selected && this.SelectedChild.IsNone then
                    if options.Hotkeys.Previous.Get().Tapped() then this.Left()
                    elif options.Hotkeys.Up.Get().Tapped() then this.Up()
                    elif options.Hotkeys.Next.Get().Tapped() then this.Right()
                    elif options.Hotkeys.Down.Get().Tapped() then this.Down()
                    elif options.Hotkeys.Select.Get().Tapped() then this.SelectedChild <- this.HoverChild
                    elif options.Hotkeys.Exit.Get().Tapped() then this.Selected <- false

        type RowSelectable() =
            inherit NavigateSelectable()

            let items = ResizeArray<Selectable>()

            override this.Add(c) =
                base.Add(c)
                match c with
                | :? Selectable as c -> items.Add(c)
                | _ -> ()

            override this.OnSelect() = base.OnSelect(); if this.HoverChild.IsNone then this.HoverChild <- Some items.[0]

            override this.Left() =
                match this.HoverChild with
                | None -> failwith "impossible"
                | Some w -> let i = (items.IndexOf(w) - 1 + items.Count) % items.Count in this.HoverChild <- Some items.[i]

            override this.Right() =
                match this.HoverChild with
                | None -> failwith "impossible"
                | Some w -> let i = (items.IndexOf(w) + 1) % items.Count in this.HoverChild <- Some items.[i]

        type BigButton(label, onClick) as this =
            inherit Selectable()

            do
                this.Add(Frame())
                this.Add(TextBox(K label, K Color.White, 0.5f))
                this.Add(Clickable((fun () -> this.Selected <- true), fun b -> if b then this.Hover <- true))

            override this.OnSelect() =
                this.Selected <- false
                onClick()