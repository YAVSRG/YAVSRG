namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude

module private Dropdown =

    let ITEMSIZE = 55.0f

    type Item(label: string, color: Color * Color, onclick: unit -> unit) =
        inherit
            Container(
                NodeType.Button(fun () ->
                    Style.click.Play()
                    onclick ()
                )
            )

        override this.Init(parent) =
            this 
            |+ Clickable.Focus(this, Floating = true)
            |* Text(label, Align = Alignment.LEFT, Position = Position.Margin(10.0f, 5.0f), Color = K color)
            base.Init parent

        override this.OnFocus(by_mouse: bool) =
            base.OnFocus by_mouse
            Style.hover.Play()

        override this.Draw() =
            if this.Focused then
                Draw.rect this.Bounds (!*Palette.HOVER)

            base.Draw()

type DropdownWrapper(positioning: IHeight -> Position) =
    inherit SwapContainer()

    let mutable shown = false

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if shown then
            if
                (%%"exit").Tapped()
                || not this.Focused
                || Mouse.left_click ()
                || Mouse.right_click ()
            then
                this.Dismiss()

            if Mouse.hover this.Bounds then
                Input.finish_frame_events ()

    member this.Show<'T when 'T :> Widget and 'T :> IHeight>(d: 'T) =
        shown <- true
        d.Position <- positioning (d :> IHeight)
        this.Current <- d
        if not d.Focused then d.Focus false

    member this.Dismiss() =
        if shown then
            this.Current <- Dummy()
            shown <- false

    member this.Toggle(thunk: unit -> 'T) =
        if shown then this.Dismiss()
        else this.Show(thunk())

/// Represents a dropdown menu where items represent different values for one setting, supports one value being pre-selected
type DropdownOptions<'T when 'T: equality> =
    {
        Items: ('T * string) seq
        Setting: Setting<'T>
        ColorFunc: 'T -> Color * Color
    }

type Dropdown<'T when 'T: equality>(options: DropdownOptions<'T>) as this =
    inherit FrameContainer(NodeType.Container(fun _ -> Some this.Items), Fill = !%Palette.DARK, Border = !%Palette.LIGHT)

    let flow = FlowContainer.Vertical(Dropdown.ITEMSIZE, Floating = true)

    override this.Init(parent: Widget) =
        let mutable what_to_focus: Widget = this

        flow
        |+ seq {
            for (value, label) in options.Items do
                let item =
                    Dropdown.Item(
                        label,
                        options.ColorFunc value,
                        fun () ->
                            options.Setting.Set value
                            Selection.up false
                    )

                if value = options.Setting.Value then
                    what_to_focus <- item

                yield item
        }
        |> ScrollContainer
        |> this.Add

        base.Init parent
        what_to_focus.Focus false

    member private this.Items = flow

    interface IHeight with
        member this.Height = float32 (Seq.length options.Items) * Dropdown.ITEMSIZE

/// Represents a dropdown where each item represents a menu option
type DropdownMenuOptions =
    {
        Items: ((unit -> unit) * string) seq
    }

type DropdownMenu(options: DropdownMenuOptions) as this =
    inherit FrameContainer(NodeType.Container(fun _ -> Some this.Items), Fill = !%Palette.DARK, Border = !%Palette.LIGHT)

    let flow = FlowContainer.Vertical(Dropdown.ITEMSIZE, Floating = true)

    override this.Init(parent: Widget) =
        flow
        |+ seq {
            for (action, label) in options.Items do
                yield
                    Dropdown.Item(
                        label,
                        Colors.text,
                        fun () ->
                            action ()
                            Selection.up false
                    )
        }
        |> ScrollContainer
        |> this.Add

        base.Init parent

    member private this.Items = flow
        
    interface IHeight with
        member this.Height = float32 (Seq.length options.Items) * Dropdown.ITEMSIZE