namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude

module private Dropdown =

    let ITEMSIZE = 55.0f

    type Item(label: string, color: Color * Color, onclick: unit -> unit) as this =
        inherit
            Container(
                NodeType.Button(fun () ->
                    Style.click.Play()
                    onclick ()
                )
            )

        do
            this |+ Clickable.Focus(this, Floating = true)
            |* Text(label, Align = Alignment.LEFT, Position = Position.Margin(10.0f, 5.0f), Color = K color)

        override this.OnFocus(by_mouse: bool) =
            base.OnFocus by_mouse
            Style.hover.Play()

        override this.Draw() =
            if this.Focused then
                Draw.rect this.Bounds (!*Palette.HOVER)

            base.Draw()

type DropdownOptions<'T when 'T: equality> =
    {
        Items: ('T * string) seq
        Setting: Setting<'T>
        ColorFunc: 'T -> Color * Color
        OnClose: unit -> unit
    }

type Dropdown<'T when 'T: equality>(options: DropdownOptions<'T>) as this =
    inherit
        FrameContainer(NodeType.Container(fun _ -> Some this.Items), Fill = !%Palette.DARK, Border = !%Palette.LIGHT)

    let flow = FlowContainer.Vertical(Dropdown.ITEMSIZE, Floating = true)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if
            (%%"exit").Tapped()
            || not this.Focused
            || Mouse.left_click ()
            || Mouse.right_click ()
        then
            this.Close()

        if Mouse.hover this.Bounds then
            Input.finish_frame_events ()

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
                            this.Close()
                    )

                if value = options.Setting.Value then
                    what_to_focus <- item

                yield item
        }
        |> ScrollContainer
        |> this.Add

        base.Init parent
        what_to_focus.Focus false

    member this.Close() = options.OnClose()
    member private this.Items = flow

    member this.Place(x, y, width) =
        this.Position <- Position.Box(0.0f, 0.0f, x, y, width, this.Height)

    // todo: use IHeight interface
    member this.Height = float32 (Seq.length options.Items) * Dropdown.ITEMSIZE

type DropdownMenuOptions =
    {
        Items: ((unit -> unit) * string) seq
        OnClose: unit -> unit
    }

type DropdownMenu(options: DropdownMenuOptions) as this =
    inherit
        FrameContainer(NodeType.Container(fun _ -> Some this.Items), Fill = !%Palette.DARK, Border = !%Palette.LIGHT)

    let flow = FlowContainer.Vertical(Dropdown.ITEMSIZE, Floating = true)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if
            (%%"exit").Tapped()
            || not this.Focused
            || Mouse.left_click ()
            || Mouse.right_click ()
        then
            this.Close()

        if Mouse.hover this.Bounds then
            Input.finish_frame_events ()

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
                            this.Close()
                    )
        }
        |> ScrollContainer
        |> this.Add

        base.Init parent
        this.Focus false

    member this.Close() = options.OnClose()
    member private this.Items = flow

    member this.Place(x, y, width) =
        this.Position <- Position.Box(0.0f, 0.0f, x, y, width, this.Height)

    member this.Height = float32 (Seq.length options.Items) * Dropdown.ITEMSIZE
