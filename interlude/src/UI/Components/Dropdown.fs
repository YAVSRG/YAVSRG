namespace Interlude.UI.Components

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common

module private Dropdown =

    let ITEMSIZE = 60.0f

    type Item(label: string, color: Color * Color, onclick: unit -> unit) as this =
        inherit
            StaticContainer(
                NodeType.Button(fun () ->
                    Style.click.Play()
                    onclick ()
                )
            )

        do
            this
            |+ Clickable(
                this.Select,
                OnHover =
                    (fun b ->
                        if b && not this.Focused then
                            this.Focus()
                    ),
                Floating = true
            )
            |* Text(label, Align = Alignment.LEFT, Position = Position.Margin(10.0f, 5.0f), Color = K color)

        override this.OnFocus() =
            Style.hover.Play()
            base.OnFocus()

        override this.Draw() =
            if this.Focused then
                Draw.rect this.Bounds (!*Palette.HOVER)

            base.Draw()

type Dropdown(items: (string * (Color * Color) * (unit -> unit)) seq, on_close: unit -> unit) as this =
    inherit FrameContainer(NodeType.Switch(fun _ -> this.Items), Fill = !%Palette.DARK, Border = !%Palette.LIGHT)

    let flow = FlowContainer.Vertical(Dropdown.ITEMSIZE, Floating = true)
    let scroll = ScrollContainer(flow)

    do
        for (label, color, action) in items do
            flow
            |* Dropdown.Item(
                label,
                color,
                fun () ->
                    action ()
                    this.Close()
            )

        this.Add scroll

    new(items: (string * (unit -> unit)) seq, on_close: unit -> unit) =
        Dropdown(items |> Seq.map (fun (label, action) -> (label, Colors.text, action)), on_close)

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

        base.Init parent
        this.Focus()

    member this.Close() = on_close ()
    member private this.Items = flow

    member this.Place(x, y, width) =
        this.Position <- Position.Box(0.0f, 0.0f, x, y, width, this.Height)

    member this.Height = float32 (Seq.length items) * Dropdown.ITEMSIZE

    static member Selector (items: 'T seq) (labels: 'T -> string) (on_select: 'T -> unit) (on_close: unit -> unit) =
        Dropdown(Seq.map (fun item -> (labels item, Colors.text, (fun () -> on_select item))) items, on_close)

    static member ColorSelector
        (items: 'T seq)
        (labels: 'T -> string)
        (colors: 'T -> Color * Color)
        (on_select: 'T -> unit)
        (on_close: unit -> unit)
        =
        Dropdown(Seq.map (fun item -> (labels item, colors item, (fun () -> on_select item))) items, on_close)
