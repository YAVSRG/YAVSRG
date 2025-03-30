namespace Interlude.UI

open System.Runtime.CompilerServices
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude

[<RequireQualifiedAccess>]
type CalloutContent =
    | Header of string
    | Body of string[]
    | Hotkey of string option * Hotkey
    | Button of string * (unit -> unit)

type CalloutSize =
    | Large
    | Small
    | Tiny

type Callout =
    {
        Size: CalloutSize
        Contents: CalloutContent list
        _Icon: string option
    }

    static member Tiny : Callout =
        {
            Size = Tiny
            Contents = []
            _Icon = None
        }

    static member Small : Callout =
        {
            Size = Small
            Contents = []
            _Icon = None
        }

    static member Normal : Callout =
        {
            Size = Large
            Contents = []
            _Icon = None
        }

    member this.Title(s: string) : Callout =
        if s <> "" then
            { this with
                Contents = this.Contents @ [ CalloutContent.Header s ]
            }
        else
            this

    member this.Body(s: string) : Callout=
        if s <> "" then
            { this with
                Contents = this.Contents @ [ CalloutContent.Body(s.Split "\n") ]
            }
        else
            this

    member this.Hotkey(h: Hotkey) : Callout =
        assert(Hotkeys.get h |> K true)
        { this with
            Contents = this.Contents @ [ CalloutContent.Hotkey(None, h) ]
        }

    member this.Hotkey(desc: string, h: Hotkey) : Callout =
        assert(Hotkeys.get h |> K true)
        { this with
            Contents = this.Contents @ [ CalloutContent.Hotkey(Some desc, h) ]
        }

    member this.Icon(icon: string) : Callout =
        if icon <> "" then
            { this with _Icon = Some icon }
        else
            { this with _Icon = None }

    member this.Button(label: string, action: unit -> unit) : Callout =
        { this with
            Contents = this.Contents @ [ CalloutContent.Button(label, action) ]
        }

module Callout =

    let private PADDING_X = 30.0f
    let private PADDING_Y = 20.0f
    let private spacing : CalloutSize -> float32 =
        function
        | Tiny -> 10.0f
        | Small -> 10.0f
        | Large -> 15.0f
    let private header_size : CalloutSize -> float32 =
        function
        | Tiny -> 20.0f
        | Small -> 25.0f
        | Large -> 35.0f
    let private text_size : CalloutSize -> float32 =
        function
        | Tiny -> 15.0f
        | Small -> 18.0f
        | Large -> 25.0f
    let private text_spacing : CalloutSize -> float32 =
        function
        | Tiny -> 6.0f
        | Small -> 8.0f
        | Large -> 10.0f
    let private icon_size : CalloutSize -> float32 =
        function
        | Tiny -> 25.0f
        | Small -> 30.0f
        | Large -> 50.0f

    let private DEFAULT_HOTKEY_TEXT = %"misc.hotkeyhint"

    let measure (c: Callout) : float32 * float32 =
        let spacing = spacing c.Size
        let mutable width = 0.0f
        let mutable height = PADDING_Y * 2.0f

        for b in c.Contents do
            match b with
            | CalloutContent.Header text ->
                let size = header_size c.Size
                height <- height + size
                width <- max width (Text.measure (Style.font, text) * size)
            | CalloutContent.Body xs ->
                let size = text_size c.Size

                height <-
                    height
                    + (float32 xs.Length * size)
                    + (float32 (xs.Length - 1) * text_spacing c.Size)

                for x in xs do
                    width <- max width (Text.measure (Style.font, x) * size)
            | CalloutContent.Hotkey _ -> height <- height + text_size c.Size
            | CalloutContent.Button (text, _) ->
                width <- max width (Text.measure (Style.font, text) * text_size c.Size + spacing * 2.0f)
                height <- height + spacing * 3.0f + text_size c.Size

            height <- height + spacing

        let icon_size =
            if c._Icon.IsSome then
                min (icon_size c.Size) height + PADDING_X * 2.0f
            else
                PADDING_X

        width + icon_size + PADDING_X, height

    let draw (x, y, width, height, col, callout: Callout) : unit =
        let x, width =
            match callout._Icon with
            | Some i ->
                let icon_size = min (icon_size callout.Size) height
                Text.draw_b (Style.font, i, icon_size, x + 30.0f, y + height * 0.5f - icon_size * 0.7f, col)
                x + icon_size + PADDING_X * 2.0f, width - icon_size - PADDING_X * 3.0f
            | None -> x + PADDING_X, width - PADDING_X * 2.0f

        let spacing = spacing callout.Size
        let mutable y = y + PADDING_Y
        let a = int (fst col).A

        for b in callout.Contents do
            match b with
            | CalloutContent.Header s ->
                let size = header_size callout.Size
                Text.draw_b (Style.font, s, size, x, y, col)
                y <- y + size
            | CalloutContent.Body xs ->
                let size = text_size callout.Size
                let tspacing = text_spacing callout.Size

                for line in xs do
                    Text.draw_b (Style.font, line, size, x, y, col)
                    y <- y + size
                    y <- y + tspacing

                y <- y - tspacing
            | CalloutContent.Hotkey(desc, hotkey) ->
                let size = text_size callout.Size

                let text = sprintf "%s: %O" (Option.defaultValue DEFAULT_HOTKEY_TEXT desc) %%hotkey

                Text.draw_b (Style.font, text, size, x, y, (Colors.cyan_accent.O4a a, Colors.shadow_2.O4a a))
                y <- y + size
            | CalloutContent.Button(label, _) ->
                y <- y + spacing
                let tsize = text_size callout.Size
                let button_size = tsize + spacing * 2.0f
                let bounds = Rect.FromSize(x, y, width, button_size)
                Render.rect bounds (Colors.shadow_2.O2a a)

                let text_col =
                    if bounds.Contains(Mouse.pos ()) then
                        (Colors.yellow_accent.O4a a, Colors.shadow_2.O4a a)
                    else
                        col

                Text.draw_aligned_b (
                    Style.font,
                    label,
                    tsize,
                    x + width * 0.5f,
                    y + button_size * 0.5f - tsize * 0.8f,
                    text_col,
                    Alignment.CENTER
                )

                y <- y + button_size

            y <- y + spacing

    let update (x: float32, y: float32, width: float32, height: float32, callout: Callout) : unit =
        let x, width =
            match callout._Icon with
            | Some i ->
                let icon_size = min (icon_size callout.Size) height
                x + icon_size + PADDING_X * 2.0f, width - icon_size - PADDING_X * 3.0f
            | None -> x + PADDING_X, width - PADDING_X * 2.0f

        let spacing = spacing callout.Size
        let mutable y = y

        for b in callout.Contents do
            match b with
            | CalloutContent.Header s -> y <- y + header_size callout.Size
            | CalloutContent.Body xs ->
                y <-
                    y
                    + (float32 xs.Length * text_size callout.Size)
                    + (float32 (xs.Length - 1) * text_spacing callout.Size)
            | CalloutContent.Hotkey(desc, hk) -> y <- y + text_size callout.Size
            | CalloutContent.Button(_, action) ->
                y <- y + spacing
                let tsize = text_size callout.Size
                let button_size = tsize + spacing * 2.0f
                let bounds = Rect.FromSize(x, y, width, button_size)

                if Mouse.hover bounds && Mouse.left_clicked () then
                    action ()

                y <- y + button_size

            y <- y + spacing

[<Sealed>]
type CalloutCard(callout: Callout, border: Color, fill: Color) =
    inherit StaticWidget(NodeType.None)

    let w, h = Callout.measure callout

    new(callout: Callout) = CalloutCard(callout, Colors.cyan_accent, Colors.cyan.O3)

    member val ContentColor = Colors.text with get, set

    override this.Draw() =

        Render.border Style.PADDING this.Bounds border
        Render.rect this.Bounds fill

        Callout.draw (this.Bounds.Left, this.Bounds.CenterY - h * 0.5f, w, h, this.ContentColor, callout)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        Callout.update (this.Bounds.Left, this.Bounds.CenterY - h * 0.5f, w, h, callout)

    interface IHeight with
        member _.Height = h

    interface IWidth with
        member _.Width = w

[<Extension>]
type CalloutCardExtensions =

    [<Extension>]
    static member Position (card: CalloutCard, position: float32 * float32 -> Position) : CalloutCard =
        card.Position <- position((card :> IWidth).Width, (card :> IHeight).Height)
        card