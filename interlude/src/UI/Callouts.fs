namespace Interlude.UI

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

type Callout =
    {
        IsSmall: bool
        Contents: CalloutContent list
        _Icon: string option
    }
    static member Small =
        {
            IsSmall = true
            Contents = []
            _Icon = None
        }

    static member Normal =
        {
            IsSmall = false
            Contents = []
            _Icon = None
        }

    member this.Title(s: string) =
        if s <> "" then
            { this with
                Contents = this.Contents @ [ CalloutContent.Header s ]
            }
        else
            this

    member this.Body(s: string) =
        if s <> "" then
            { this with
                Contents = this.Contents @ [ CalloutContent.Body(s.Split "\n") ]
            }
        else
            this

    member this.Hotkey(h: Hotkey) =
        assert(Hotkeys.get h |> K true)
        { this with
            Contents = this.Contents @ [ CalloutContent.Hotkey(None, h) ]
        }

    member this.Hotkey(desc: string, h: Hotkey) =
        assert(Hotkeys.get h |> K true)
        { this with
            Contents = this.Contents @ [ CalloutContent.Hotkey(Some desc, h) ]
        }

    member this.Icon(icon: string) =
        if icon <> "" then
            { this with _Icon = Some icon }
        else
            { this with _Icon = None }

    member this.Button(label: string, action: unit -> unit) =
        { this with
            Contents = this.Contents @ [ CalloutContent.Button(label, action) ]
        }

module Callout =

    let private PADDING_X = 30.0f
    let private PADDING_Y = 20.0f
    let private spacing is_small = if is_small then 10.0f else 15.0f
    let private header_size is_small = if is_small then 25.0f else 35.0f
    let private text_size is_small = if is_small then 18.0f else 25.0f
    let private text_spacing is_small = if is_small then 8.0f else 10.0f

    let private DEFAULT_HOTKEY_TEXT = %"misc.hotkeyhint"

    let measure (c: Callout) : float32 * float32 =
        let spacing = spacing c.IsSmall
        let mutable width = 0.0f
        let mutable height = PADDING_Y * 2.0f

        for b in c.Contents do
            match b with
            | CalloutContent.Header text ->
                let size = header_size c.IsSmall
                height <- height + size
                width <- max width (Text.measure (Style.font, text) * size)
            | CalloutContent.Body xs ->
                let size = text_size c.IsSmall

                height <-
                    height
                    + (float32 xs.Length * size)
                    + (float32 (xs.Length - 1) * text_spacing c.IsSmall)

                for x in xs do
                    width <- max width (Text.measure (Style.font, x) * size)
            | CalloutContent.Hotkey _ -> height <- height + text_size c.IsSmall
            | CalloutContent.Button (text, _) ->
                width <- max width (Text.measure (Style.font, text) * text_size c.IsSmall + spacing * 2.0f)
                height <- height + spacing * 3.0f + text_size c.IsSmall

            height <- height + spacing

        let icon_size =
            if c._Icon.IsSome then
                min (if c.IsSmall then 30.0f else 50.0f) height + PADDING_X * 2.0f
            else
                PADDING_X

        width + icon_size + PADDING_X, height

    let draw (x, y, width, height, col, c: Callout) =
        let x, width =
            match c._Icon with
            | Some i ->
                let icon_size = min (if c.IsSmall then 30.0f else 50.0f) height
                Text.draw_b (Style.font, i, icon_size, x + 30.0f, y + height * 0.5f - icon_size * 0.7f, col)
                x + icon_size + PADDING_X * 2.0f, width - icon_size - PADDING_X * 3.0f
            | None -> x + PADDING_X, width - PADDING_X * 2.0f

        let spacing = spacing c.IsSmall
        let mutable y = y + PADDING_Y
        let a = int (fst col).A

        for b in c.Contents do
            match b with
            | CalloutContent.Header s ->
                let size = header_size c.IsSmall
                Text.draw_b (Style.font, s, size, x, y, col)
                y <- y + size
            | CalloutContent.Body xs ->
                let size = text_size c.IsSmall
                let tspacing = text_spacing c.IsSmall

                for line in xs do
                    Text.draw_b (Style.font, line, size, x, y, col)
                    y <- y + size
                    y <- y + tspacing

                y <- y - tspacing
            | CalloutContent.Hotkey(desc, hotkey) ->
                let size = text_size c.IsSmall

                let text = sprintf "%s: %O" (Option.defaultValue DEFAULT_HOTKEY_TEXT desc) %%hotkey

                Text.draw_b (Style.font, text, size, x, y, (Colors.cyan_accent.O4a a, Colors.shadow_2.O4a a))
                y <- y + size
            | CalloutContent.Button(label, _) ->
                y <- y + spacing
                let tsize = text_size c.IsSmall
                let button_size = tsize + spacing * 2.0f
                let bounds = Rect.Box(x, y, width, button_size)
                Draw.rect bounds (Colors.shadow_2.O2a a)

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

    let update (x, y, width, height, c) =
        let x, width =
            match c._Icon with
            | Some i ->
                let icon_size = min (if c.IsSmall then 30.0f else 50.0f) height
                x + icon_size + PADDING_X * 2.0f, width - icon_size - PADDING_X * 3.0f
            | None -> x + PADDING_X, width - PADDING_X * 2.0f

        let spacing = spacing c.IsSmall
        let mutable y = y

        for b in c.Contents do
            match b with
            | CalloutContent.Header s -> y <- y + header_size c.IsSmall
            | CalloutContent.Body xs ->
                y <-
                    y
                    + (float32 xs.Length * text_size c.IsSmall)
                    + (float32 (xs.Length - 1) * text_spacing c.IsSmall)
            | CalloutContent.Hotkey(desc, hk) -> y <- y + text_size c.IsSmall
            | CalloutContent.Button(_, action) ->
                y <- y + spacing
                let tsize = text_size c.IsSmall
                let button_size = tsize + spacing * 2.0f
                let bounds = Rect.Box(x, y, width, button_size)

                if Mouse.hover bounds && Mouse.left_click () then
                    action ()

                y <- y + button_size

            y <- y + spacing

    // todo: get rid of me, use CalloutCard instead
    let frame (callout: Callout) (pos: float32 * float32 -> Position) =
        let w, h = measure callout

        { new FrameContainer(NodeType.None,
                             Fill = K Colors.cyan.O3,
                             Border = K Colors.cyan_accent,
                             Position = pos (w, h)) with
            override this.Draw() =
                base.Draw()
                draw (this.Bounds.Left, this.Bounds.CenterY - h * 0.5f, w, h, Colors.text, callout)

            override this.Update(elapsed_ms, moved) =
                base.Update(elapsed_ms, moved)
                update (this.Bounds.Left, this.Bounds.CenterY - h * 0.5f, w, h, callout)
        }

type CalloutCard(callout: Callout, border: Color, fill: Color) =
    inherit StaticWidget(NodeType.None)

    let w, h = Callout.measure callout

    member val ContentColor = Colors.text with get, set

    override this.Draw() =

        Draw.rect (this.Bounds.BorderTopCorners Style.PADDING) border
        Draw.rect (this.Bounds.BorderBottomCorners Style.PADDING) border
        Draw.rect (this.Bounds.BorderLeft Style.PADDING) border
        Draw.rect (this.Bounds.BorderRight Style.PADDING) border

        Draw.rect this.Bounds fill

        Callout.draw (this.Bounds.Left, this.Bounds.Top, w, h, this.ContentColor, callout)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        Callout.update (this.Bounds.Left, this.Bounds.Top, w, h, callout)

    interface IHeight with
        member _.Height = h

    interface IWidth with
        member _.Width = w