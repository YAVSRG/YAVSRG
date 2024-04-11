namespace Interlude.UI

open System
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

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

    let x_padding = 30.0f
    let spacing is_small = if is_small then 10.0f else 15.0f
    let header_size is_small = if is_small then 25.0f else 35.0f
    let text_size is_small = if is_small then 18.0f else 25.0f
    let text_spacing is_small = if is_small then 8.0f else 10.0f

    let default_hotkey_text = %"misc.hotkeyhint"

    let measure (c: Callout) : float32 * float32 =
        let spacing = spacing c.IsSmall
        let mutable width = 0.0f
        let mutable height = 0.0f

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
            | CalloutContent.Button _ -> height <- height + spacing * 3.0f + text_size c.IsSmall

            height <- height + spacing

        let icon_size =
            if c._Icon.IsSome then
                min (if c.IsSmall then 30.0f else 50.0f) height + x_padding * 2.0f
            else
                x_padding

        width + icon_size + x_padding, height

    let draw (x, y, width, height, col, c: Callout) =
        let x, width =
            match c._Icon with
            | Some i ->
                let icon_size = min (if c.IsSmall then 30.0f else 50.0f) height
                Text.draw_b (Style.font, i, icon_size, x + 30.0f, y + height * 0.5f - icon_size * 0.7f, col)
                x + icon_size + x_padding * 2.0f, width - icon_size - x_padding * 3.0f
            | None -> x + x_padding, width - x_padding * 2.0f

        let spacing = spacing c.IsSmall
        let mutable y = y
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

                let text = sprintf "%s: %O" (Option.defaultValue default_hotkey_text desc) %%hotkey

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
                x + icon_size + x_padding * 2.0f, width - icon_size - x_padding * 3.0f
            | None -> x + x_padding, width - x_padding * 2.0f

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

    let frame (callout: Callout) (pos: float32 * float32 -> Position) =
        let w, h = measure callout
        let PADDING = 20.0f

        { new FrameContainer(NodeType.None,
                             Fill = K Colors.cyan.O3,
                             Border = K Colors.cyan_accent,
                             Position = pos (w, h + PADDING * 2.0f)) with
            override this.Draw() =
                base.Draw()
                draw (this.Bounds.Left, this.Bounds.CenterY - h * 0.5f, w, h, Colors.text, callout)

            override this.Update(elapsed_ms, moved) =
                base.Update(elapsed_ms, moved)
                update (this.Bounds.Left, this.Bounds.CenterY - h * 0.5f, w, h, callout)
        }

    type Card(callout: Callout, border: Color, fill: Color) =
        inherit StaticWidget(NodeType.None)

        let w, h = measure callout
        let PADDING = 20.0f

        override this.Draw() =

            Draw.rect (this.Bounds.BorderTopCorners Style.PADDING) border
            Draw.rect (this.Bounds.BorderBottomCorners Style.PADDING) border
            Draw.rect (this.Bounds.BorderLeft Style.PADDING) border
            Draw.rect (this.Bounds.BorderRight Style.PADDING) border

            Draw.rect this.Bounds fill

            draw (this.Bounds.Left, this.Bounds.Top + PADDING, w, h, Colors.text, callout)

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)
            update (this.Bounds.Left, this.Bounds.Top + PADDING, w, h, callout)

        interface IHeight with
            member _.Height = h + PADDING * 2.0f

type private Notification =
    {
        Data: Callout
        Size: float32 * float32
        Fade: Animation.Fade
        FillColor: Color * Color
        ContentColor: Color * Color
        mutable Duration: float
    }

type private Tooltip =
    {
        Data: Callout
        Size: float32 * float32
        Fade: Animation.Fade
        Target: Widget
        Bind: Bind
    }

module Notifications =

    let mutable private current_tooltip: Tooltip option = None
    let mutable tooltip_available = false
    let private items = ResizeArray<Notification>()

    type Display() =
        inherit Overlay(NodeType.None)

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            match current_tooltip with
            | None -> ()
            | Some t ->
                t.Fade.Update elapsed_ms

                if t.Fade.Target <> 0.0f then
                    if t.Bind.Released() then
                        t.Fade.Target <- 0.0f
                elif t.Fade.Value < 0.01f then
                    current_tooltip <- None

                let outline = t.Target.Bounds.Expand(20.0f).Intersect(Viewport.bounds)
                let width, height = t.Size

                let x =
                    outline.CenterX - width * 0.5f
                    |> min (Viewport.bounds.Width - width - 50.0f)
                    |> max 50.0f

                let y =
                    if outline.Top > Viewport.bounds.CenterY then
                        outline.Top - 50.0f - height - 60.0f
                    else
                        outline.Bottom + 50.0f

                let callout_bounds = Rect.Box(x, y, width, height + 60.0f)
                Callout.update (callout_bounds.Left, callout_bounds.Top + 30.0f, width, height, t.Data)

            let padding = 20.0f
            let mutable y = this.Bounds.Top + 80.0f

            for i in items do
                let width, height = i.Size

                let bounds =
                    Rect.Box(this.Bounds.Right - width - 10.0f, y, width, height + padding * 2.0f)

                Callout.update (bounds.Left, bounds.Top + padding, width, height, i.Data)
                y <- y + (height + padding * 2.0f + 15.0f) * i.Fade.Value

                i.Duration <- i.Duration - elapsed_ms
                i.Fade.Update elapsed_ms

                if i.Fade.Target <> 0.0f then
                    if i.Duration <= 0.0 then
                        i.Fade.Target <- 0.0f
                elif i.Fade.Value < 0.01f then
                    sync (fun () -> items.Remove i |> ignore)

            tooltip_available <- false

        override this.Draw() =
            match current_tooltip with
            | None -> ()
            | Some t ->
                let outline = t.Target.Bounds.Expand(20.0f).Intersect(Viewport.bounds)

                let c l =
                    Math.Clamp((t.Fade.Value - l) / 0.25f, 0.0f, 1.0f)
                // border around thing
                Draw.rect
                    (outline.SliceLeft(Style.PADDING).SliceBottom(outline.Height * c 0.0f))
                    (Colors.yellow_accent.O3a t.Fade.Alpha)

                Draw.rect
                    (outline.SliceTop(Style.PADDING).SliceLeft(outline.Width * c 0.25f))
                    (Colors.yellow_accent.O3a t.Fade.Alpha)

                Draw.rect
                    (outline.SliceRight(Style.PADDING).SliceTop(outline.Height * c 0.5f))
                    (Colors.yellow_accent.O3a t.Fade.Alpha)

                Draw.rect
                    (outline.SliceBottom(Style.PADDING).SliceRight(outline.Width * c 0.75f))
                    (Colors.yellow_accent.O3a t.Fade.Alpha)
                // blackout effect
                Draw.rect (Viewport.bounds.SliceLeft outline.Left) (Colors.shadow_2.O3a t.Fade.Alpha)
                Draw.rect (Viewport.bounds.TrimLeft outline.Right) (Colors.shadow_2.O3a t.Fade.Alpha)

                Draw.rect
                    (Viewport.bounds
                        .TrimLeft(outline.Left)
                        .SliceLeft(outline.Width)
                        .SliceTop(outline.Top))
                    (Colors.shadow_2.O3a t.Fade.Alpha)

                Draw.rect
                    (Viewport.bounds
                        .TrimLeft(outline.Left)
                        .SliceLeft(outline.Width)
                        .TrimTop(outline.Bottom))
                    (Colors.shadow_2.O3a t.Fade.Alpha)

                let width, height = t.Size

                let x =
                    outline.CenterX - width * 0.5f
                    |> min (Viewport.bounds.Width - width - 50.0f)
                    |> max 50.0f

                let y =
                    if outline.Top > Viewport.bounds.CenterY then
                        outline.Top - 50.0f - height - 60.0f
                    else
                        outline.Bottom + 50.0f

                let callout_bounds = Rect.Box(x, y, width, height + 60.0f)
                Draw.rect callout_bounds (Colors.cyan.O3a t.Fade.Alpha)
                let frame_bounds = callout_bounds.Expand(5.0f)
                Draw.rect (frame_bounds.SliceTop 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)
                Draw.rect (frame_bounds.SliceBottom 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)
                Draw.rect (frame_bounds.SliceLeft 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)
                Draw.rect (frame_bounds.SliceRight 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)

                Callout.draw (
                    callout_bounds.Left,
                    callout_bounds.Top + 30.0f,
                    width,
                    height,
                    (Colors.white.O4a t.Fade.Alpha, Colors.shadow_1.O4a t.Fade.Alpha),
                    t.Data
                )

            let padding = 20.0f
            let mutable y = this.Bounds.Top + 80.0f

            for i in items do
                let width, height = i.Size
                let accent, body = i.FillColor

                let bounds =
                    Rect.Box(this.Bounds.Right - width - 10.0f, y, width, height + padding * 2.0f)

                let border = bounds.Expand(5.0f)
                Draw.rect (border.SliceLeft(5.0f)) (accent.O4a i.Fade.Alpha)
                Draw.rect (border.SliceTop(5.0f)) (accent.O4a i.Fade.Alpha)
                Draw.rect (border.SliceRight(5.0f)) (accent.O4a i.Fade.Alpha)
                Draw.rect (border.SliceBottom(5.0f)) (accent.O4a i.Fade.Alpha)
                Draw.rect bounds (Colors.shadow_2.O2a i.Fade.Alpha)
                Draw.rect bounds (body.O3a i.Fade.Alpha)

                Callout.draw (
                    bounds.Left,
                    bounds.Top + padding,
                    width,
                    height,
                    ((fst i.ContentColor).O4a i.Fade.Alpha, (snd i.ContentColor).O4a i.Fade.Alpha),
                    i.Data
                )

                y <- y + (height + padding * 2.0f + 15.0f) * i.Fade.Value

    let display = Display()

    let tooltip (b: Bind, w: Widget, body: Callout) =
        let t: Tooltip =
            {
                Data = body
                Size = Callout.measure body
                Fade = Animation.Fade(0.0f, Target = 1.0f)
                Target = w
                Bind = b
            }

        current_tooltip <- Some t

    let private add (body: Callout, colors: Color * Color, content_colors: Color * Color) =
        ensure_ui_thread
        <| fun () ->
            let n: Notification =
                {
                    Data = body
                    Size = Callout.measure body
                    FillColor = colors
                    ContentColor = content_colors
                    Fade = Animation.Fade(0.0f, Target = 1.0f)
                    Duration = 2000.0
                }

            items.Add n

    let private add_long (body: Callout, colors: Color * Color, content_colors: Color * Color) =
        ensure_ui_thread
        <| fun () ->
            let n: Notification =
                {
                    Data = body
                    Size = Callout.measure body
                    FillColor = colors
                    ContentColor = content_colors
                    Fade = Animation.Fade(0.0f, Target = 1.0f)
                    Duration = 5000.0
                }

            items.Add n

    let task_feedback (icon: string, title: string, description: string) =
        add (Callout.Small.Icon(icon).Title(title).Body(description), (Colors.pink_accent, Colors.pink), Colors.text)
        ensure_ui_thread Style.notify_task.Play

    let action_feedback (icon: string, title: string, description: string) =
        add (Callout.Small.Icon(icon).Title(title).Body(description), (Colors.cyan_accent, Colors.cyan), Colors.text)
        ensure_ui_thread Style.notify_info.Play

    let action_feedback_button
        (
            icon: string,
            title: string,
            description: string,
            button_label: string,
            button_action: unit -> unit
        ) =
        add_long (
            Callout.Small
                .Icon(icon)
                .Title(title)
                .Body(description)
                .Button(button_label, button_action),
            (Colors.cyan_accent, Colors.cyan),
            Colors.text
        )

        ensure_ui_thread Style.notify_info.Play

    let system_feedback (icon: string, title: string, description: string) =
        add (Callout.Small.Icon(icon).Title(title).Body(description), (Colors.green_accent, Colors.green), Colors.text)
        ensure_ui_thread Style.notify_system.Play

    let error (title, description) =
        add_long (
            Callout.Small.Icon(Icons.ALERT_CIRCLE).Title(title).Body(description),
            (Colors.red_accent, Colors.red),
            Colors.text
        )

        ensure_ui_thread Style.notify_error.Play
