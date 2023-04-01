namespace Interlude.UI

open System
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude.UI
open Interlude.Utils

type CalloutContent =
    | Header of string
    | Body of string[]
    | Hotkey of string option * Hotkey

type Callout =
    {
        IsSmall: bool
        Contents: CalloutContent list
        _Icon: string option
    }
    static member Small = { IsSmall = true; Contents = []; _Icon = None }
    static member Normal = { IsSmall = false; Contents = []; _Icon = None }
    member this.Title(s: string) = if s <> "" then { this with Contents = this.Contents @ [Header s] } else this
    member this.Body(s: string) = if s <> "" then { this with Contents = this.Contents @ [Body (s.Split "\n")] } else this
    member this.Hotkey(h: Hotkey) = { this with Contents = this.Contents @ [Hotkey (None, h)] }
    member this.Hotkey(desc: string, h: Hotkey) = { this with Contents = this.Contents @ [Hotkey (Some desc, h)] }
    member this.Icon(icon: string) = if icon <> "" then { this with _Icon = Some icon } else { this with _Icon = None }

module Callout =

    let x_padding = 30.0f
    let spacing isSmall = if isSmall then 10.0f else 15.0f
    let header_size isSmall = if isSmall then 25.0f else 35.0f
    let text_size isSmall = if isSmall then 18.0f else 25.0f
    let text_spacing isSmall = if isSmall then 8.0f else 10.0f

    let default_hotkey_text = L"misc.hotkeyhint"

    let measure (c: Callout) : float32 * float32 =
        let spacing = spacing c.IsSmall
        let mutable width = 0.0f
        let mutable height = 0.0f
        for b in c.Contents do
            match b with
            | Header text -> 
                let size = header_size c.IsSmall
                height <- height + size
                width <- max width (Text.measure (Style.baseFont, text) * size)
            | Body xs ->
                let size = text_size c.IsSmall
                height <- height + (float32 xs.Length * size) + (float32 (xs.Length - 1) * text_spacing c.IsSmall)
                for x in xs do width <- max width (Text.measure (Style.baseFont, x) * size)
            | Hotkey _ -> height <- height + text_size c.IsSmall
            height <- height + spacing
        let icon_size =
            if c._Icon.IsSome then
                min (if c.IsSmall then 30.0f else 50.0f) height + x_padding * 2.0f
            else x_padding
                
        width + icon_size + x_padding, height

    let draw (x, y, height, col, c: Callout) =
        let x =
            match c._Icon with
            | Some i ->
                let icon_size = min (if c.IsSmall then 30.0f else 50.0f) height
                Text.drawB(Style.baseFont, i, icon_size, x + 30.0f, y + height * 0.5f - icon_size * 0.7f, col)
                x + icon_size + x_padding * 2.0f
            | None -> x + x_padding
        let spacing = spacing c.IsSmall
        let mutable y = y
        for b in c.Contents do
            match b with
            | Header s ->
                let size = header_size c.IsSmall
                Text.drawB(Style.baseFont, s, size, x, y, col)
                y <- y + size
            | Body xs ->
                let size = text_size c.IsSmall
                let tspacing = text_spacing c.IsSmall
                for line in xs do
                    Text.drawB(Style.baseFont, line, size, x, y, col)
                    y <- y + size
                    y <- y + tspacing
                y <- y - tspacing
            | Hotkey (desc, hk) ->
                let size = text_size c.IsSmall
                let text = sprintf "%s: %O" (Option.defaultValue default_hotkey_text desc) (!|hk)
                let a = int (fst col).A
                Text.drawB(
                    Style.baseFont,
                    text,
                    size, x, y,
                    (Colors.cyan_accent.O4a a, Colors.shadow_2.O4a a))
                y <- y + size
            y <- y + spacing

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

    let mutable private current_tooltip : Tooltip option = None
    let private items = ResizeArray<Notification>()
    let mutable tooltip_available = false

    type Display() =
        inherit Overlay(NodeType.None)

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            match current_tooltip with
            | None -> ()
            | Some t ->
                t.Fade.Update elapsedTime
                if t.Fade.Target <> 0.0f then
                    if t.Bind.Released() then t.Fade.Target <- 0.0f
                elif t.Fade.Value < 0.01f then current_tooltip <- None

            for i in items do
                i.Duration <- i.Duration - elapsedTime
                i.Fade.Update elapsedTime
                if i.Fade.Target <> 0.0f then
                    if i.Duration <= 0.0 then i.Fade.Target <- 0.0f
                elif i.Fade.Value < 0.01f then sync(fun () -> items.Remove i |> ignore)

            tooltip_available <- false

        override this.Draw() =
            match current_tooltip with
            | None -> ()
            | Some t ->
                let outline = t.Target.Bounds.Expand(20.0f).Intersect(Viewport.bounds)
                let c l = Math.Clamp((t.Fade.Value - l) / 0.25f, 0.0f, 1.0f)
                // border around thing
                Draw.rect (outline.SliceLeft(Style.padding).SliceBottom(outline.Height * c 0.0f)) (Colors.yellow_accent.O3a t.Fade.Alpha)
                Draw.rect (outline.SliceTop(Style.padding).SliceLeft(outline.Width * c 0.25f)) (Colors.yellow_accent.O3a t.Fade.Alpha)
                Draw.rect (outline.SliceRight(Style.padding).SliceTop(outline.Height * c 0.5f)) (Colors.yellow_accent.O3a t.Fade.Alpha)
                Draw.rect (outline.SliceBottom(Style.padding).SliceRight(outline.Width * c 0.75f)) (Colors.yellow_accent.O3a t.Fade.Alpha)
                // blackout effect
                Draw.rect (Viewport.bounds.SliceLeft outline.Left) (Colors.shadow_2.O3a t.Fade.Alpha)
                Draw.rect (Viewport.bounds.TrimLeft outline.Right) (Colors.shadow_2.O3a t.Fade.Alpha)
                Draw.rect (Viewport.bounds.TrimLeft(outline.Left).SliceLeft(outline.Width).SliceTop(outline.Top)) (Colors.shadow_2.O3a t.Fade.Alpha)
                Draw.rect (Viewport.bounds.TrimLeft(outline.Left).SliceLeft(outline.Width).TrimTop(outline.Bottom)) (Colors.shadow_2.O3a t.Fade.Alpha)
                let width, height = t.Size
                let x = 
                    outline.CenterX - width * 0.5f
                    |> min (Viewport.bounds.Width - width - 50.0f)
                    |> max 50.0f
                let y =
                    if outline.Top > Viewport.bounds.CenterY then
                        outline.Top - 50.0f - height - 60.0f
                    else outline.Bottom + 50.0f
                let calloutBounds = Rect.Box(x, y, width, height + 60.0f)
                Draw.rect calloutBounds (Colors.cyan.O3a t.Fade.Alpha)
                let frameBounds = calloutBounds.Expand(5.0f)
                Draw.rect (frameBounds.SliceTop 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)
                Draw.rect (frameBounds.SliceBottom 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)
                Draw.rect (frameBounds.SliceLeft 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)
                Draw.rect (frameBounds.SliceRight 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)
                Callout.draw (calloutBounds.Left, calloutBounds.Top + 30.0f, height, (Colors.white.O4a t.Fade.Alpha, Colors.shadow_1.O4a t.Fade.Alpha), t.Data)

            let padding = 20.0f
            let mutable y = this.Bounds.Top + 70.0f
            for i in items do
                let width, height = i.Size
                let accent, body = i.FillColor
                let bounds = Rect.Box(this.Bounds.Right - width * i.Fade.Value, y, width, height + padding * 2.0f)
                Draw.rect (bounds.Expand(5.0f, 0.0f).SliceLeft(5.0f)) (accent.O4a i.Fade.Alpha)
                Draw.rect bounds (body.O3a i.Fade.Alpha)
                Callout.draw (bounds.Left, bounds.Top + padding, height, i.ContentColor, i.Data)
                y <- y + (height + padding * 2.0f) * i.Fade.Value

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
        let n: Notification =
            {
                Data = body
                Size = Callout.measure body
                FillColor = colors
                ContentColor = content_colors
                Fade = Animation.Fade(0.0f, Target = 1.0f)
                Duration = 2000.0
            }
        if Percyqaz.Flux.Utils.isUiThread() then items.Add n else sync(fun () -> items.Add n)

    let task_feedback(icon: string, title: string, description: string) =
        add (Callout.Small.Icon(icon).Title(title).Body(description), (Colors.pink_accent, Colors.pink), Colors.text)

    let action_feedback(icon: string, title: string, description: string) =
        add (Callout.Small.Icon(icon).Title(title).Body(description), (Colors.cyan_accent, Colors.cyan), Colors.text)

    let system_feedback(icon: string, title: string, description: string) =
        add (Callout.Small.Icon(icon).Title(title).Body(description), (Colors.green_accent, Colors.green), Colors.text)

    let error(title, description) =
        add (Callout.Small.Icon(Icons.alert).Title(title).Body(description), (Colors.red_accent, Colors.red), Colors.text)
        
