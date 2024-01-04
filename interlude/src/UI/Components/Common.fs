﻿namespace Interlude.UI.Components

open System
open OpenTK.Mathematics
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts.Sorting
open Interlude.UI
open Interlude.Utils

type TextEntry(setting: Setting<string>, hotkey: Hotkey) as this =
    inherit StaticContainer(NodeType.Leaf)

    let ticker = Animation.Counter(600.0)

    let toggle () =
        if this.Selected then this.Focus() else this.Select()

    member val Clickable = true with get, set

    member val ColorFunc =
        fun () ->
            Colors.white,
            (if this.Selected then
                 Colors.pink_shadow
             else
                 Colors.shadow_1) with get, set

    override this.Init(parent) =
        base.Init parent

        this
        |+ Text(
            (fun () -> setting.Get() + if this.Selected && ticker.Loops % 2 = 0 then "_" else ""),
            Align = Alignment.LEFT,
            Color = this.ColorFunc
        )
        |* HotkeyAction(hotkey, toggle)

        if this.Clickable then
            this.Add(
                let c = Clickable.Focus this in
                c.OnRightClick <- (fun () -> setting.Set "")
                c
            )

    override this.OnSelected() =
        base.OnSelected()
        Style.text_open.Play()

        Input.listen_to_text (
            setting |> Setting.trigger (fun v -> Style.key.Play()),
            fun () ->
                if this.Selected then
                    this.Focus()
        )

    override this.OnDeselected() =
        base.OnDeselected()
        Style.text_close.Play()
        Input.remove_listener ()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        ticker.Update(elapsed_ms)

type StylishButton(on_click, label_func: unit -> string, color_func) as this =
    inherit
        StaticContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                on_click ()
            )
        )

    member val Hotkey: Hotkey = "none" with get, set
    member val TiltLeft = true with get, set
    member val TiltRight = true with get, set

    member val TextColor =
        fun () -> (if this.Focused then Colors.yellow_accent else Colors.grey_1), Colors.shadow_2 with get, set

    override this.Draw() =
        let h = this.Bounds.Height

        Draw.untextured_quad
            (Quad.create
             <| Vector2(this.Bounds.Left, this.Bounds.Top)
             <| Vector2(this.Bounds.Right + (if this.TiltRight then h * 0.5f else 0.0f), this.Bounds.Top)
             <| Vector2(this.Bounds.Right, this.Bounds.Bottom)
             <| Vector2(this.Bounds.Left - (if this.TiltLeft then h * 0.5f else 0.0f), this.Bounds.Bottom))
            (color_func () |> Quad.color)

        Text.fill_b (Style.font, label_func (), this.Bounds, this.TextColor(), 0.5f)
        base.Draw()

    override this.Init(parent: Widget) =
        this |+ Clickable.Focus this
        |* HotkeyAction(
            this.Hotkey,
            fun () ->
                Style.click.Play()
                on_click ()
        )

        base.Init parent

    override this.OnFocus() =
        Style.hover.Play()
        base.OnFocus()

    static member Selector<'T>(label: string, values: ('T * string) array, setting: Setting<'T>, color_func) =
        let mutable current = array.IndexOf(values |> Array.map fst, setting.Value)
        current <- max 0 current

        StylishButton(
            (fun () ->
                current <- (current + 1) % values.Length
                setting.Value <- fst values.[current]
            ),
            (fun () -> sprintf "%s %s" label (snd values.[current])),
            color_func
        )

type InlaidButton(label, action, icon) =
    inherit
        StaticContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                action ()
            )
        )

    member val Hotkey = "none" with get, set
    member val HoverText = label with get, set
    member val HoverIcon = icon with get, set
    member val UnfocusedColor = Colors.text_greyout with get, set

    override this.Init(parent) =
        this |+ Clickable.Focus this
        |* HotkeyAction(
            this.Hotkey,
            fun () ->
                Style.click.Play()
                action ()
        )

        base.Init parent

    override this.OnFocus() =
        Style.hover.Play()
        base.OnFocus()

    override this.Draw() =
        let area = this.Bounds.TrimBottom(15.0f)

        let text =
            if this.Focused then
                sprintf "%s %s" this.HoverIcon this.HoverText
            else
                sprintf "%s %s" icon label

        Draw.rect area (Colors.shadow_1.O2)

        Text.fill_b (
            Style.font,
            text,
            area.Shrink(10.0f, 5.0f),
            (if this.Focused then
                 Colors.text_yellow_2
             else
                 this.UnfocusedColor),
            Alignment.CENTER
        )

        base.Draw()

type SearchBox(s: Setting<string>, callback: unit -> unit) as this =
    inherit Frame(NodeType.Switch(fun _ -> this.TextEntry))
    let search_timer = new Diagnostics.Stopwatch()

    let text_entry =
        TextEntry(
            s |> Setting.trigger (fun _ -> this.StartSearch()),
            "search",
            Position = Position.Margin(10.0f, 0.0f),
            ColorFunc =
                fun () ->
                    (if this.TextEntry.Selected then
                         Colors.white
                     else
                         !*Palette.LIGHT),
                    !*Palette.DARKER
        )

    member val DebounceTime = 400L with get, set

    new(s: Setting<string>, callback: Filter -> unit) = SearchBox(s, (fun () -> callback (Filter.parse s.Value)))

    member private this.StartSearch() = search_timer.Restart()

    member private this.TextEntry: TextEntry = text_entry

    override this.Init(parent) =
        this.Fill <-
            fun () ->
                if this.TextEntry.Selected then
                    Colors.yellow_accent.O1
                else
                    !*Palette.DARK

        this.Border <-
            fun () ->
                if this.TextEntry.Selected then
                    Colors.yellow_accent
                else
                    !*Palette.LIGHT

        this |+ text_entry
        |* Text(
            fun () ->
                match s.Value with
                | "" -> Icons.SEARCH + " " + [ (%%"search").ToString() ] %> "misc.search"
                | _ -> ""
            , Color = text_entry.ColorFunc
            , Align = Alignment.LEFT
            , Position = Position.Margin(10.0f, 0.0f)
        )

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if search_timer.ElapsedMilliseconds > this.DebounceTime then
            search_timer.Reset()
            callback ()

type WIP() as this =
    inherit StaticWidget(NodeType.None)

    let text = %"misc.wip"

    do this.Position <- Position.SliceBottom(100.0f)

    override this.Draw() =
        Draw.rect this.Bounds (Color.FromArgb(127, Color.Yellow))
        let w = this.Bounds.Width / 20.0f

        for i = 0 to 19 do
            Draw.rect
                (Rect.Box(this.Bounds.Left + w * float32 i, this.Bounds.Top, w, 10.0f))
                (if i % 2 = 0 then Color.Yellow else Color.Black)

            Draw.rect
                (Rect.Box(this.Bounds.Left + w * float32 i, this.Bounds.Bottom - 10.0f, w, 10.0f))
                (if i % 2 = 1 then Color.Yellow else Color.Black)

        Text.fill_b (Style.font, text, this.Bounds.Shrink(20.0f), Colors.text, Alignment.CENTER)

type EmptyState(icon: string, text: string) =
    inherit StaticWidget(NodeType.None)

    member val Subtitle = "" with get, set

    override this.Draw() =
        let color = (!*Palette.LIGHT, !*Palette.DARKER)
        Text.fill_b (Style.font, icon, this.Bounds.Shrink(30.0f, 100.0f).SliceTop(200.0f), color, Alignment.CENTER)

        Text.fill_b (
            Style.font,
            text,
            this.Bounds.Shrink(30.0f, 100.0f).TrimTop(175.0f).SliceTop(60.0f),
            color,
            Alignment.CENTER
        )

        Text.fill_b (
            Style.font,
            this.Subtitle,
            this.Bounds.Shrink(30.0f, 100.0f).TrimTop(230.0f).SliceTop(40.0f),
            color,
            Alignment.CENTER
        )

type LoadingState() =
    inherit StaticWidget(NodeType.None)

    let animation = Animation.Counter(250.0)

    let animation_frames =
        [|
            Icons.CLOUD_SNOW
            Icons.CLOUD_DRIZZLE
            Icons.CLOUD_RAIN
            Icons.CLOUD_DRIZZLE
        |]

    member val Text = %"misc.loading" with get, set

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        animation.Update elapsed_ms

    override this.Draw() =
        let color = (!*Palette.LIGHT, !*Palette.DARKER)
        let icon = animation_frames.[animation.Loops % animation_frames.Length]
        Text.fill_b (Style.font, icon, this.Bounds.Shrink(30.0f, 100.0f).SliceTop(200.0f), color, Alignment.CENTER)

        Text.fill_b (
            Style.font,
            this.Text,
            this.Bounds.Shrink(30.0f, 100.0f).TrimTop(175.0f).SliceTop(60.0f),
            color,
            Alignment.CENTER
        )

type LoadingIndicator() =
    inherit StaticWidget(NodeType.None)

    let animation = Animation.Counter(1500.0)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        animation.Update elapsed_ms

    override this.Draw() =
        let tick_width = this.Bounds.Width * 0.2f

        let pos =
            -tick_width
            + (this.Bounds.Width + tick_width) * float32 animation.Time / 1500.0f

        Draw.rect this.Bounds !*Palette.DARK

        Draw.rect
            (Rect.Create(
                this.Bounds.Left + max 0.0f pos,
                this.Bounds.Top,
                this.Bounds.Left + min this.Bounds.Width (pos + tick_width),
                this.Bounds.Bottom
            ))
            !*Palette.LIGHT

type NewAndShiny() =
    inherit StaticWidget(NodeType.None)

    member val Icon = Icons.ALERT_CIRCLE with get, set

    override this.Draw() =
        let x, y = this.Bounds.Right, this.Bounds.Bottom // todo: alignment options
        let r = 18f
        let angle = MathF.PI / 15.0f

        let vec i =
            let angle = float32 i * angle
            let struct (a, b) = MathF.SinCos(angle)
            (x + r * a, y - r * b)

        for i = 0 to 29 do
            Draw.untextured_quad (Quad.createv (x, y) (x, y) (vec i) (vec (i + 1))) (Quad.color Colors.red_accent)

        Text.fill_b (Style.font, this.Icon, Rect.Box(x, y, 0.0f, 0.0f).Expand(r), Colors.text, Alignment.CENTER)
