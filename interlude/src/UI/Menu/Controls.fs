namespace Interlude.UI

open System
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude

type Slider(setting: Setting.Bounded<float32>) as this =
    inherit Container(NodeType.Leaf)

    let TEXTWIDTH = 130.0f
    let mutable dragging = false

    let mutable decimal_places = 2
    let mutable step = 0.01f

    let get_percent () =
        let (Setting.Bounds(lo, hi)) = setting.Config
        (setting.Value - lo) / (hi - lo)

    let set_percent (v: float32) =
        let (Setting.Bounds(lo, hi)) = setting.Config
        setting.Value <- MathF.Round((hi - lo) * v + lo, decimal_places)

    let add (v) =
        setting.Value <- MathF.Round(setting.Value + v, decimal_places)
        Style.click.Play()

    do
        this
        |+ Text(
            (fun () -> this.Format setting.Value),
            Align = Alignment.LEFT,
            Position =
                { Position.DEFAULT with
                    Right = 0.0f %+ TEXTWIDTH
                }
        )
        |* Clickable(
            (fun () ->
                this.Select true
                Style.click.Play()
                dragging <- true
            ),
            OnHover =
                (fun b ->
                    if b && not this.Focused then
                        this.Focus true
                    elif not b && this.FocusedByMouse then
                        Selection.up true
                )
        )

    member this.Step
        with get () = step
        and set (value) =
            step <- value
            decimal_places <- max 0 (int (MathF.Ceiling(- MathF.Log10(step))))

    member val Format : float32 -> string = (fun x -> x.ToString("n" + decimal_places.ToString())) with get, set

    static member Percent(setting) =
        Slider(setting, Format = (fun x -> sprintf "%.0f%%" (x * 100.0f)))

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        let bounds = this.Bounds.ShrinkL TEXTWIDTH

        if dragging && Mouse.held Mouse.LEFT then
            let l, r = bounds.Left, bounds.Right
            let amt = (Mouse.x () - l) / (r - l)
            set_percent amt
        elif dragging then
            Style.click.Play()
            dragging <- false

        if this.Selected || Mouse.hover this.Bounds then
            let s = Mouse.scroll ()

            if s > 0.0f then
                setting.Value <- setting.Value + step
            elif s < 0.0f then
                setting.Value <- setting.Value - step

        if this.Selected then

            if (%%"left").Tapped() then
                add (-step)
            elif (%%"right").Tapped() then
                add (step)
            elif (%%"up").Tapped() then
                add (step * 5.0f)
            elif (%%"down").Tapped() then
                add (-step * 5.0f)

    override this.Draw() =
        let v = get_percent ()
        let bounds = this.Bounds.ShrinkL TEXTWIDTH

        let cursor_x = bounds.Left + bounds.Width * v

        Draw.rect
            (Rect.Create(cursor_x, (bounds.Top + 10.0f), bounds.Right, (bounds.Bottom - 10.0f)))
            (if this.Selected then
                 Colors.pink_shadow.O3
             else
                 Colors.grey_2.O2)

        Draw.rect
            (Rect.Create(bounds.Left, (bounds.Top + 10.0f), cursor_x, (bounds.Bottom - 10.0f)))
            (if this.Selected then Colors.pink_accent else Colors.grey_2)

        base.Draw()

type Checkbox(setting: Setting<bool>) =
    inherit Container(NodeType.Button(fun () -> setting.Value <- not setting.Value; Style.click.Play()))

    override this.Init(parent: Widget) =
        this 
        |+ Text(
            (fun () -> if setting.Value then Icons.CHECK_CIRCLE else Icons.CIRCLE),
            Color = (fun () -> if this.Focused then Colors.text_yellow_2 else Colors.text), 
            Align = Alignment.LEFT
        )
        |* Clickable(
            (fun () ->
                this.Select true
            ),
            OnHover =
                fun b ->
                    if b && not this.Focused then
                        this.Focus true
                    elif not b && this.FocusedByMouse then
                        Selection.up true
        )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

type Selector<'T>(items: ('T * string) array, setting: Setting<'T>) =
    inherit Container(NodeType.Leaf)

    let mutable index =
        items
        |> Array.tryFindIndex (fun (v, _) -> Object.Equals(v, setting.Value))
        |> Option.defaultValue 0

    let fd () =
        index <- (index + 1) % items.Length
        setting.Value <- fst items.[index]
        Style.click.Play()

    let bk () =
        index <- (index + items.Length - 1) % items.Length
        setting.Value <- fst items.[index]
        Style.click.Play()

    override this.Init(parent: Widget) =
        this 
        |+ Text((fun () -> snd items.[index]), Align = Alignment.LEFT)
        |* Clickable(
            (fun () ->
                this.Select true
                fd ()
            ),
            OnHover =
                fun b ->
                    if b && not this.Focused then
                        this.Focus true
                    elif not b && this.FocusedByMouse then
                        Selection.up true
        )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Selected then
            if (%%"left").Tapped() then
                bk ()
            elif (%%"right").Tapped() then
                fd ()
            elif (%%"up").Tapped() then
                fd ()
            elif (%%"down").Tapped() then
                bk ()

    static member FromEnum(setting: Setting<'T>) =
        let names = Enum.GetNames(typeof<'T>)
        let values = Enum.GetValues(typeof<'T>) :?> 'T array
        Selector(Array.zip values names, setting)

type SelectDropdown<'T when 'T : equality>(items: ('T * string) array, setting: Setting<'T>) as this =
    inherit Container(NodeType.Button(fun () -> this.ToggleDropdown()))

    let dropdown_wrapper = DropdownWrapper(fun d ->
        let height = min d.Height (Viewport.vheight - this.Bounds.Bottom - Style.PADDING * 2.0f)
        Position.BorderB(height + 2.0f * Style.PADDING).Shrink(Style.PADDING)
    )

    let wrapped_setting = 
        let current_value = setting.Value
        match items |> Array.tryFind (fun (v, _) -> v = current_value) with
        | Some v -> v
        | None -> items.[0]
        |> Setting.simple
        |> Setting.trigger (fun (v, _) -> setting.Set v)

    override this.Init(parent) =
        this
        |+ Text((fun () -> snd wrapped_setting.Value), Align = Alignment.LEFT)
        |+ Clickable.Focus this
        |* dropdown_wrapper

        base.Init parent

    member this.ToggleDropdown() =
        dropdown_wrapper.Toggle(fun () ->
            Dropdown
                {
                    Items = items |> Array.map (fun (v, label) -> ((v, label), label))
                    ColorFunc = K Colors.text
                    Setting = wrapped_setting
                }
        )

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()
    
    static member FromEnum(setting: Setting<'T>) =
        let names = Enum.GetNames(typeof<'T>)
        let values = Enum.GetValues(typeof<'T>) :?> 'T array
        SelectDropdown(Array.zip values names, setting)

type PageSetting(localised_text, widget: Widget) as this =
    inherit Container(NodeType.Container(fun _ -> Some this.Child))

    let mutable widget = widget

    member this.Child
        with get () = widget
        and set (w: Widget) =
            let old_widget = widget
            widget <- w
            w.Position <- Position.ShrinkL(PRETTYTEXTWIDTH).Shrink(Style.PADDING)

            if this.Initialised then
                w.Init this

                if old_widget.Focused then
                    w.Focus false

    member this.Label = localised_text

    override this.Init(parent) =
        this
        |* Text(
            localised_text + ":",
            Color = (fun () -> (if widget.Focused then Colors.text_yellow_2 else Colors.text)),
            Align = Alignment.LEFT,
            Position =
                Position
                    .Box(0.0f, 0.0f, 0.0f, 0.0f, PRETTYTEXTWIDTH - 10.0f, PRETTYHEIGHT)
                    .Shrink(Style.PADDING)
        )

        base.Init parent
        widget.Position <- Position.ShrinkL(PRETTYTEXTWIDTH).Shrink(Style.PADDING)
        widget.Init this

    override this.Draw() =
        if widget.Selected then
            Draw.rect (widget.Bounds.Expand(15.0f, Style.PADDING)) Colors.pink_accent.O2
        elif widget.Focused then
            Draw.rect (widget.Bounds.Expand(15.0f, Style.PADDING)) Colors.yellow_accent.O1

        base.Draw()
        widget.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        widget.Update(elapsed_ms, moved)

type PageButton(localised_text, action) as this =
    inherit
        Container(
            NodeType.Button(fun _ ->
                if not (this.Disabled()) then
                    Style.click.Play()
                    action ()
            )
        )

    member val Icon = "" with get, set

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K(
                if this.Icon <> "" then
                    sprintf "%s %s  >" this.Icon localised_text
                else
                    sprintf "%s  >" localised_text
            ),
            Color =
                (fun () ->
                    if not (this.Disabled()) then
                        (if this.Focused then Colors.text_yellow_2 else Colors.text)
                    else
                        Colors.text_greyout
                ),
            Align = Alignment.LEFT,
            Position = Position.Shrink(Style.PADDING)
        )
        |* Clickable.Focus this

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.Focused then
            Draw.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

    member val Disabled = K false with get, set

    static member Once(localised_text, action) =
        let mutable clicked = false
        PageButton(
            localised_text,
            (fun () -> 
                clicked <- true
                action()
            ),
            Disabled = fun () -> clicked
        )

    static member Once(localised_text, action, disabled: unit -> bool) =
        let mutable clicked = false
        PageButton(
            localised_text,
            (fun () -> 
                clicked <- true
                action()
            ),
            Disabled = fun () -> clicked || disabled()
        )

type PageTextEntry(name, setting) =
    inherit
        PageSetting(
            name,
            let entry = TextEntry(setting, "none", false) in

            entry
            |+ Frame(
                Position = Position.DEFAULT.Shrink(-15.0f, 0.0f),
                Fill = K Color.Transparent,
                Border =
                    fun () ->
                        if entry.Selected then Colors.pink_accent
                        elif entry.Focused then Colors.yellow_accent
                        else Colors.grey_2
            )
        )

type ColorPicker(s: Setting<Color>, allow_alpha: bool) as this =
    inherit Container(NodeType.Container(fun _ -> Some this.HexEditor))

    let HEX_EDITOR_HEIGHT = PRETTYHEIGHT * 0.6f

    let (H, S, V) = s.Value.ToHsv()
    let mutable H = H
    let mutable S = S
    let mutable V = V
    let mutable A = if allow_alpha then float32 s.Value.A / 255.0f else 1.0f

    let mutable dragging_sv = false
    let mutable dragging_h = false
    let mutable dragging_a = false

    let hex =
        Setting.simple (s.Value.ToHex())
        |> Setting.trigger (fun color ->
            try
                s.Value <- Color.FromHex color
                let (h, s, v) = s.Value.ToHsv()
                H <- h
                S <- s
                V <- v
            with _ ->
                ()
        )

    let hex_editor =
        { new TextEntry(hex, "none", false, Position = Position.ShrinkL(50.0f).SliceT HEX_EDITOR_HEIGHT) with
            override this.OnDeselected(by_mouse: bool) =
                base.OnDeselected by_mouse
                hex.Value <- s.Value.ToHex()
        }

    let s = Setting.trigger (fun (c: Color) -> hex.Value <- c.ToHex()) s

    do this.Add hex_editor

    member private this.HexEditor = hex_editor

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        base.Draw()

        let preview = this.Bounds.SliceT(HEX_EDITOR_HEIGHT).SliceL(50.0f).Shrink(5.0f)

        let saturation_value_picker =
            this.Bounds.ShrinkT(HEX_EDITOR_HEIGHT).SliceL(200.0f).Shrink(5.0f)

        let hue_picker =
            this.Bounds
                .ShrinkT(HEX_EDITOR_HEIGHT)
                .SliceL(230.0f)
                .ShrinkL(200.0f)
                .Shrink(5.0f)

        let alpha_picker =
            this.Bounds
                .ShrinkT(HEX_EDITOR_HEIGHT)
                .SliceL(260.0f)
                .ShrinkL(230.0f)
                .Shrink(5.0f)

        Draw.rect preview s.Value

        Draw.untextured_quad
            (saturation_value_picker.AsQuad)
            (struct (Color.White, Color.FromHsv(H, 1.0f, 1.0f), Color.Black, Color.Black))

        let x = saturation_value_picker.Left + S * saturation_value_picker.Width
        let y = saturation_value_picker.Bottom - V * saturation_value_picker.Height
        Draw.rect (Rect.Create(x - 2.5f, y - 2.5f, x + 2.5f, y + 2.5f)) Color.White

        let h = hue_picker.Height / 6.0f

        for i = 0 to 5 do
            let a = Color.FromHsv(float32 i / 6.0f, 1.0f, 1.0f)
            let b = Color.FromHsv((float32 i + 1.0f) / 6.0f, 1.0f, 1.0f)

            Draw.untextured_quad
                (Rect.Box(hue_picker.Left, hue_picker.Top + h * float32 i, hue_picker.Width, h))
                    .AsQuad
                (Quad.gradient_top_to_bottom a b)

        Draw.rect
            (Rect.Box(hue_picker.Left, hue_picker.Top + H * (hue_picker.Height - 5.0f), hue_picker.Width, 5.0f))
            Color.White

        if allow_alpha then
            Draw.untextured_quad
                alpha_picker.AsQuad
                (Quad.gradient_top_to_bottom (s.Value.O4a 0) s.Value)

            Draw.rect
                (Rect.Box(
                    alpha_picker.Left,
                    alpha_picker.Top + A * (alpha_picker.Height - 5.0f),
                    alpha_picker.Width,
                    5.0f
                ))
                Color.White

    override this.Update(elapsed_ms, moved) =

        base.Update(elapsed_ms, moved)

        let saturation_value_picker =
            this.Bounds.ShrinkT(HEX_EDITOR_HEIGHT).SliceL(200.0f).Shrink(5.0f)

        let hue_picker =
            this.Bounds
                .ShrinkT(HEX_EDITOR_HEIGHT)
                .SliceL(230.0f)
                .ShrinkL(200.0f)
                .Shrink(5.0f)

        let alpha_picker =
            this.Bounds
                .ShrinkT(HEX_EDITOR_HEIGHT)
                .SliceL(260.0f)
                .ShrinkL(230.0f)
                .Shrink(5.0f)

        if Mouse.hover saturation_value_picker && Mouse.left_click() then
            dragging_sv <- true

        if dragging_sv then
            let x, y = Mouse.pos ()
            S <- (x - saturation_value_picker.Left) / saturation_value_picker.Width |> min 1.0f |> max 0.0f
            V <- 1.0f - (y - saturation_value_picker.Top) / saturation_value_picker.Height |> min 1.0f |> max 0.0f
            s.Value <- Color.FromArgb(int (A * 255.0f), Color.FromHsv(H, S, V))
            if not (Mouse.held Mouse.LEFT) then dragging_sv <- false

        elif Mouse.hover hue_picker && Mouse.left_click() then
            dragging_h <- true

        if dragging_h then
            let y = Mouse.y ()
            H <- (y - hue_picker.Top) / hue_picker.Height |> min 1.0f |> max 0.0f
            s.Value <- Color.FromArgb(int (A * 255.0f), Color.FromHsv(H, S, V))
            if not (Mouse.held Mouse.LEFT) then dragging_h <- false

        elif Mouse.hover alpha_picker && Mouse.left_click() then
            dragging_a <- true

        if dragging_a then
            let y = Mouse.y ()
            A <- (y - alpha_picker.Top) / alpha_picker.Height |> min 1.0f |> max 0.0f
            s.Value <- Color.FromArgb(int (A * 255.0f), Color.FromHsv(H, S, V))
            if not (Mouse.held Mouse.LEFT) then dragging_a <- false

type OptionsMenuButton(label: string, width: float32, on_click: unit -> unit) =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                on_click ()
            )
        )

    member val IsHighlighted = K false with get, set
    member val Keybind = Bind.Dummy with get, set

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Init(parent) =
        this |* Clickable.Focus this
        base.Init(parent)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        if this.Keybind.Tapped() then on_click()

    override this.Draw() =
        let is_highlighted = this.IsHighlighted()
        let trim_color =
            if is_highlighted then Colors.pink_accent
            elif this.Focused then Colors.black
            else Colors.shadow_1
            
        let color =
            if is_highlighted then Colors.pink_accent.O2
            elif this.Focused then Colors.shadow_2.O3
            else Colors.shadow_2.O2

        let text_color =
            if this.Focused then Colors.text_yellow_2
            elif is_highlighted then Colors.text
            else Colors.text_subheading

        Draw.rect this.Bounds color
        Draw.rect (this.Bounds.BorderB Style.PADDING) trim_color

        Text.fill_b (Style.font, label, this.Bounds.Shrink(Style.PADDING * 2.0f), text_color, Alignment.CENTER)

    interface IWidth with
        member this.Width = width

[<AutoOpen>]
module Helpers =

    let page_container () =
        NavigationContainer.Column(WrapNavigation = false, Position = Position.Shrink(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))

    let refreshable_row number cons =
        let r = NavigationContainer.Row()

        let refresh () =
            r.Clear()
            let n = number ()

            for i in 0 .. (n - 1) do
                r.Add(cons i n)

        refresh ()
        r, refresh