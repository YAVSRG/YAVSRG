namespace Interlude.UI.Menu

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude.Common
open Interlude.Utils
open Interlude.UI

type Divider() =
    inherit StaticWidget(NodeType.None)

    member this.Pos(y) =
        this.Position <- Position.Box(0.0f, 0.0f, 100.0f, y - 5.0f, PRETTYWIDTH, 10.0f)
        this

    override this.Draw() =
        Draw.quad (Quad.ofRect this.Bounds) (struct(Color.White, Color.FromArgb(0, 255, 255, 255), Color.FromArgb(0, 255, 255, 255), Color.White)) Sprite.DefaultQuad

type PrettySetting(name, widget: Widget) as this =
    inherit StaticContainer(NodeType.Switch (fun _ -> this.Child))

    let mutable widget = widget

    member this.Child
        with get() = widget
        and set(w: Widget) =
            let old_widget = widget
            widget <- w
            w.Position <- Position.TrimLeft(PRETTYTEXTWIDTH).Margin(Style.padding)
            if this.Initialised then 
                w.Init this
                if old_widget.Focused then w.Focus()
    
    member this.Pos(y, width, height) =
        this.Position <- Position.Box(0.0f, 0.0f, 100.0f, y, width, height) 
        this
    
    member this.Pos(y, width) = this.Pos(y, width, PRETTYHEIGHT)
    member this.Pos(y) = this.Pos(y, PRETTYWIDTH)

    override this.Init(parent) =
        this
        |+ Text(
            K (N name + ":"),
            Color = (fun () -> ((if this.Selected then Style.color(255, 1.0f, 0.2f) else Color.White), Color.Black)),
            Align = Alignment.LEFT,
            Position = Position.Box(0.0f, 0.0f, PRETTYTEXTWIDTH, PRETTYHEIGHT).Margin(Style.padding))
        |* Tooltip(Tooltip.Info(sprintf "options.%s" name))
        base.Init parent
        widget.Position <- Position.TrimLeft(PRETTYTEXTWIDTH).Margin(Style.padding)
        widget.Init this

    override this.Draw() =
        if widget.Selected then Draw.rect this.Bounds (!*Palette.SELECTED)
        elif widget.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
        base.Draw()
        widget.Draw()
    
    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        widget.Update(elapsedTime, moved)

type PrettyButton(name, action) as this =
    inherit StaticContainer(NodeType.Button (fun _ -> if this.Enabled then action()))

    member val Icon = "" with get, set
    member val Text = N name with get, set

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K (if this.Icon <> "" then sprintf "%s %s  >" this.Icon this.Text else sprintf "%s  >" this.Text),
            Color = ( 
                fun () -> 
                    if this.Enabled then
                        ( (if this.Focused then Style.color(255, 1.0f, 0.5f) else Color.White), Color.Black )
                    else (Color.Gray, Color.Black)
            ),
            Align = Alignment.LEFT,
            Position = Position.Margin(Style.padding))
        |+ Clickable(this.Select, OnHover = fun b -> if b then this.Focus())
        |* Tooltip(Tooltip.Info(sprintf "options.%s" name))
        base.Init parent

    override this.Draw() =
        if this.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
        base.Draw()

    member this.Pos(y, height) = 
        this.Position <- Position.Box(0.0f, 0.0f, 100.0f, y, PRETTYWIDTH, height)
        this

    member this.Pos(y) = this.Pos(y, PRETTYHEIGHT)

    member val Enabled = true with get, set
    
    static member Once(name, action) =
        let mutable ref = Unchecked.defaultof<PrettyButton>
        let button = PrettyButton(name, fun () ->
                if ref.Enabled then action()
                ref.Enabled <- false
            )
        ref <- button
        button

type CaseSelector(name: string, cases: string array, controls: Widget array array, setting: Setting<int>) as this =
    inherit StaticWidget(NodeType.Switch(fun _ -> this._selector()))

    let selector = PrettySetting(name, Selector<int>(Array.indexed cases, setting)).Pos(200.0f)

    member this._selector() = selector

    member this.Pos(pos) = selector.Pos(pos) |> ignore; this

    member private this.WhoIsFocused : int option =
        if selector.Focused then Some -1
        else Seq.tryFindIndex (fun (c: Widget) -> c.Focused) controls.[setting.Value]

    member this.Previous() =
        match this.WhoIsFocused with
        | Some n ->
            let current_controls = controls.[setting.Value]
            if n = -1 then
                current_controls.[current_controls.Length - 1].Focus()
            elif n = 0 then selector.Focus()
            else current_controls.[n - 1].Focus()
        | None -> ()

    member this.Next() =
        match this.WhoIsFocused with
        | Some n ->
            let current_controls = controls.[setting.Value]
            if n = -1 then
                current_controls.[0].Focus()
            elif n = current_controls.Length - 1 then selector.Focus()
            else current_controls.[n + 1].Focus()
        | None -> ()

    member this.SelectFocusedChild() =
        match this.WhoIsFocused with
        | Some -1 -> selector.Select()
        | Some n -> controls.[setting.Value].[n].Select()
        | None -> ()

    override this.Draw() =
        let current_controls = controls.[setting.Value]
        selector.Draw()
        for c in current_controls do
            c.Draw()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        if moved then
            selector.Update(elapsedTime, true)
            for case in controls do
                for control in case do
                    control.Update(elapsedTime, true)
        else
            selector.Update(elapsedTime, false)
            for control in controls.[setting.Value] do
                control.Update(elapsedTime, false)

        if this.Focused then
            if not selector.Focused && (!|"up").Tapped() then this.Previous()
            elif (!|"down").Tapped() then this.Next()
            elif (!|"select").Tapped() then this.SelectFocusedChild()

    override this.Init(parent: Widget) =
        base.Init parent
        selector.Init this
        for case in controls do
            for control in case do
                 control.Init this

type ColorPicker(s: Setting<Color>, allowAlpha: bool) as this =
    inherit StaticContainer(NodeType.Switch(fun _ -> this.HexEditor))

    let (H, S, V) = s.Value.ToHsv()
    let mutable H = H
    let mutable S = S
    let mutable V = V
    let mutable A = if allowAlpha then float32 s.Value.A / 255.0f else 1.0f

    let hex = 
        Setting.simple (s.Value.ToHex())
        |> Setting.trigger (fun color -> 
            try 
                s.Value <- Color.FromHex color
                let (h, s, v) = s.Value.ToHsv()
                H <- h
                S <- s
                V <- v
            with _ -> ())

    let hexEditor = 
        { new TextEntry(hex, "none", Position = Position.TrimLeft(50.0f).SliceTop PRETTYHEIGHT) with
            override this.OnDeselected() = 
                base.OnDeselected()
                hex.Value <- s.Value.ToHex()
        }

    let s = Setting.trigger (fun (c: Color) -> hex.Value <- c.ToHex()) s

    do this.Add hexEditor

    member private this.HexEditor = hexEditor

    override this.Draw() =
        base.Draw()

        let preview = this.Bounds.SliceTop(PRETTYHEIGHT).SliceLeft(50.0f).Shrink(5.0f)

        let saturation_value_picker = this.Bounds.TrimTop(PRETTYHEIGHT).SliceLeft(200.0f).Shrink(5.0f)
        let hue_picker = this.Bounds.TrimTop(PRETTYHEIGHT).SliceLeft(230.0f).TrimLeft(200.0f).Shrink(5.0f)
        let alpha_picker = this.Bounds.TrimTop(PRETTYHEIGHT).SliceLeft(260.0f).TrimLeft(230.0f).Shrink(5.0f)

        Draw.rect preview s.Value
        Draw.quad 
            (Quad.ofRect saturation_value_picker)
            (struct (Color.White, Color.FromHsv(H, 1.0f, 1.0f), Color.Black, Color.Black))
            Sprite.DefaultQuad
        let x = saturation_value_picker.Left + S * saturation_value_picker.Width
        let y = saturation_value_picker.Bottom - V * saturation_value_picker.Height
        Draw.rect (Rect.Create (x - 2.5f, y - 2.5f, x + 2.5f, y + 2.5f)) Color.White

        let h = hue_picker.Height / 6.0f
        for i = 0 to 5 do
            let a = Color.FromHsv(float32 i / 6.0f, 1.0f, 1.0f)
            let b = Color.FromHsv((float32 i + 1.0f) / 6.0f, 1.0f, 1.0f)
            Draw.quad 
                (Quad.ofRect (Rect.Box(hue_picker.Left, hue_picker.Top + h * float32 i, hue_picker.Width, h)))
                (struct (a, a, b, b))
                Sprite.DefaultQuad
        Draw.rect (Rect.Box (hue_picker.Left, hue_picker.Top + H * (hue_picker.Height - 5.0f), hue_picker.Width, 5.0f)) Color.White

        if allowAlpha then
            Draw.quad 
                (Quad.ofRect alpha_picker)
                (struct (Color.FromArgb(0, s.Value), Color.FromArgb(0, s.Value), s.Value, s.Value))
                Sprite.DefaultQuad
            Draw.rect (Rect.Box (alpha_picker.Left, alpha_picker.Top + A * (alpha_picker.Height - 5.0f), alpha_picker.Width, 5.0f)) Color.White

    override this.Update(elapsedTime, moved) =

        base.Update(elapsedTime, moved)
        
        let saturation_value_picker = this.Bounds.TrimTop(PRETTYHEIGHT).SliceLeft(200.0f).Shrink(5.0f)
        let hue_picker = this.Bounds.TrimTop(PRETTYHEIGHT).SliceLeft(230.0f).TrimLeft(200.0f).Shrink(5.0f)
        let alpha_picker = this.Bounds.TrimTop(PRETTYHEIGHT).SliceLeft(260.0f).TrimLeft(230.0f).Shrink(5.0f)

        if Mouse.hover saturation_value_picker && Mouse.held Mouse.LEFT then
            let x, y = Mouse.pos()
            S <- (x - saturation_value_picker.Left) / saturation_value_picker.Width
            V <- 1.0f - (y - saturation_value_picker.Top) / saturation_value_picker.Height
            s.Value <- Color.FromArgb(int (A * 255.0f), Color.FromHsv(H, S, V))

        elif Mouse.hover hue_picker && Mouse.held Mouse.LEFT then
            let y = Mouse.y()
            H <- (y - hue_picker.Top) / hue_picker.Height
            s.Value <- Color.FromArgb(int (A * 255.0f), Color.FromHsv(H, S, V))

        elif Mouse.hover alpha_picker && Mouse.held Mouse.LEFT then
            let y = Mouse.y()
            A <- (y - alpha_picker.Top) / alpha_picker.Height
            s.Value <- Color.FromArgb(int (A * 255.0f), Color.FromHsv(H, S, V))
        