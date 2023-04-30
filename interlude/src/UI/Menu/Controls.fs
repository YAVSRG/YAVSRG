namespace Interlude.UI.Menu

open System
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude.Common
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components

type Slider(setting: Setting.Bounded<float32>) as this =
    inherit StaticContainer(NodeType.Leaf)

    let TEXTWIDTH = 130.0f
    let mutable dragging = false

    let mutable decimal_places = 2
    let mutable step = 0.01f
        
    let get_percent () =
        let (Setting.Bounds (lo, hi)) = setting.Config
        (setting.Value - lo) / (hi - lo)

    let set_percent (v: float32) =
        let (Setting.Bounds (lo, hi)) = setting.Config
        setting.Value <- MathF.Round((hi - lo) * v + lo, decimal_places)

    let add(v) = setting.Value <- MathF.Round(setting.Value + v, decimal_places)

    do
        this
        |+ Text(
            (fun () -> this.Format setting.Value),
            Align = Alignment.LEFT,
            Position = { Position.Default with Right = 0.0f %+ TEXTWIDTH })
        |* Clickable(
            (fun () -> this.Select(); dragging <- true),
            OnHover = (fun b -> if b && not this.Focused then this.Focus()))

    member this.Step with get() = step and set(value) = step <- value; decimal_places <- max 0 (int (MathF.Ceiling(-MathF.Log10(step))))

    member val Format = (fun x -> x.ToString()) with get, set

    static member Percent(setting) = Slider(setting, Format = fun x -> sprintf "%.0f%%" (x * 100.0f))

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let bounds = this.Bounds.TrimLeft TEXTWIDTH
        if this.Selected || Mouse.hover this.Bounds then
            let s = Mouse.scroll()
            if s > 0.0f then setting.Value <- setting.Value + step
            elif s < 0.0f then setting.Value <- setting.Value - step
        if this.Selected then
            if (Mouse.held Mouse.LEFT && dragging) then
                let l, r = bounds.Left, bounds.Right
                let amt = (Mouse.x() - l) / (r - l)
                set_percent amt
            else dragging <- false

            if (!|"left").Tapped() then add (-step)
            elif (!|"right").Tapped() then add (step)
            elif (!|"up").Tapped() then add (step * 5.0f)
            elif (!|"down").Tapped() then add (-step * 5.0f)

    override this.Draw() =
        let v = get_percent()
        let bounds = this.Bounds.TrimLeft TEXTWIDTH

        let cursor_x = bounds.Left + bounds.Width * v
        Draw.rect (Rect.Create(cursor_x, (bounds.Top + 10.0f), bounds.Right, (bounds.Bottom - 10.0f))) (if this.Selected then Colors.pink_shadow.O3 else Colors.grey_2.O2)
        Draw.rect (Rect.Create(bounds.Left, (bounds.Top + 10.0f), cursor_x, (bounds.Bottom - 10.0f))) (if this.Selected then Colors.pink_accent else Colors.grey_2)
        base.Draw()

type Selector<'T>(items: ('T * string) array, setting: Setting<'T>) as this =
    inherit StaticContainer(NodeType.Leaf)

    let mutable index = 
        items
        |> Array.tryFindIndex (fun (v, _) -> Object.Equals(v, setting.Value))
        |> Option.defaultValue 0

    let fd() = 
        index <- (index + 1) % items.Length
        setting.Value <- fst items.[index]

    let bk() =
        index <- (index + items.Length - 1) % items.Length
        setting.Value <- fst items.[index]

    do
        this
        |+ Text((fun () -> snd items.[index]), Align = Alignment.LEFT)
        |* Clickable(
            (fun () -> (if not this.Selected then this.Select()); fd()),
            OnHover = fun b -> if b && not this.Focused then this.Focus())
        this.Position <- Position.SliceLeft 100.0f

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if this.Selected then
            if (!|"left").Tapped() then bk()
            elif (!|"right").Tapped() then fd()
            elif (!|"up").Tapped() then fd()
            elif (!|"down").Tapped() then bk()

    static member FromEnum(setting: Setting<'T>) =
        let names = Enum.GetNames(typeof<'T>)
        let values = Enum.GetValues(typeof<'T>) :?> 'T array
        Selector(Array.zip values names, setting)

    static member FromBool(setting: Setting<bool>) =
        Selector<bool>([|false, Icons.unselected; true, Icons.selected|], setting)

type Divider() =
    inherit StaticWidget(NodeType.None)

    member this.Pos(y) =
        this.Position <- Position.Box(0.0f, 0.0f, 100.0f, y - 5.0f, PRETTYWIDTH, 10.0f)
        this

    override this.Draw() =
        Draw.quad (Quad.ofRect this.Bounds) (struct(Color.White, Color.FromArgb(0, 255, 255, 255), Color.FromArgb(0, 255, 255, 255), Color.White)) Sprite.DefaultQuad

type PageSetting(name, widget: Widget) as this =
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
        |* Text(
            K (L (sprintf "%s.name" name) + ":"),
            Color = (fun () -> (if this.Focused then Colors.text_yellow_2 else Colors.text)),
            Align = Alignment.LEFT,
            Position = Position.Box(0.0f, 0.0f, PRETTYTEXTWIDTH - 10.0f, PRETTYHEIGHT).Margin(Style.padding))
        base.Init parent
        widget.Position <- Position.TrimLeft(PRETTYTEXTWIDTH).Margin(Style.padding)
        widget.Init this

    override this.Draw() =
        if widget.Selected then Draw.rect (widget.Bounds.Expand(15.0f, Style.padding)) Colors.pink_accent.O2
        elif widget.Focused then Draw.rect (widget.Bounds.Expand(15.0f, Style.padding)) Colors.yellow_accent.O1
        base.Draw()
        widget.Draw()
    
    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        widget.Update(elapsedTime, moved)

type PageButton(name, action) as this =
    inherit StaticContainer(NodeType.Button (fun _ -> if this.Enabled then action()))

    member val Icon = "" with get, set
    member val Text = L (sprintf "%s.name" name) with get, set

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K (if this.Icon <> "" then sprintf "%s %s  >" this.Icon this.Text else sprintf "%s  >" this.Text),
            Color = ( 
                fun () -> 
                    if this.Enabled then (if this.Focused then Colors.text_yellow_2 else Colors.text)
                    else Colors.text_greyout
            ),
            Align = Alignment.LEFT,
            Position = Position.Margin(Style.padding))
        |* Clickable(this.Select, OnHover = fun b -> if b then this.Focus())
        base.Init parent

    override this.Draw() =
        if this.Focused then Draw.rect this.Bounds Colors.yellow_accent.O1
        base.Draw()

    member this.Pos(y, height) = 
        this.Position <- Position.Box(0.0f, 0.0f, 100.0f, y, PRETTYWIDTH, height)
        this

    member this.Pos(y) = this.Pos(y, PRETTYHEIGHT)

    member val Enabled = true with get, set
    
    static member Once(name, action) =
        let mutable ref = Unchecked.defaultof<PageButton>
        let button = PageButton(name, fun () ->
                if ref.Enabled then action()
                ref.Enabled <- false
            )
        ref <- button
        button

type CaseSelector(name: string, cases: string array, controls: Widget array array, setting: Setting<int>) as this =
    inherit StaticWidget(NodeType.Switch(fun _ -> this._selector()))

    let selector = PageSetting(name, Selector<int>(Array.indexed cases, setting)).Pos(200.0f)

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
        
