namespace Percyqaz.Flux.UI

open System
open System.Drawing
open Percyqaz.Common
open Percyqaz.Flux.Resources
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input

type TextEntry(setting: Setting<string>, bind: Hotkey) as this =
    inherit StaticContainer(NodeType.Leaf)

    let color = Animation.Fade(0.5f)
    let ticker = Animation.Counter(600.0)

    let toggle() = if this.Selected then this.Focus() else this.Select()

    do
        this
        |+ Text(
            (fun () -> setting.Get() + if this.Selected && ticker.Loops % 2 = 0 then "_" else ""),
            Align = Alignment.LEFT, 
            Color = fun () -> (Style.highlight(255, color.Value), Color.Black))
        |+ Clickable(this.Select, OnHover = fun b -> if b && not this.Focused then this.Focus())
        |* HotkeyAction(bind, toggle)

    override this.OnSelected() =
        base.OnSelected()
        color.Target <- 1.0f
        Input.setTextInput(setting, fun () -> if this.Selected then this.Focus())

    override this.OnDeselected() =
        base.OnDeselected()
        color.Target <- 0.8f
        Input.removeInputMethod()
    
    override this.OnFocus() = base.OnFocus(); color.Target <- 0.8f

    override this.OnUnfocus() = base.OnUnfocus(); color.Target <- 0.5f

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        color.Update(elapsedTime)
        ticker.Update(elapsedTime)

type Slider<'T>(setting: Setting.Bounded<'T>, stepPercentage: float32) as this =
    inherit StaticContainer(NodeType.Leaf)

    let TEXTWIDTH = 130.0f
    let color = Animation.Fade 0.5f
    let mutable dragging = false
        
    let get_percent () =
        let (Setting.Bounds (lo, hi)) = setting.Config
        let (lo, hi) = (Convert.ToSingle lo, Convert.ToSingle hi)
        let value = Convert.ToSingle setting.Value
        (value - lo) / (hi - lo)

    let set_percent (v: float32) =
        let (Setting.Bounds (lo, hi)) = setting.Config
        let (lo, hi) = (Convert.ToSingle lo, Convert.ToSingle hi)
        setting.Value <- Convert.ChangeType((hi - lo) * v + lo, typeof<'T>) :?> 'T

    let add_percent v = set_percent (get_percent () + v)
    do
        this
        |+ Text(
            (fun () -> this.Format setting.Value),
            Color = K (Color.White, Color.Black),
            Align = Alignment.LEFT,
            Position = { Position.Default with Right = 0.0f %+ TEXTWIDTH })
        |* Clickable(
            (fun () -> this.Select(); dragging <- true),
            OnHover = (fun b -> if b && not this.Focused then this.Focus()))
        this.Position <- Position.SliceLeft 100.0f

    member val Format = (fun x -> x.ToString()) with get, set

    static member Percent(setting, incr) = Slider<float>(setting, incr, Format = fun x -> sprintf "%.0f%%" (x * 100.0))

    override this.OnSelected() = base.OnSelected(); color.Target <- 0.9f
    override this.OnDeselected() = base.OnDeselected(); color.Target <- 0.8f
    override this.OnFocus() = base.OnFocus(); color.Target <- 0.8f
    override this.OnUnfocus() = base.OnUnfocus(); color.Target <- 0.5f

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        color.Update(elapsedTime)
        let bounds = this.Bounds.TrimLeft TEXTWIDTH
        if this.Selected || Mouse.hover this.Bounds then
            add_percent(stepPercentage * Mouse.scroll())
        if this.Selected then
            if (Mouse.held Mouse.LEFT && dragging) then
                let l, r = if Input.ThisFrame.shift then 0.0f, Viewport.vwidth else bounds.Left, bounds.Right
                let amt = (Mouse.x() - l) / (r - l)
                set_percent amt
            else dragging <- false

            if (!|"left").Tapped() then add_percent -stepPercentage
            elif (!|"right").Tapped() then add_percent stepPercentage
            elif (!|"up").Tapped() then add_percent (stepPercentage * 5.0f)
            elif (!|"down").Tapped() then add_percent (-stepPercentage * 5.0f)

    override this.Draw() =
        let v = get_percent()
        let bounds = this.Bounds.TrimLeft TEXTWIDTH

        let cursor_x = bounds.Left + bounds.Width * v
        let cursor = Rect.Create(cursor_x, bounds.Top, cursor_x, bounds.Bottom).Expand(10.0f, -10.0f)
        let m = bounds.CenterY
        Draw.rect (Rect.Create(cursor_x, (m - 10.0f), bounds.Right, (m + 10.0f))) (Style.color(100, 1.0f, 0.0f))
        Draw.rect (Rect.Create(bounds.Left, (m - 10.0f), cursor_x, (m + 10.0f))) (Style.color(255, 1.0f, color.Value))
        Draw.rect cursor (Style.color(255, 1.0f, color.Value))
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
        Selector<bool>([|false, Feather.circle; true, Feather.check_circle|], setting)