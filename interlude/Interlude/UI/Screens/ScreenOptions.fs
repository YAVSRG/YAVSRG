namespace Interlude.UI

open System
open FSharp.Reflection
open System.Collections.Generic
open OpenTK
open Prelude.Common
open Interlude.Options
open Interlude.Render
open Interlude
open Interlude.Utils
open Interlude.Input
open OpenTK.Input
open Interlude.Options
open Interlude.UI.Animation
open Interlude.UI.Components

module SelectionWheel =

    let WIDTH = 350.0f

    [<AbstractClass>]
    type ISelectable(onDeselect) =
        inherit Widget()

        let mutable selected = false
        member this.Selected = selected

        abstract member Select: unit -> unit
        default this.Select() = selected <- true
        abstract member Deselect: unit -> unit
        default this.Deselect() = selected <- false; onDeselect()
        abstract member DeselectChild: unit -> unit
        abstract member AutoSelect: bool

    type SelectionWheel(onDeselect) as this  =
        inherit ISelectable(onDeselect)

        let mutable index = 0
        let items = new List<ISelectable>()
        let collapse = new AnimationFade(1.0f)
        do this.Animation.Add(collapse)
        override this.Select() = base.Select(); collapse.SetTarget(0.0f)
        override this.Deselect() = base.Deselect(); collapse.SetTarget(1.0f)
        override this.DeselectChild() = Seq.iter (fun (w: ISelectable) -> if w.Selected then w.Deselect()) items

        override this.Add(w) = failwith "don't use this, use AddItem"
        member this.AddItem(w) = items.Add(w); w.AddTo(this)

        override this.Draw() =
            let o = WIDTH * collapse.Value
            if collapse.Value < 0.98f then
                let struct (left, top, right, bottom) = this.Bounds
                Draw.rect(this.Bounds |> Rect.sliceLeft(WIDTH - o))(Color.FromArgb(180, 30, 30, 30))(Sprite.Default)
                Draw.rect(this.Bounds |> Rect.sliceLeft(WIDTH - o) |> Rect.sliceRight(5.0f))(Color.White)(Sprite.Default)
                let mutable t = top
                for i in 0 .. (items.Count - 1) do
                    let w = items.[i]
                    let h = w.Bounds |> Rect.height
                    if index = i then
                        Draw.quad
                            (Rect.create (left - o) t (left + WIDTH - o) (t + h) |> Quad.ofRect)
                            (Color.FromArgb(255,180,180,180), Color.FromArgb(0,180,180,180), Color.FromArgb(0,180,180,180), Color.FromArgb(255,180,180,180))
                            Sprite.DefaultQuad
                    w.Draw()
                    t <- t + h

        override this.Update(elapsedTime, bounds) =
            let struct (left, _, right, bottom) = bounds
            base.Update(elapsedTime, struct (left, 0.0f, right, bottom))
            let o = WIDTH * collapse.Value
            let struct (left, _, _, bottom) = this.Bounds
            let mutable flag = 0
            let mutable t = 0.0f
            for i in 0 .. (items.Count - 1) do
                let w = items.[i]
                if w.Selected then flag <- if w.AutoSelect then 1 else 2
                w.Update(elapsedTime, Rect.create (left - o) t (left + WIDTH - o) bottom)
                let h = w.Bounds |> Rect.height
                t <- t + h
            if this.Selected && flag < 2 then
                if options.Hotkeys.Down.Get().Tapped(false) then
                    if flag = 1 then items.[index].Deselect()
                    index <- (index + 1) % items.Count
                    if items.[index].AutoSelect then items.[index].Select()
                elif options.Hotkeys.Up.Get().Tapped(false) then
                    if flag = 1 then items.[index].Deselect()
                    index <- (index + items.Count - 1) % items.Count
                    if items.[index].AutoSelect then items.[index].Select()
                elif options.Hotkeys.Exit.Get().Tapped(false) || (flag = 0 && options.Hotkeys.Previous.Get().Tapped(false)) then
                    this.Deselect()
                    this.DeselectChild()
                if flag = 0 && (options.Hotkeys.Select.Get().Tapped(false) || options.Hotkeys.Next.Get().Tapped(false)) then items.[index].Select()
        
        override this.AutoSelect = false

    type ActionItem(name, action) as this =
        inherit ISelectable(ignore)
        do
            this.Add(new TextBox(K name, K (Color.White, Color.Black), 0.5f))
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 60.0f, 0.0f)
        override this.Select() = action()
        override this.DeselectChild() = ()
        override this.AutoSelect = false

    (*
    type WidgetItem(name, w: ISelectable, onDeselect) as this =
        inherit ISelectable(onDeselect)
        do
            this.Add(new TextBox(K name, (fun () -> if this.Selected then Color.Yellow, Color.Black else Color.White, Color.Black), 0.5f))
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 60.0f, 0.0f)
        override this.Select() = base.Select(); w.Select()
        override this.DeselectChild() = () //?
        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if not w.Selected then this.Deselect()
            w.Update(elapsedTime, this.Parent.Bounds |> Rect.trimLeft(WIDTH))
        override this.Draw() =
            w.Draw()
            base.Draw()
        override this.AutoSelect = false
    *)

    //I'm sure there is a way to make this a special case of container item
    //but im too dumb to figure it out rn and ultimately it doesn't matter
    type SelectionWheelItem(name, sw: SelectionWheel) as this =
        inherit ISelectable(ignore)
        do
            this.Add(new TextBox(K name, (fun () -> if this.Selected then Color.Yellow, Color.Black else Color.White, Color.Black), 0.5f))
            //this.Add(new Clickable((fun () -> if not this.Selected then (this.Parent :?> ISelectionWheel).DeselectChild(); this.Select()), ignore))
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 60.0f, 0.0f)
        override this.Select() = base.Select(); sw.Select()
        override this.DeselectChild() = ()
        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if not sw.Selected then this.Deselect()
            sw.Update(elapsedTime, this.Parent.Bounds |> Rect.trimLeft(WIDTH))
        override this.Draw() =
            Stencil.create(false)
            Draw.rect(this.Parent.Bounds |> Rect.trimLeft(WIDTH))(Color.Transparent)(Sprite.Default)
            Stencil.draw()
            sw.Draw()
            Stencil.finish()
            base.Draw()
        override this.AutoSelect = false

    type Selector(items: string array, index, func, name, onDeselect) as this =
        inherit ISelectable(onDeselect)
        let mutable index = index
        let fd() =
            index <- ((index + 1) % items.Length)
            func(index, items.[index])
        let bk() =
            index <- ((index + items.Length - 1) % items.Length)
            func(index, items.[index])
        do
            this.Add(new TextBox(K name, (fun () -> if this.Selected then Color.Yellow, Color.Black else Color.White, Color.Black), 0.5f) |> positionWidgetA(0.0f, 0.0f, 0.0f, -40.0f))
            this.Add(new TextBox((fun () -> items.[index]), (fun () -> Color.White, Color.Black), 0.5f) |> positionWidgetA(0.0f, 60.0f, 0.0f, 0.0f))
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f)
            //this.Add(new Clickable((fun () -> (if not this.Selected then (this.Parent :?> ISelectionWheel).DeselectChild(); this.Select()); fd()), ignore))

        override this.DeselectChild() = ()
        override this.AutoSelect = true

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if this.Selected then
                //if options.Hotkeys.Exit.Get().Tapped(false) then this.Deselect()
                if options.Hotkeys.Next.Get().Tapped(false) then fd()
                elif options.Hotkeys.Previous.Get().Tapped(false) then bk()

        static member FromEnum<'U, 'T when 'T: enum<'U>>(setting: ISettable<'T>, label, onDeselect) =
            let names = Enum.GetNames(typeof<'T>)
            let values = Enum.GetValues(typeof<'T>) :?> 'T array
            new Selector(names, Array.IndexOf(values, setting.Get()), (fun (i, _) -> setting.Set(values.[i])), label, onDeselect)

        static member FromBool(setting: ISettable<bool>, label, onDeselect) =
            new Selector([|"NO" ; "YES"|], (if setting.Get() then 1 else 0), (fun (i, _) -> setting.Set(i > 0)), label, onDeselect)

        static member FromKeymode(setting: ISettable<int>, onDeselect) =
            new Selector([|"3K"; "4K"; "5K"; "6K"; "7K"; "8K"; "9K"; "10K"|], setting.Get(), (fun (i, _) -> setting.Set(i)), Localisation.localise "options.name.Keymode", onDeselect)
    
    type Slider<'T when 'T : comparison>(setting: NumSetting<'T>, name, onDeselect) as this =
        inherit ISelectable(onDeselect)
        let color = AnimationFade(0.5f)
        let mutable dragging = false
        let chPercent(v) = setting.SetPercent(setting.GetPercent() + v)
        do
            this.Animation.Add(color)
            this.Add(new TextBox(K name, (fun () -> if this.Selected then Color.Yellow, Color.Black else Color.White, Color.Black), 0.5f) |> positionWidgetA(0.0f, 0.0f, 0.0f, -40.0f))
            this.Add(new TextBox((fun () -> setting.Get().ToString()), (fun () -> Color.White, Color.Black), 0.5f) |> positionWidgetA(0.0f, 60.0f, 0.0f, 0.0f))
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f)
            this.Add(new Clickable((fun () -> dragging <- true), fun b -> color.SetTarget(if b then 0.8f else 0.5f)))

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            let struct (l, t, r, b) = this.Bounds |> Rect.expand(-15.0f, -15.0f)
            if this.Selected then
                if (Mouse.pressed(Input.MouseButton.Left) && dragging) then
                    let amt = (Mouse.X() - l) / (r - l)
                    setting.SetPercent(amt)
                else
                    dragging <- false
                if options.Hotkeys.UpRateHalf.Get().Tapped(false) || options.Hotkeys.Next.Get().Tapped(false) then chPercent(0.05f)
                elif options.Hotkeys.UpRateSmall.Get().Tapped(false) then chPercent(0.01f)
                elif options.Hotkeys.UpRate.Get().Tapped(false) then chPercent(0.1f)
                elif options.Hotkeys.DownRateHalf.Get().Tapped(false) || options.Hotkeys.Previous.Get().Tapped(false) then chPercent(-0.05f)
                elif options.Hotkeys.DownRateSmall.Get().Tapped(false) then chPercent(-0.01f)
                elif options.Hotkeys.Exit.Get().Tapped(false) then this.Deselect()
                elif options.Hotkeys.DownRate.Get().Tapped(false) then chPercent(-0.1f)

        override this.Draw() =
            let v = setting.GetPercent()
            let struct (l, t, r, b) = this.Bounds |> Rect.expand(-15.0f, -15.0f)
            let cursor = Rect.create (l + (r - l) * v) (b - 10.0f) (l + (r - l) * v) (b)
            Draw.rect (this.Bounds |> Rect.sliceBottom 30.0f |> Rect.expand(0.0f, -10.0f)) (Screens.accentShade(255, 1.0f, v)) Sprite.Default
            Draw.rect (cursor |> Rect.expand(10.0f, 10.0f)) (Screens.accentShade(255, 1.0f, color.Value)) Sprite.Default
            base.Draw()

        override this.DeselectChild() = ()
        override this.AutoSelect = true

    type KeyBinder(setting: ISettable<Bind>, name, allowModifiers, onDeselect) as this =
        inherit ISelectable(onDeselect)
        do
            if name = "" then
                this.Add(new TextBox((fun () -> setting.Get().ToString()), (fun () -> if this.Selected then Color.Yellow, Color.Black else Color.White, Color.Black), 0.5f) |> positionWidgetA(0.0f, 25.0f, 0.0f, -25.0f))
            else
                this.Add(new TextBox(K name, (fun () -> if this.Selected then Color.Yellow, Color.Black else Color.White, Color.Black), 0.5f) |> positionWidgetA(0.0f, 0.0f, 0.0f, -40.0f))
                this.Add(new TextBox((fun () -> setting.Get().ToString()), (fun () -> Color.White, Color.Black), 0.5f) |> positionWidgetA(0.0f, 60.0f, 0.0f, 0.0f))
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f)
        override this.Draw() =
            if name = "" then Draw.rect(this.Bounds |> Rect.expand(0.0f, -25.0f))(Screens.accentShade(127, 0.8f, 0.0f))Sprite.Default
            base.Draw()
        override this.Select() =
            base.Select()
            //todo: refactor where this logic is moved to Interlude.Input and also supports mouse/joystick inputs
            //Input.Keyboard.pressedOverride is marked internal because it should only be used from Interlude.Input and i was naughty to be lazy here
            Input.grabKey(
                fun k ->
                    if k = Key.Escape then if allowModifiers then setting.Set(Dummy)
                    else
                        setting.Set(
                            Input.Key (k,
                                (allowModifiers && (Input.Keyboard.pressedOverride(Key.ControlLeft) || Input.Keyboard.pressedOverride(Key.ControlRight)), false,
                                    allowModifiers && (Input.Keyboard.pressedOverride(Key.ShiftLeft) || Input.Keyboard.pressedOverride(Key.ShiftRight)))))
                    this.Deselect())
        override this.DeselectChild() = ()
        override this.AutoSelect = true

    type SelectionContainer(onDeselect) =
        inherit ISelectable(onDeselect)

        let mutable index = 0
        let items = new List<ISelectable>()

        member private this.Reset() = index <- 0; Seq.iter (fun (w: ISelectable) -> w.Dispose()) items; items.Clear(); items
        override this.DeselectChild() = Seq.iter (fun (w: ISelectable) -> if w.Selected then w.Deselect()) items

        override this.Add(w) = if w :? ISelectable then items.Add(w :?> ISelectable); base.Add(w)

        override this.Draw() =
            for i in 0 .. (items.Count - 1) do
                let w = items.[i]
                if index = i && not w.Selected then Draw.rect w.Bounds (Color.FromArgb(180,255,255,255)) Sprite.Default
            base.Draw()

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if this.Selected && items |> Seq.exists (fun (w: ISelectable) -> w.Selected) |> not then
                if options.Hotkeys.Select.Get().Tapped(false) then items.[index].Select()
                elif options.Hotkeys.Exit.Get().Tapped(false) then this.Deselect()
                elif options.Hotkeys.Next.Get().Tapped(false) then index <- (index + 1) % items.Count
                elif options.Hotkeys.Previous.Get().Tapped(false) then index <- (index + items.Count - 1) % items.Count

        override this.AutoSelect = true

    type SelectionRow(number, cons, name, onDeselect) as this =
        inherit ISelectable(onDeselect)
        let mutable index = 0
        let mutable items: ISelectable list = [0 .. (number() - 1)] |> List.map cons
        do
            this.Add(new TextBox(K name, (fun () -> if this.Selected then Color.Yellow, Color.Black else Color.White, Color.Black), 0.5f) |> positionWidgetA(0.0f, 20.0f, 0.0f, -20.0f))
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f)
        member this.Refresh() =
            index <- 0
            items |> List.iter (fun o -> o.Dispose())
            items <- [0 .. (number() - 1)] |> List.map cons
        override this.Draw() =
            List.iteri
                (fun i (w: ISelectable) ->
                    if this.Selected && index = i then Draw.rect w.Bounds (Color.FromArgb(255, 180, 180, 180)) Sprite.Default
                    w.Draw()) items
            base.Draw()
        override this.DeselectChild() = Seq.iter (fun (w: ISelectable) -> if w.Selected then w.Deselect()) items

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            let struct (_, top, right, bottom) = this.Bounds
            let h = bottom - top
            let mutable flag = true
            let mutable x = 0.0f
            List.iteri
                (fun i (w: ISelectable) ->
                    if w.Selected then flag <- false
                    w.Update(elapsedTime, Rect.create (right + x) top (right + x + h) bottom)
                    x <- x + h) items
            if flag && this.Selected then
                if options.Hotkeys.Select.Get().Tapped(false) then items.[index].Select()
                elif options.Hotkeys.Exit.Get().Tapped(false) then this.Deselect()
                elif options.Hotkeys.Next.Get().Tapped(false) then index <- (index + 1) % number()
                elif options.Hotkeys.Previous.Get().Tapped(false) then let n = number() in index <- (index + n - 1) % n

        override this.AutoSelect = true

    let swBuilder items = let sw = new SelectionWheel(ignore) in items |> List.iter sw.AddItem; sw
    let swItemBuilder items name = SelectionWheelItem(name, swBuilder items)

[<AutoOpen>]
module ThemeSelector =
    type private ThemeItem(name, m: ThemeSelector) as this =
        inherit Widget()
        do
            this.Add(TextBox(K name, K Color.White, 0.5f))
            this.Add(
                Clickable(
                    (fun () ->
                        if this.Parent = (m.Selected :> Widget) then 
                            m.Selected.Remove(this)
                            m.Available.Add(this)
                            options.EnabledThemes.Remove(name) |> ignore
                        else
                            m.Available.Remove(this)
                            m.Selected.Add(this)
                            options.EnabledThemes.Add(name)), ignore))
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 50.0f, 0.0f)
        override this.Draw() =
            Draw.rect(this.Bounds)(UI.Screens.accentShade(127, 1.0f, 0.0f))(Sprite.Default)
            base.Draw()
    and ThemeSelector() as this =
        inherit Dialog()
        let selected = FlowContainer()
        let available = FlowContainer()
        do
            selected.Add(TextBox(K "(fallback)", K Color.White, 0.5f) |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 50.0f, 0.0f))
            this.Add(available |> Frame.Create |> positionWidget(200.0f, 0.0f, 200.0f, 0.0f, -50.0f, 0.5f, -200.0f, 1.0f))
            this.Add(selected |> Frame.Create |> positionWidget(50.0f, 0.5f, 200.0f, 0.0f, -200.0f, 1.0f, -200.0f, 1.0f))
            this.Add(TextBox(K (Localisation.localise "options.select.Available"), K (Color.White, Color.Black), 0.5f) |> positionWidget(200.0f, 0.0f, 100.0f, 0.0f, -50.0f, 0.5f, 200.0f, 0.0f))
            this.Add(TextBox(K (Localisation.localise "options.select.Selected"), K (Color.White, Color.Black), 0.5f) |> positionWidget(50.0f, 0.5f, 100.0f, 0.0f, -200.0f, 1.0f, 200.0f, 0.0f))
            this.Add(Button(this.Close, (Localisation.localise "options.select.Confirm"), options.Hotkeys.Select, Sprite.Default) |> positionWidget(-100.0f, 0.5f, -150.0f, 1.0f, 100.0f, 0.5f, -50.0f, 1.0f))
            Themes.refreshAvailableThemes()
            for t in Themes.availableThemes do
                if options.EnabledThemes.Contains(t) |> not then
                    available.Add(ThemeItem(t, this))
            for t in options.EnabledThemes do
                selected.Add(ThemeItem(t, this))
        member this.Selected: FlowContainer = selected
        member this.Available: FlowContainer = available
        override this.OnClose() =
            Themes.loadThemes(options.EnabledThemes)

open SelectionWheel

type OptionsMenu() as this =
    inherit Dialog()
    let t s = Localisation.localise("options.name." + s)
    let sw =
        swBuilder [
            swItemBuilder [
                Slider((options.AudioOffset :?> FloatSetting), t "AudioOffset", fun () -> Audio.globalOffset <- float32 (options.AudioOffset.Get()) * 1.0f<ms>) :> ISelectable
                Slider((options.AudioVolume :?> FloatSetting), t "AudioVolume", fun () -> Audio.changeVolume(options.AudioVolume.Get()))  :> ISelectable
                Selector.FromEnum(config.WindowMode, t "WindowMode", Options.applyOptions)  :> ISelectable
                //todo: resolution DU editor
                Selector([|"UNLIMITED"; "30"; "60"; "90"; "120"; "240"|], int(config.FrameLimiter.Get() / 30.0) |> min(5),
                    (let e = [|0.0; 30.0; 60.0; 90.0; 120.0; 240.0|] in fun (i, _) -> config.FrameLimiter.Set(e.[i])), t "FrameLimiter", Options.applyOptions) :> ISelectable
                ](t "System")
            swItemBuilder [
                Slider((options.ScrollSpeed :?> FloatSetting), t "ScrollSpeed", ignore) :> ISelectable
                Slider((options.HitPosition :?> IntSetting), t "HitPosition", ignore) :> ISelectable
                Selector.FromBool(options.Upscroll, t "Upscroll", ignore) :> ISelectable
                Slider((options.BackgroundDim :?> FloatSetting), t "BackgroundDim", ignore) :> ISelectable
                swItemBuilder [
                    //todo: preview of screencover
                    Slider((options.ScreenCoverDown :?> FloatSetting), t "ScreenCoverDown", ignore) :> ISelectable
                    Slider((options.ScreenCoverUp :?> FloatSetting), t "ScreenCoverUp", ignore) :> ISelectable
                    Slider((options.ScreenCoverFadeLength :?> IntSetting), t "ScreenCoverFadeLength", ignore) :> ISelectable
                    ](t "ScreenCover") :> ISelectable
                //todo: pacemaker DU editor
                //todo: accuracy and hp system selector
                ](t "Gameplay")
            swItemBuilder [
                { new ActionItem(t "NewTheme", ignore) with override this.Select() = Screens.addDialog(TextInputDialog(this.Bounds, "NYI", ignore)) }
                new ActionItem(t "ChangeTheme", fun () -> Screens.addDialog(new ThemeSelector()))
                ](t "Themes")
            let mutable keycount = options.KeymodePreference.Get()
            let f km i =
                { new ISettable<_>() with
                    override this.Set(v) = options.GameplayBinds.[km].[i] <- v
                    override this.Get() = options.GameplayBinds.[km].[i] }
            let binder = SelectionRow((fun () -> keycount), (fun i -> KeyBinder(f (keycount - 3) i, "", false, ignore) :> ISelectable), t "GameplayBinds", ignore)
            swItemBuilder [
                Selector.FromKeymode(
                    { new ISettable<int>() with
                        override this.Set(v) = keycount <- v + 3
                        override this.Get() = keycount - 3 }, binder.Refresh) :> ISelectable
                binder :> ISelectable
                ](t "Noteskin")
            swItemBuilder [ActionItem("TODO", ignore)](t "Hotkeys")
            swItemBuilder [ActionItem("TODO", ignore)](t "Debug")]
    do
        sw.Select()
        this.Add(sw)
    override this.OnClose() = ()
    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if not sw.Selected then this.Close()