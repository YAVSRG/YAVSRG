namespace Interlude.UI

open System
open System.Drawing
open System.Collections.Generic
open OpenTK
open Prelude.Common
open Interlude.Options
open Interlude.Render
open Interlude
open Interlude.Utils
open Interlude.Input
open OpenTK.Windowing.GraphicsLibraryFramework
open Interlude.Options
open Interlude.UI.Animation
open Interlude.UI.Components

module SelectionWheel =

    let WIDTH = 350.0f

    let t s = Localisation.localise("options.name." + s)

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

    //todo: maybe try making this a special case of SelectionContainer if im feeling big brain
    type SelectionWheel(onDeselect) as this  =
        inherit ISelectable(onDeselect)

        let mutable index = 0
        let items = new List<ISelectable>()
        let collapse = new AnimationFade(1.0f)
        do this.Animation.Add(collapse)
        override this.Select() = base.Select(); collapse.Target <- 0.0f; if items.[index].AutoSelect then items.[index].Select()
        override this.Deselect() = base.Deselect(); collapse.Target <- 1.0f
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
                if options.Hotkeys.Down.Get().Tapped() then
                    if flag = 1 then items.[index].Deselect()
                    index <- (index + 1) % items.Count
                    if items.[index].AutoSelect then items.[index].Select()
                elif options.Hotkeys.Up.Get().Tapped() then
                    if flag = 1 then items.[index].Deselect()
                    index <- (index + items.Count - 1) % items.Count
                    if items.[index].AutoSelect then items.[index].Select()
                elif options.Hotkeys.Exit.Get().Tapped() || (flag = 0 && options.Hotkeys.Previous.Get().Tapped()) then
                    this.DeselectChild()
                    this.Deselect()
                if flag = 0 && (options.Hotkeys.Select.Get().Tapped() || options.Hotkeys.Next.Get().Tapped()) then items.[index].Select()
        
        override this.AutoSelect = false

    type SelectionContainer(onDeselect, horizontal) =
        inherit ISelectable(onDeselect)

        let mutable index = 0
        let items = new List<ISelectable>()

        let fd() = if horizontal then options.Hotkeys.Next.Get().Tapped() else options.Hotkeys.Down.Get().Tapped()
        let bk() = if horizontal then options.Hotkeys.Previous.Get().Tapped() else options.Hotkeys.Up.Get().Tapped()
        let sel() = (not horizontal && options.Hotkeys.Next.Get().Tapped()) || options.Hotkeys.Select.Get().Tapped()
        let desel() = options.Hotkeys.Exit.Get().Tapped()

        member this.Clear() = index <- 0; Seq.iter (fun (w: ISelectable) -> w.Dispose(); this.Remove(w)) items; items.Clear()
        override this.DeselectChild() = Seq.iter (fun (w: ISelectable) -> if w.Selected then w.Deselect()) items
        override this.Select() = base.Select(); if items.[index].AutoSelect then items.[index].Select()
        override this.Deselect() = this.DeselectChild(); base.Deselect()

        override this.Add(w) = if w :? ISelectable then items.Add(w :?> ISelectable); base.Add(w)

        override this.Draw() =
            if this.Selected then
                for i in 0 .. (items.Count - 1) do
                    let w = items.[i]
                    if index = i && not w.Selected then Draw.rect w.Bounds (Color.FromArgb(160,180,180,180)) Sprite.Default
            base.Draw()

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            let mutable flag = 0
            for w in items do if w.Selected then flag <- if w.AutoSelect then 1 else 2
            if this.Selected && flag < 2 then
                if fd() then
                    if flag = 1 then items.[index].Deselect()
                    index <- (index + 1) % items.Count
                    if items.[index].AutoSelect then items.[index].Select()
                elif bk() then
                    if flag = 1 then items.[index].Deselect()
                    index <- (index + items.Count - 1) % items.Count
                    if items.[index].AutoSelect then items.[index].Select()
                elif desel() then
                    this.Deselect()
                if flag = 0 && sel() then items.[index].Select()

        override this.AutoSelect = true

    type SelectionRow(number, cons, name, onDeselect) as this =
        inherit SelectionContainer(onDeselect, true)

        let SCALE = 150.0f
        do this.Refresh()
        member this.Refresh() =
            let l = this.Clear()
            let n = number()
            let x = -SCALE * 0.5f * float32 n
            for i in 0 .. number() - 1 do
                let w: ISelectable = cons i
                w.Reposition(x + float32 i * SCALE, 0.5f, 0.0f, 0.0f, x + SCALE + float32 i * SCALE, 0.5f, SCALE, 0.0f)
                this.Add(w)

    type ActionItem(name, action) as this =
        inherit ISelectable(ignore)
        do
            this.Add(new TextBox(K name, K (Color.White, Color.Black), 0.5f))
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 60.0f, 0.0f)
        override this.Select() = action()
        override this.DeselectChild() = ()
        override this.AutoSelect = false

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
                if options.Hotkeys.Next.Get().Tapped() then fd()
                elif options.Hotkeys.Previous.Get().Tapped() then bk()

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
            this.Add(new Clickable((fun () -> dragging <- true), fun b -> color.Target <- if b then 0.8f else 0.5f))

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            let struct (l, t, r, b) = this.Bounds |> Rect.expand(-15.0f, -15.0f)
            if this.Selected then
                if (Mouse.Held(MouseButton.Left) && dragging) then
                    let amt = (Mouse.X() - l) / (r - l)
                    setting.SetPercent(amt)
                else
                    dragging <- false
                if options.Hotkeys.UpRateHalf.Get().Tapped() || options.Hotkeys.Next.Get().Tapped() then chPercent(0.05f)
                elif options.Hotkeys.UpRateSmall.Get().Tapped() then chPercent(0.01f)
                elif options.Hotkeys.UpRate.Get().Tapped() then chPercent(0.1f)
                elif options.Hotkeys.DownRateHalf.Get().Tapped() || options.Hotkeys.Previous.Get().Tapped() then chPercent(-0.05f)
                elif options.Hotkeys.DownRateSmall.Get().Tapped() then chPercent(-0.01f)
                elif options.Hotkeys.DownRate.Get().Tapped() then chPercent(-0.1f)

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
        override this.Draw() =
            if name = "" then Draw.rect(this.Bounds |> Rect.expand(0.0f, -25.0f))(Screens.accentShade(127, 0.8f, 0.0f))Sprite.Default
            base.Draw()
        override this.Update(elapsedTime, bounds) =
            if this.Selected then
                match Input.consumeAny(InputEvType.Press) with
                | ValueNone -> ()
                | ValueSome b ->
                    match b with
                    | Key (k, (ctrl, _, shift)) ->
                        if k = Keys.Escape then setting.Set(Dummy)
                        elif allowModifiers then setting.Set(Key (k, (ctrl, false, shift)))
                        else setting.Set(Key (k, (false, false, false)))
                        this.Deselect()
                    | _ -> ()
            base.Update(elapsedTime, bounds)
        override this.DeselectChild() = ()
        override this.AutoSelect = false

    let swBuilder items = let sw = new SelectionWheel(ignore) in items |> List.iter sw.AddItem; sw
    let swItemBuilder items name = SelectionWheelItem(name, swBuilder items) :> ISelectable

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

[<AutoOpen>]
module LayoutEditor =
    open Prelude.Gameplay.NoteColors

    type private ColorPicker(color: ISettable<byte>) as this =
        inherit ISelectable(ignore)
        let sprite = Themes.getTexture("note")
        let n = byte sprite.Rows
        let fd() = color.Set((color.Get() + n - 1uy) % n)
        let bk() = color.Set((color.Get() + 1uy) % n)
        do this.Add(new Clickable(fd, ignore))
        override this.DeselectChild() = ()
        override this.AutoSelect = false
        override this.Draw() =
            base.Draw()
            if this.Selected then
                Draw.rect(this.Bounds)(Screens.accentShade(127, 1.0f, 0.0f))Sprite.Default
            Draw.quad(this.Bounds |> Quad.ofRect)(Color.White |> Quad.colorOf)(sprite |> Sprite.gridUV(3, int <| color.Get()))
        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if this.Selected then
                if options.Hotkeys.Previous.Get().Tapped() then fd()
                elif options.Hotkeys.Next.Get().Tapped() then bk()

    type LayoutEditor() as this =
        inherit Dialog()
        let sc = SelectionContainer(ignore, false)
        do
            let mutable keycount = options.KeymodePreference.Get()

            let g keycount i =
                let k = if options.ColorStyle.Get().UseGlobalColors then 0 else keycount - 2
                { new ISettable<_>() with
                    override this.Set(v) = options.ColorStyle.Get().Colors.[k].[i] <- v
                    override this.Get() = options.ColorStyle.Get().Colors.[k].[i] }

            let f keycount i =
                let k = keycount - 3
                { new ISettable<_>() with
                    override this.Set(v) = options.GameplayBinds.[k].[i] <- v
                    override this.Get() = options.GameplayBinds.[k].[i] }

            let binder = SelectionRow((fun () -> keycount), (fun i -> KeyBinder(f keycount i, "", false, ignore) :> ISelectable), t "GameplayBinds", ignore)
            binder |> positionWidget(0.0f, 0.0f, 300.0f, 0.0f, 0.0f, 1.0f, 550.0f, 0.0f) |> sc.Add
            let colors = SelectionRow((fun () -> options.ColorStyle.Get().Style |> colorCount keycount), (fun i -> ColorPicker(g keycount i) :> ISelectable), t "NoteColors", ignore)
            colors |> positionWidget(0.0f, 0.0f, 650.0f, 0.0f, 0.0f, 1.0f, 800.0f, 0.0f) |> sc.Add
            Selector.FromKeymode(
                { new ISettable<int>() with
                    override this.Set(v) = keycount <- v + 3
                    override this.Get() = keycount - 3 }, fun () -> binder.Refresh(); colors.Refresh()) |> positionWidget(200.0f, 0.0f, 100.0f, 0.0f, 400.0f, 0.0f, 200.0f, 0.0f) |> sc.Add
            Selector.FromEnum(
                { new ISettable<ColorScheme>() with
                    override this.Set(v) = options.ColorStyle.Set({options.ColorStyle.Get() with Style = v})
                    override this.Get() = options.ColorStyle.Get().Style }, t "ColorStyle", fun () -> colors.Refresh()) |> positionWidget(500.0f, 0.0f, 100.0f, 0.0f, 700.0f, 0.0f, 200.0f, 0.0f) |> sc.Add
            let ns = Themes.noteskins() |> Seq.toArray
            let ids = ns |> Array.map fst
            let names = ns |> Array.map (fun (id, data) -> data.Name)
            Selector(names, Math.Max(0, Array.IndexOf(ids, Themes.currentNoteSkin)), (fun (i, _) -> let id = ns.[i] |> fst in options.NoteSkin.Set(id); Themes.changeNoteSkin(id); colors.Refresh()), t "Noteskin", ignore) |> positionWidget(800.0f, 0.0f, 100.0f, 0.0f, 1100.0f, 0.0f, 200.0f, 0.0f) |> sc.Add
            sc.Select()
            this.Add(sc)
        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if not sc.Selected then this.Close()
        override this.OnClose() = ()

type OptionsMenu() as this =
    inherit Dialog()
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
                    ](t "ScreenCover")
                //todo: pacemaker DU editor
                //todo: accuracy and hp system selector
                ](t "Gameplay")
            swItemBuilder [
                { new ActionItem(t "NewTheme", ignore) with override this.Select() = Screens.addDialog(TextInputDialog(this.Bounds, "NYI", ignore)) }
                new ActionItem(t "ChangeTheme", fun () -> Screens.addDialog(new ThemeSelector()))
                ](t "Themes")
            new ActionItem(t "NoteskinAndLayout", fun () -> Screens.addDialog(new LayoutEditor())) :> ISelectable
            swItemBuilder [ActionItem("TODO", ignore)](t "Hotkeys")
            swItemBuilder [ActionItem("TODO", ignore)](t "Debug")]
    do
        sw.Select()
        this.Add(sw)
    override this.OnClose() = ()
    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if not sw.Selected then this.Close()