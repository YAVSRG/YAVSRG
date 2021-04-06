namespace Interlude.UI

open System
open System.Drawing
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Interlude
open Interlude.Options
open Interlude.Render
open Interlude.UI.Animation
open Interlude.Input
open Interlude.Utils
open OpenTK

module Components =
    
    let positionWidgetA(l, t, r, b) (w: Widget) : Widget =
        w.Reposition(l, t, r, b)
        w

    let positionWidget(l, la, t, ta, r, ra, b, ba) (w: Widget) : Widget =
        w.Reposition(l, la, t, ta, r, ra, b, ba)
        w
    
    type Frame(fillColor: unit -> Color, frameColor: unit -> Color, fill, frame) =
        inherit Widget()

        let BORDERWIDTH = 5.0f

        new() = Frame((fun () -> Screens.accentShade(200, 0.5f, 0.3f)), (fun () -> Screens.accentShade(80, 0.5f, 0.0f)), true, true)
        new((), frame) = Frame((K Color.Transparent), (K frame), false, true)
        new((), frame) = Frame((K Color.Transparent), frame, false, true)
        new(fill, ()) = Frame((K fill), (K Color.Transparent), true, false)
        new(fill, ()) = Frame(fill, (K Color.Transparent), true, false)
        new(fill, frame) = Frame((K fill), (K frame), true, true)
        new(fill, frame) = Frame(fill, frame, true, true)

        override this.Draw() =
            if frame then
                let c = frameColor()
                let r = Rect.expand(BORDERWIDTH, BORDERWIDTH) this.Bounds
                Draw.rect (Rect.sliceLeft BORDERWIDTH r) c Sprite.Default
                Draw.rect (Rect.sliceRight BORDERWIDTH r) c Sprite.Default
                let r = Rect.expand(0.0f, BORDERWIDTH) this.Bounds
                Draw.rect (Rect.sliceTop BORDERWIDTH r) c Sprite.Default
                Draw.rect (Rect.sliceBottom BORDERWIDTH r) c Sprite.Default

            if fill then
                Draw.rect base.Bounds (fillColor()) Sprite.Default
            base.Draw()
        static member Create(w: Widget) =
            let f = Frame()
            f.Add(w)
            f

    type TextBox(textFunc, color, just) =
        inherit Widget()

        new(textFunc, scolor, just) = TextBox(textFunc, (fun () -> scolor(), Color.Transparent), just)

        override this.Draw() = 
            Text.drawFillB(Themes.font(), textFunc(), this.Bounds, color(), just)
            base.Draw()

    type Clickable(onClick, onHover) =
        inherit Widget()

        let mutable hover = false

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            let oh = hover
            hover <- Mouse.Hover(this.Bounds)
            if oh && not hover then onHover(false)
            elif not oh && hover && Mouse.Moved() then onHover(true)
            elif hover && Mouse.Click(MouseButton.Left) then onClick()

    type Button(onClick, label, bind: ISettable<Bind>, sprite) as this =
        inherit Widget()

        let color = AnimationFade(0.3f)

        do
            this.Animation.Add(color)
            this.Add(new Clickable(onClick, fun b -> color.Target <- if b then 0.7f else 0.3f))

        override this.Draw() =
            Draw.rect this.Bounds (Screens.accentShade(80, 0.5f, color.Value)) Sprite.Default
            Draw.rect (Rect.sliceBottom 10.0f this.Bounds) (Screens.accentShade(255, 1.0f, color.Value)) Sprite.Default
            Text.drawFillB(Themes.font(), label, Rect.trimBottom 10.0f this.Bounds, (Screens.accentShade(255, 1.0f, color.Value), Screens.accentShade(255, 0.4f, color.Value)), 0.5f)

        override this.Update(elapsedTime, bounds) =
            if bind.Get().Tapped() then onClick()
            base.Update(elapsedTime, bounds)

    type FlowContainer() =
        inherit Widget()
        let mutable spacing = 10.0f
        let mutable margin = (0.0f, 0.0f)
        let mutable contentSize = 0.0f
        let mutable scrollPos = 0.0f

        member this.Spacing with set(value) = spacing <- value
        //todo: margin doesn't work correctly
        member this.Margin with set((x, y)) = margin <- (-x, -y)

        member private this.FlowContent(thisBounds) =
            let mutable vBounds = thisBounds |> Rect.expand margin |> Rect.translate(0.0f, -scrollPos)
            let struct (left, top, right, bottom) = thisBounds
            let struct (_, t1, _, _) = vBounds
            for c in this.Children do
                if c.Enabled then
                    let (la, ta, ra, ba) = c.Anchors
                    let struct (l, t, r, b) = vBounds
                    let struct (lb, tb, rb, bb) =
                        if c.Initialised then Rect.createWH l t (Rect.width c.Bounds) (Rect.height c.Bounds)
                        else Rect.create (la.Position(l, r)) (ta.Position(t, b)) (ra.Position(l, r)) (ba.Position(t, b))
                    let pos (a: AnchorPoint) = if c.Initialised then a.MoveRelative else a.RepositionRelative
                    pos la (left, right, lb); pos ta (top, bottom, tb); pos ra (left, right, rb); pos ba (top, bottom, bb)
                    vBounds <- Rect.translate(0.0f, bb - tb + spacing) vBounds
            let struct (_, t2, _, _) = vBounds
            contentSize <- t2 - t1

        override this.Update(elapsedTime, bounds) =
            //todo: fix for ability to interact with components that appear outside of the container (they should update but clickable components should stop working)
            let thisBounds =
                if this.Initialised then this.Bounds
                else
                    let (left, top, right, bottom) = this.Anchors
                    let struct (l, t, r, b) = bounds
                    Rect.create <| left.Position(l, r) <| top.Position(t, b) <| right.Position(l, r) <| bottom.Position(t, b)
            this.FlowContent(thisBounds) 
            base.Update(elapsedTime, bounds)
            if Mouse.Hover(this.Bounds) then scrollPos <- Math.Max(0.0f, Math.Min(scrollPos - Mouse.Scroll() * 100.0f, contentSize - Rect.height this.Bounds))

        override this.Draw() =
            Stencil.create(false)
            Draw.rect this.Bounds Color.Transparent Sprite.Default
            Stencil.draw()
            let struct (_, top, _, bottom) = this.Bounds
            for c in this.Children do
                if c.Initialised && c.Enabled then
                    let struct (_, t, _, b) = c.Bounds
                    if t < bottom && b > top then c.Draw()
            Stencil.finish()

        member this.Filter(f: Widget -> bool) = for c in this.Children do c.Enabled <- f c
        member this.Sort(comp: Comparison<Widget>) = this.Children.Sort(comp)

    type Dropdown(options: string array, index, func, label, buttonSize) as this =
        inherit Widget()

        let color = AnimationFade(0.5f)
        let mutable index = index

        do
            this.Animation.Add(color)
            let fr = new Frame(Enabled = false)
            this.Add((Clickable((fun () -> fr.Enabled <- not fr.Enabled), fun b -> color.Target <- if b then 0.8f else 0.5f)) |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, buttonSize, 0.0f))
            this.Add(
                let fc = FlowContainer(Spacing = 0.0f)
                fr.Add(fc)
                Array.iteri
                    (fun i o -> fc.Add(Button((fun () -> index <- i; func(i)), o, Bind.DummyBind, Sprite.Default) |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 40.0f, 0.0f)))
                    options
                fr |> positionWidgetA(0.0f, buttonSize, 0.0f, 0.0f))
            
        override this.Draw() =
            let bbounds = Rect.sliceTop buttonSize this.Bounds
            Draw.rect (Rect.expand (5.0f, 5.0f) bbounds) (Screens.accentShade(127, 0.5f, 0.0f)) Sprite.Default
            Draw.rect bbounds (Screens.accentShade(255, 0.6f, 0.0f)) Sprite.Default
            Text.drawFill(Themes.font(), label, Rect.sliceTop 20.0f bbounds, Color.White, 0.5f)
            Text.drawFill(Themes.font(), options.[index], bbounds |> Rect.trimTop 20.0f, Color.White, 0.5f)
            base.Draw()

    type TextEntry(s: ISettable<string>, bind: ISettable<Bind> option, prompt: string) as this =
        inherit Frame()

        let color = AnimationFade(0.5f)

        let mutable active = false
        let toggle() =
            active <- not active
            if active then
                color.Target <- 1.0f
                Input.setInputMethod(s, fun () -> active <- false; color.Target <- 0.5f)
            else
                color.Target <- 0.5f
                Input.removeInputMethod()

        do
            this.Animation.Add(color)
            if Option.isNone bind then toggle() else this.Add(new Clickable(toggle, ignore))
            this.Add(
                TextBox(
                    (fun () ->
                        match bind with
                        | Some b ->
                            match s.Get() with
                            //todo: localise
                            | "" -> sprintf "Press %s to %s" (b.Get().ToString()) prompt
                            | text -> text
                        | None -> match s.Get() with "" -> prompt | text -> text),
                    (fun () -> Screens.accentShade(255, 1.0f, color.Value)), 0.0f))

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            match bind with
            | Some b -> if b.Get().Tapped() then toggle()
            | None -> ()

        override this.Dispose() =
            if active then Input.removeInputMethod()

    type SearchBox(s: ISettable<string>, callback: Prelude.Data.ChartManager.Sorting.Filter -> unit) as this =
        inherit Widget()
        //todo: this seems excessive. replace with two variables?
        let searchTimer = new System.Diagnostics.Stopwatch()
        do this.Add <| TextEntry(new WrappedSetting<string, string>(s, (fun s -> searchTimer.Restart(); s), id), Some (options.Hotkeys.Search :> ISettable<Bind>), "search")

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if searchTimer.ElapsedMilliseconds > 400L then searchTimer.Reset(); callback(s.Get() |> Prelude.Data.ChartManager.Sorting.parseFilter)

    type TextInputDialog(bounds: Rect, prompt, callback) as this =
        inherit Dialog()
        let buf = Setting<string>("")
        let tb = TextEntry(buf, None, prompt)
        do
            let struct (l, t, r, b) = bounds
            this.Add(tb |> positionWidget(l, 0.0f, t, 0.0f, r, 0.0f, b, 0.0f))
        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if options.Hotkeys.Select.Get().Tapped() || options.Hotkeys.Exit.Get().Tapped() then tb.Dispose(); this.Close()
        override this.OnClose() = callback(buf.Get())

    //provide the first tab when constructing
    type TabContainer(name: string, widget: Widget) as this =
        inherit Widget()
        let mutable selectedItem = widget
        let mutable selected = name
        let mutable count = 0.0f

        let TABHEIGHT = 60.0f
        let TABWIDTH = 250.0f

        do this.AddTab(name, widget)

        member this.AddTab(name, widget) =
            this.Add(
                { new Button((fun () -> selected <- name; selectedItem <- widget), name, Bind.DummyBind, Sprite.Default) with member this.Dispose() = base.Dispose(); widget.Dispose() }
                |> positionWidget(count * TABWIDTH, 0.0f, 0.0f, 0.0f, (count+1.0f) * TABWIDTH, 0.0f, TABHEIGHT, 0.0f))
            count <- count + 1.0f

        override this.Draw() =
            base.Draw()
            selectedItem.Draw()

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            selectedItem.Update(elapsedTime, Rect.trimTop TABHEIGHT this.Bounds)
            