namespace Interlude.UI

open System
open Prelude.Common
open Interlude
open Interlude.Options
open Interlude.Render
open Interlude.UI.Animation
open Interlude.Input
open OpenTK

module Components =
    
    let positionWidgetA(l, t, r, b) (w: Widget) : Widget =
        w.Reposition(l, t, r, b)
        w

    let positionWidget(l, la, t, ta, r, ra, b, ba) (w: Widget) : Widget =
        w.Reposition(l, la, t, ta, r, ra, b, ba)
        w
    
    type Frame() =
        inherit Widget()
        override this.Draw() =
            Draw.rect <| Rect.expand(5.0f, 5.0f) base.Bounds <| Screens.accentShade(127, 0.5f, 0.0f) <| Sprite.Default
            Draw.rect <| base.Bounds <| Screens.accentShade(255, 0.8f, 0.0f) <| Sprite.Default
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

        override this.Update(time, bounds) =
            base.Update(time, bounds)
            let oh = hover
            hover <- Mouse.Hover(this.Bounds)
            if oh <> hover then onHover(hover)
            if hover && Mouse.Click(Input.MouseButton.Left) then onClick()

    type Button(onClick, label, bind: ISettable<Bind>, sprite) as this =
        inherit Widget()

        let color = AnimationFade(0.3f)

        do
            this.Animation.Add(color)
            this.Add(new Clickable(onClick, fun b -> color.SetTarget(if b then 0.7f else 0.3f)))

        override this.Draw() =
            Draw.rect this.Bounds (Screens.accentShade(80, 0.5f, color.Value)) Sprite.Default
            Draw.rect (Rect.sliceBottom 10.0f this.Bounds) (Screens.accentShade(255, 1.0f, color.Value)) Sprite.Default
            Text.drawFillB(Themes.font(), label, Rect.trimBottom 10.0f this.Bounds, (Screens.accentShade(255, 1.0f, color.Value), Screens.accentShade(255, 0.4f, color.Value)), 0.5f)

        override this.Update(bounds, elapsedTime) =
            if bind.Get().Tapped(false) then onClick()
            base.Update(bounds, elapsedTime)

    type FlowContainer(?spacingX: float32, ?spacingY: float32) =
        inherit Widget()
        let spacingX, spacingY = (defaultArg spacingX 10.0f, defaultArg spacingY 5.0f)
        let mutable contentSize = 0.0f
        let mutable scrollPos = 0.0f

        member private this.FlowContent(instant) =
            let width = Rect.width this.Bounds
            let height = Rect.height this.Bounds
            let pos (anchor : AnchorPoint) = if instant then anchor.RepositionRelative else anchor.MoveRelative
            lock(this)
                (fun () ->
                    contentSize <-
                        (this.Children
                        |> Seq.filter (fun c -> c.State &&& WidgetState.Disabled < WidgetState.Disabled)
                        |> Seq.fold (fun (x, y) w ->
                            let (l, t, r, b) = w.Position
                            let cwidth = r.Position(0.0f, width) - l.Position(0.0f, width)
                            let cheight = b.Position(0.0f, height) - t.Position(0.0f, height)
                            let x = x + cwidth + spacingX
                            let x, y = if x > width then cwidth + spacingX, y + cheight + spacingY else x, y
                            pos l (0.0f, width, x - cwidth - spacingX); pos t (0.0f, height, y)
                            pos r (0.0f, width, x - spacingX); pos b (0.0f, height, y + cheight)
                            (x + spacingY, y)
                            ) (0.0f, -scrollPos)
                        |> snd) + scrollPos)

        override this.Update(time, bounds) =
            if (this.Initialised) then
                this.FlowContent(false) 
                base.Update(time, bounds)
                if Mouse.Hover(this.Bounds) then scrollPos <- Math.Max(0.0f, Math.Min(scrollPos - (Mouse.Scroll() |> float32) * 100.0f, contentSize - Rect.height this.Bounds))
            else
                //todo: fix for ability to interact with components that appear outside of the container (they should update but clickable components should stop working)
                base.Update(time, bounds)
                this.FlowContent(true)

        override this.Draw() =
            Stencil.create(false)
            Draw.rect(this.Bounds)(Color.Transparent)(Sprite.Default)
            Stencil.draw()
            let struct (_, top, _, bottom) = this.Bounds
            lock(this)
                (fun () ->
                    for c in this.Children do
                        if c.State < WidgetState.Disabled then
                            let struct (_, t, _, b) = c.Bounds
                            if t < bottom && b > top then c.Draw())
            Stencil.finish()

        override this.Add(child) =
            base.Add(child)
            if (this.Initialised) then this.FlowContent(true)

        member this.Clear() = this.Children.Clear()

    type Dropdown(options: string array, index, func, label, buttonSize) as this =
        inherit Widget()

        let color = AnimationFade(0.5f)
        let mutable index = index

        do
            this.Animation.Add(color)
            let fr = new Frame()
            this.Add((Clickable((fun () -> fr.State <- fr.State ^^^ WidgetState.Disabled), fun b -> color.SetTarget(if b then 0.8f else 0.5f))) |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, buttonSize, 0.0f))
            this.Add(
                let fc = FlowContainer(0.0f, 0.0f)
                fr.Add(fc)
                Array.iteri
                    (fun i o -> fc.Add(Button((fun () -> index <- i; func(i)), o, Bind.DummyBind, Sprite.Default) |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 40.0f, 0.0f)))
                    options
                fr |> positionWidgetA(0.0f, buttonSize, 0.0f, 0.0f))
            fr.State <- WidgetState.Disabled
            
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
                color.SetTarget(1.0f)
                Input.createInputMethod(s)
            else
                color.SetTarget(0.5f)
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
                            | "" -> sprintf "Press %s to %s" (b.Get().ToString()) prompt
                            | text -> text
                        | None -> match s.Get() with "" -> prompt | text -> text),
                    (fun () -> Screens.accentShade(255, 1.0f, color.Value)), 0.0f))

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            match bind with
            | Some b -> if b.Get().Tapped(true) then toggle()
            | None -> ()

        override this.Dispose() =
            if active then Input.removeInputMethod()

    type TextInputDialog(bounds: Rect, prompt, callback) as this =
        inherit Dialog()
        let buf = Setting<string>("")
        let tb = TextEntry(buf, None, prompt)
        do
            let struct (l, t, r, b) = bounds
            this.Add(tb |> positionWidget(l, 0.0f, t, 0.0f, r, 0.0f, b, 0.0f))
        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if options.Hotkeys.Select.Get().Tapped(true) || options.Hotkeys.Exit.Get().Tapped(true) then tb.Dispose(); this.Close()
        override this.OnClose() = callback(buf.Get())
