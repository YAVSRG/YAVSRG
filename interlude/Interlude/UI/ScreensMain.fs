namespace Interlude.UI

open System
open System.Drawing
open OpenTK.Mathematics
open Interlude
open Interlude.Render
open Interlude.UI.Animation
open Interlude.UI.Components
open Interlude.Utils
open Interlude.Input
open Interlude.Options

// Menu screen

type ScreenMenu() =
    inherit Screen()

    override this.OnEnter(prev: Screen) =
        Screens.logo.Move(-Render.vwidth * 0.5f, -400.0f, 800.0f - Render.vwidth * 0.5f, 400.0f)
        Screens.backgroundDim.SetTarget(0.0f)
        Screens.setToolbarCollapsed(false)

    override this.OnExit(next: Screen) =
        Screens.logo.Move(-Render.vwidth * 0.5f - 600.0f, -300.0f, -Render.vwidth * 0.5f, 300.0f)
        Screens.backgroundDim.SetTarget(0.7f)

    override this.Draw() =
        let (x, y) = Rect.center this.Bounds
        base.Draw()

    override this.Update(time, bounds) =
        base.Update(time, bounds)
        if (Options.options.Hotkeys.Select.Get().Tapped(false)) then
            Screens.addScreen(ScreenLevelSelect >> (fun s -> s :> Screen), ScreenTransitionFlag.Default)

// Loading screen

type ScreenLoading() as this =
    inherit Screen()

    let mutable closing = false
    let fade = new AnimationFade(1.0f)
    do
        this.Animation.Add(fade)

    override this.OnEnter(prev: Screen) =
        fade.SetValue(0.0f)
        Screens.logo.Move(-400.0f, -400.0f, 400.0f, 400.0f)
        Screens.setToolbarCollapsed(true)
        match prev with
        | :? ScreenMenu ->
            closing <- true
            let s = AnimationSequence()
            s.Add(AnimationTimer(1500.0))
            s.Add(AnimationAction(fun () -> Screens.popScreen(ScreenTransitionFlag.Default)))
            this.Animation.Add(s)
        | _ -> 
            let s = AnimationSequence()
            s.Add(AnimationTimer(1500.0))
            s.Add(AnimationAction(fun () -> Screens.addScreen(ScreenMenu >> (fun s -> s :> Screen), ScreenTransitionFlag.UnderLogo)))
            this.Animation.Add(s)

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        Audio.changeVolume(Options.options.AudioVolume.Get() * float (if closing then 1.0f - fade.Value else fade.Value))
        
    override this.Draw() =
        let (x, y) = Rect.center this.Bounds
        Text.drawJust(Themes.font(), (if closing then "Bye o/" else "Loading :)"), 80.f, x, y - 500.0f, Color.White, 0.5f)

// Toolbar widgets

type Jukebox() as this =
    inherit Widget()
    //todo: right click to seek/tools to pause and play music
    let fade = new AnimationFade(0.0f)
    let slider = new AnimationFade(0.0f)
    do
        this.Animation.Add(fade)
        this.Animation.Add(slider)

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if Options.options.Hotkeys.Volume.Get().Pressed(true) then
            fade.SetTarget(1.0f)
            Options.options.AudioVolume.Set(Options.options.AudioVolume.Get() + float (Mouse.Scroll()) * 0.02)
            Audio.changeVolume(Options.options.AudioVolume.Get())
            slider.SetTarget(float32 <| Options.options.AudioVolume.Get())
        else
            fade.SetTarget(0.0f)

    override this.Draw() =
        let r = Rect.sliceBottom(5.0f) this.Bounds
        Draw.rect(r)(Screens.accentShade(int (255.0f * fade.Value), 0.4f, 0.0f))(Sprite.Default)
        Draw.rect(r |> Rect.sliceLeft(slider.Value * (Rect.width r)))(Screens.accentShade(int (255.0f * fade.Value), 1.0f, 0.0f))(Sprite.Default)


// Toolbar

type Toolbar() as this =
    inherit Widget()

    static let height = 70.f

    let barSlider = new AnimationFade(1.0f)
    let notifSlider = new AnimationFade(0.0f)

    let mutable cursor = true
    let mutable userCollapse = false
    let mutable forceCollapse = true
    
    do
        this.Animation.Add(barSlider)
        this.Animation.Add(notifSlider)
        this.Add(new TextBox(K version, K (Color.White, Color.Black), 1.0f) |> positionWidget(-300.f, 1.f, 0.f, 1.f, 0.f, 1.f, height * 0.5f, 1.f))
        this.Add(new TextBox((fun () -> System.DateTime.Now.ToString()), K (Color.White, Color.Black), 1.0f) |> positionWidget(-300.f, 1.f, height * 0.5f, 1.f, 0.f, 1.f, height, 1.f))
        this.Add(new Button((fun () -> Screens.popScreen(ScreenTransitionFlag.Default)), "Back", Options.options.Hotkeys.Exit, Sprite.Default) |> positionWidget(0.0f, 0.0f, 0.0f, 1.0f, 200.f, 0.0f, height, 1.0f))
        this.Add(new Button((fun () -> Screens.addDialog(new OptionsMenu())), "Options", Options.options.Hotkeys.Options, Sprite.Default) |> positionWidget(0.0f, 0.0f, -height, 0.0f, 200.f, 0.0f, 0.0f, 0.0f))
        this.Add(new Button((fun () -> (ScreenImport >> (fun s -> s :> Screen), ScreenTransitionFlag.Default) |> Screens.addScreen), "Import", Options.options.Hotkeys.Import, Sprite.Default) |> positionWidget(200.0f, 0.0f, -height, 0.0f, 400.f, 0.0f, 0.0f, 0.0f))
        this.Add(new Button(ignore, "Help", Options.options.Hotkeys.Help, Sprite.Default) |> positionWidget(400.0f, 0.0f, -height, 0.0f, 600.f, 0.0f, 0.0f, 0.0f))
        this.Add(new Jukebox())

        Screens.setToolbarCollapsed <- (fun b -> forceCollapse <- b)
        Screens.setCursorVisible <- (fun b -> cursor <- b)

    override this.Draw() = 
        let struct (l, t, r, b) = this.Bounds
        Draw.rect(Rect.create l (t - height) r t) (Screens.accentShade(127, 0.8f, 0.0f)) Sprite.Default
        Draw.rect(Rect.create l b r (b + height)) (Screens.accentShade(127, 0.8f, 0.0f)) Sprite.Default
        if barSlider.Value > 0.01f then
            let s = (r - l) / 48.0f
            for i in 0 .. 47 do
                let level = System.Math.Min((Audio.waveForm.[i] + 0.01f) * barSlider.Value * 0.4f, height)
                Draw.rect(Rect.create (l + float32 i * s + 2.0f) (t - height) (l + (float32 i + 1.0f) * s - 2.0f) (t - height + level))(Screens.accentShade(int level, 1.0f, 0.5f))(Sprite.Default)
                Draw.rect(Rect.create (r - (float32 i + 1.0f) * s + 2.0f) (b + height - level) (r - float32 i * s - 2.0f) (b + height))(Screens.accentShade(int level, 1.0f, 0.5f))(Sprite.Default)
        base.Draw()
        if cursor then Draw.rect(Rect.create <| Mouse.X() <| Mouse.Y() <| Mouse.X() + Themes.themeConfig.CursorSize <| Mouse.Y() + Themes.themeConfig.CursorSize)(Screens.accentShade(255, 1.0f, 0.5f))(Themes.getTexture("cursor"))

    override this.Update(elapsed, bounds) =
        if (not forceCollapse) && Options.options.Hotkeys.Toolbar.Get().Tapped(false) then
            userCollapse <- not userCollapse
            barSlider.SetTarget(if userCollapse then 0.0f else 1.0f)
        base.Update(elapsed, Rect.expand (0.f, -height * if forceCollapse then 0.0f else barSlider.Value) bounds)

//Screen manager

type ScreenContainer() as this =
    inherit Widget()

    let dialogs = new ResizeArray<Dialog>()
    let mutable current = new ScreenLoading() :> Screen
    let mutable screens = [current]
    let mutable exit = false

    let transitionTime = 500.0
    let mutable transitionFlags = ScreenTransitionFlag.Default
    let screenTransition = new AnimationSequence()
    let t1 = new AnimationTimer(transitionTime)
    let t2 = new AnimationTimer(transitionTime)

    let toolbar = new Toolbar()

    do
        Screens.addScreen <- this.AddScreen
        Screens.popScreen <- this.RemoveScreen
        Screens.addDialog <- this.AddDialog
        this.Add(toolbar)
        this.Add(Screens.logo |> Components.positionWidget(-300.0f, 0.5f, 1000.0f, 0.5f, 300.0f, 0.5f, 1600.0f, 0.5f))
        this.Animation.Add(screenTransition)
        this.Animation.Add(Screens.accentColor)
        this.Animation.Add(Screens.parallaxZ)
        this.Animation.Add(Screens.parallaxX)
        this.Animation.Add(Screens.parallaxY)
        this.Animation.Add(Screens.backgroundDim)
        current.OnEnter(current)

    member this.Exit = exit

    member this.AddDialog(d: Dialog) =
        dialogs.Add(d)

    member this.AddScreen(s: unit -> Screen, flags) =
        transitionFlags <- flags
        if screenTransition.Complete() then
            this.Animation.Add(screenTransition)
            screenTransition.Add(t1)
            screenTransition.Add(
                new AnimationAction(
                    fun () ->
                        let s = s()
                        if (flags &&& ScreenTransitionFlag.NoBacktrack <> ScreenTransitionFlag.NoBacktrack) then screens <- s :: screens
                        current.OnExit(s)
                        s.OnEnter(current)
                        current <- s))
            screenTransition.Add(t2)
            t2.FrameSkip() //ignore frame lag spike when initialising screen
            screenTransition.Add(new AnimationAction(fun () -> t1.Reset(); t2.Reset()))

    member this.RemoveScreen(flags) =
        transitionFlags <- flags
        if screenTransition.Complete() then
            this.Animation.Add(screenTransition)
            screenTransition.Add(t1)
            screenTransition.Add(
                new AnimationAction(
                    fun () ->
                        current.Dispose()
                        let previous = current
                        screens <- List.tail screens
                        match List.tryHead screens with
                        | None -> exit <- true
                        | Some s ->
                            current.OnExit(s)
                            current <- s
                            s.OnEnter(previous)))
            screenTransition.Add(t2)
            screenTransition.Add(new AnimationAction(fun () -> t1.Reset(); t2.Reset()))

    override this.Update(elapsedTime, bounds) =
        if Render.vwidth > 0.0f then
            Screens.parallaxX.SetTarget(Mouse.X() / Render.vwidth)
            Screens.parallaxY.SetTarget(Mouse.Y() / Render.vheight)
        Screens.accentColor.SetColor(Themes.accentColor)
        if dialogs.Count > 0 then
            dialogs.[dialogs.Count - 1].Update(elapsedTime, bounds)
            if dialogs.[dialogs.Count - 1].State = WidgetState.Disabled then
                dialogs.[dialogs.Count - 1].Dispose()
                dialogs.RemoveAt(dialogs.Count - 1)
            current.Animation.Update(elapsedTime)
            Screens.logo.Update(elapsedTime, bounds)
        else
            base.Update(elapsedTime, bounds)
            current.Update(elapsedTime, toolbar.Bounds)

    override this.Draw() =
        Screens.drawBackground(this.Bounds, Color.White, 1.0f)
        Draw.rect this.Bounds (Color.FromArgb(Screens.backgroundDim.Value * 255.0f |> int, 0, 0, 0)) Sprite.Default
        current.Draw()
        base.Draw()
        //TODO: move all this transitional logic somewhere nice and have lots of them
        if not <| screenTransition.Complete() then
            let amount = Math.Clamp((if t1.Elapsed < transitionTime then t1.Elapsed / transitionTime else (transitionTime - t2.Elapsed) / transitionTime), 0.0, 1.0) |> float32

            let s = 150.0f

            let size x =
                let f = Math.Clamp(((if t1.Elapsed < transitionTime then amount else 1.0f - amount) - (x - 2.0f * s) / Render.vwidth) / ((4.0f * s) / Render.vwidth), 0.0f, 1.0f)
                if t1.Elapsed < transitionTime then f * s * 0.5f else (1.0f - f) * s * 0.5f
            let diamond x y =
                let r = size x
                Draw.quad(Quad.create <| new Vector2(x - r, y) <| new Vector2(x, y - r) <| new Vector2(x + r, y) <| new Vector2(x, y + r))(Quad.colorOf Color.Transparent)(Sprite.DefaultQuad)
                
            Stencil.create(false)
            for x in 0 .. (Render.vwidth / s |> float |> Math.Ceiling |> int) do
                for y in 0 .. (Render.vheight / s |> float |> Math.Ceiling |> int) do
                    diamond (s * float32 x) (s * float32 y)
                    diamond (0.5f * s + s * float32 x) (0.5f * s + s * float32 y)
            Stencil.draw()
            Screens.drawBackground(this.Bounds, Screens.accentShade(255.0f * amount |> int, 1.0f, 0.0f), 1.0f)
            Stencil.finish()
            if (transitionFlags &&& ScreenTransitionFlag.UnderLogo = ScreenTransitionFlag.UnderLogo) then Screens.logo.Draw()
        for d in dialogs do
            d.Draw()