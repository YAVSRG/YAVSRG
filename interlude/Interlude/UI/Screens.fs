namespace Interlude.UI

open OpenTK
open Interlude
open Interlude.Render
open Interlude.UI.Animation
open Interlude.Utils
open Interlude.Options

type Screen() =
    inherit Widget()
    abstract member OnEnter: Screen -> unit
    default this.OnEnter(prev: Screen) = ()
    abstract member OnExit: Screen -> unit
    default this.OnExit(next: Screen) = ()

module Screens =
    let mutable internal addScreen: Screen -> unit = ignore
    let mutable internal popScreen: unit -> unit = ignore
    //add dialog

// Menu screen

type ScreenMenu() =
    inherit Screen()

    override this.Draw() =
        let (x, y) = Rect.center this.Bounds
        Font.drawJust(Font.defaultFont, Audio.timeWithOffset().ToString(), 50.f, x, y, Color.White, 0.5f)
        //Draw.rect (Rect.expand (-400.0f, -400.0f) this.Bounds) Color.White <| Themes.getTexture("note")

// Loading screen

type ScreenLoading() =
    inherit Screen()

    let mutable closing = false

    override this.OnEnter(prev: Screen) =
        match prev with
        | :? ScreenMenu ->
            closing <- true
            let s = AnimationSequence()
            s.Add(AnimationTimer(1500.0))
            s.Add(AnimationAction(fun () -> Screens.popScreen()))
            this.Animation.Add(s)
        | _ -> 
            let s = AnimationSequence()
            s.Add(AnimationTimer(1500.0))
            s.Add(AnimationAction(fun () -> Screens.addScreen (new ScreenMenu())))
            this.Animation.Add(s)
        
    override this.Draw() =
        let (x, y) = Rect.center this.Bounds
        Font.drawJust(Font.defaultFont, (if closing then "Bye o/" else "Loading :)"), 80.f, x, y, Color.White, 0.5f)

// Toolbar

type Toolbar() as this =
    inherit Widget()

    static let height = 60.f

    let barSlider = new AnimationFade(0.0f)
    let notifSlider = new AnimationFade(0.0f)
    
    do
        this.Animation.Add(barSlider)
        this.Animation.Add(notifSlider)
        let tb = new Components.TextBox(K version, 25.f, Color.White)
        tb.Reposition(-200.f, 1.f, 0.f, 1.f, 0.f, 1.f, height, 1.f)
        this.Add(tb)

    override this.Draw() = 
        let struct (l, t, r, b) = this.Bounds
        Draw.rect(Rect.create l (t - height) r t) Color.Gray Sprite.Default
        Draw.rect(Rect.create l b r (b + height)) Color.Gray Sprite.Default
        base.Draw()

    override this.Update(elapsed, bounds) =
        if Interlude.Options.Options.options.Hotkeys.Screenshot.Get().Tapped(false) then
            barSlider.SetTarget(1.0f - barSlider.Target)
        if Interlude.Options.Options.options.Hotkeys.Exit.Get().Tapped(true) then
            Screens.popScreen()
        base.Update(elapsed, Rect.expand (0.f, -height * barSlider.Value) bounds)
        //

//Screen manager

type ScreenContainer() as this =
    inherit Widget()

    let mutable dialogs = []
    let mutable previous = None
    let mutable current = new ScreenLoading() :> Screen
    let mutable screens = [current]
    let mutable exit = false

    do
        Screens.addScreen <- this.AddScreen
        Screens.popScreen <- this.RemoveScreen
        current.OnEnter(current)
        this.Add(new Toolbar())

    member this.Exit = exit

    member this.AddScreen(s: Screen) =
        screens <- s :: screens
        current.OnExit(s)
        s.OnEnter(current)
        previous <- Some current
        current <- s

    member this.RemoveScreen() =
        previous <- Some current
        screens <- List.tail screens
        match List.tryHead screens with
        | None -> exit <- true
        | Some s ->
            current.OnExit(s)
            s.OnEnter(current)
            current <- s

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        current.Update(elapsedTime, bounds)

    override this.Draw() =
        current.Draw()
        base.Draw()