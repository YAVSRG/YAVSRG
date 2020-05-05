namespace Interlude.UI

open OpenTK
open Interlude.Render
open Interlude.UI.Animation

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

type ScreenMenu() =
    inherit Screen()

    override this.Draw() =
        let (x, y) = Rect.center this.Bounds
        Font.drawJust(Font.defaultFont, "menu screen", 50.f, x, y, Color.White, 0.5f)

type ScreenLoading() =
    inherit Screen()
    override this.OnEnter(prev: Screen) =
        match prev with
        | :? ScreenMenu ->
            let s = AnimationSequence()
            s.Add(AnimationTimer(2000.0))
            s.Add(AnimationAction(fun () -> Screens.popScreen()))
            this.Animation.Add(s)
        | _ -> 
            let s = AnimationSequence()
            s.Add(AnimationTimer(2000.0))
            s.Add(AnimationAction(fun () -> Screens.addScreen (new ScreenMenu())))
            this.Animation.Add(s)
        
    override this.Draw() =
        let (x, y) = Rect.center this.Bounds
        Font.drawJust(Font.defaultFont, "Loading :)", 80.f, x, y, Color.White, 0.5f)

type Toolbar() as this =
    inherit Widget()

    let barSlider = new AnimationFade(0.0f)
    let notifSlider = new AnimationFade(0.0f)
    
    do
        this.Animation.Add(AnimationFade(0.0f))
        

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
        current.Update(elapsedTime, bounds)

    override this.Draw() =
        current.Draw()