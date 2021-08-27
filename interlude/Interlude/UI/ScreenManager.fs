namespace Interlude.UI

open System
open System.Drawing
open OpenTK.Mathematics
open Prelude.Common
open Interlude
open Interlude.Graphics
open Interlude.UI.Animation
open Interlude.UI.Components
open Interlude.UI.Selection
open Interlude.UI.Screens
open Interlude.UI.OptionsMenu
open Interlude.Utils
open Interlude.Input

// Screen manager

module ScreenTransitions =
    let TRANSITIONTIME = 500.0

    let wedge (centre: Vector2) (r1: float32) (r2: float32) (a1: float) (a2: float) (col: Color) =
        let segments = int ((a2 - a1) / 0.10)
        let segsize = (a2 - a1) / float segments
        for i = 1 to segments do
            let a2 = a1 + float i * segsize
            let a1 = a1 + float (i - 1) * segsize
            let ang1 = Vector2(Math.Sin a1 |> float32, -Math.Cos a1 |> float32)
            let ang2 = Vector2(Math.Sin a2 |> float32, -Math.Cos a2 |> float32)
            Draw.quad
                (Quad.create (centre + ang1 * r2) (centre + ang2 * r2) (centre + ang2 * r1) (centre + ang1 * r1))
                (Quad.colorOf col)
                Sprite.DefaultQuad

    let cwedge = wedge <| new Vector2(Render.vwidth * 0.5f, Render.vheight * 0.5f)

    let bubble (x, y) (r1: float32) (r2: float32) (col: Color) (lo: float32) (hi: float32) (amount: float32) =
        let pos = Math.Clamp((amount - lo) / (hi - lo), 0.0f, 1.0f) |> float
        let head = float32(Math.Pow(pos, 0.5)) * (r2 - r1) + r1
        let tail = float32(Math.Pow(pos, 2.0)) * (r2 - r1) + r1
        wedge (new Vector2(x, y)) tail head 0.0 (Math.PI * 2.0) col

    let wedgeAnim (r1: float32) (r2: float32) (col: Color) (lo: float32) (hi: float32) (amount: float32) =
        let pos = Math.Clamp((amount - lo) / (hi - lo), 0.0f, 1.0f) |> float
        let head = Math.Pow(pos, 0.5) * Math.PI * 2.0
        let tail = Math.Pow(pos, 2.0) * Math.PI * 2.0
        cwedge r1 r2 tail head col

    let wedgeAnim2 (r1: float32) (r2: float32) (col: Color) (lo: float32) (hi: float32) (amount: float32) =
        let pos = (amount - lo) / (hi - lo)
        let head = float (Math.Clamp(pos * 2.0f, 0.0f, 1.0f)) * 2.0 * Math.PI
        let tail = float (Math.Clamp(pos * 2.0f - 1.0f, 0.0f, 1.0f)) * 2.0 * Math.PI
        cwedge r1 r2 tail head col

    let fancyTransition inbound amount bounds =
        let amount = if inbound then amount else 2.0f - amount
        let a = int (255.0f * (1.0f - Math.Abs(amount - 1.0f)))
        wedgeAnim2 0.0f 1111.0f (Color.FromArgb(a, 0, 160, 255)) 0.0f 1.8f amount
        wedgeAnim2 0.0f 1111.0f (Color.FromArgb(a, 0, 200, 255)) 0.1f 1.9f amount
        wedgeAnim2 0.0f 1111.0f (Color.FromArgb(a, 0, 240, 255)) 0.2f 2.0f amount
        wedgeAnim 300.0f 500.0f Color.White 0.5f 1.5f amount
        bubble (400.0f, 200.0f) 100.0f 150.0f Color.White 1.2f 1.5f amount
        bubble (300.0f, 250.0f) 60.0f 90.0f Color.White 1.3f 1.6f amount
        bubble (1600.0f, 600.0f) 80.0f 120.0f Color.White 1.0f 1.3f amount
        bubble (1400.0f, 700.0f) 50.0f 75.0f Color.White 1.4f 1.7f amount

    let diamondWipe inbound amount bounds =
        let s = 150.0f
        let size x =
            let f = Math.Clamp(((if inbound then amount else 1.0f - amount) - (x - 2.0f * s) / Render.vwidth) / (4.0f * s / Render.vwidth), 0.0f, 1.0f)
            if inbound then f * s * 0.5f else (1.0f - f) * s * 0.5f
        let diamond x y =
            let r = size x
            Draw.quad(Quad.create <| new Vector2(x - r, y) <| new Vector2(x, y - r) <| new Vector2(x + r, y) <| new Vector2(x, y + r)) (Quad.colorOf Color.Transparent) Sprite.DefaultQuad
            
        Stencil.create false
        for x in 0 .. (Render.vwidth / s |> float |> Math.Ceiling |> int) do
            for y in 0 .. (Render.vheight / s |> float |> Math.Ceiling |> int) do
                diamond (s * float32 x) (s * float32 y)
                diamond (0.5f * s + s * float32 x) (0.5f * s + s * float32 y)
        Stencil.draw()
        Globals.drawBackground (bounds, Globals.accentShade (255.0f * amount |> int, 1.0f, 0.0f), 1.0f)
        Stencil.finish()

    let drawTransition flags inbound amount bounds =
        diamondWipe inbound amount bounds
        //fancyTransition inbound amount bounds

type ScreenContainer() as this =
    inherit Widget()

    let dialogs = new ResizeArray<Dialog>()
    let mutable current = new LoadingScreen() :> IScreen
    let screens = [|
        current;
        new MainMenu() :> IScreen;
        new ImportMenu.Screen() :> IScreen;
        new LevelSelect.Screen() :> IScreen;
        |]
    let mutable exit = false
    let mutable cursor = true

    let mutable transitionFlags = ScreenTransitionFlag.Default
    let screenTransition = new AnimationSequence()
    let t1 = AnimationTimer ScreenTransitions.TRANSITIONTIME
    let t2 = AnimationTimer ScreenTransitions.TRANSITIONTIME

    let toolbar = new Toolbar()
    let tooltip = new TooltipHandler()

    do
        Globals.changeScreen <- this.ChangeScreen //second overload
        Globals.newScreen <- this.ChangeScreen //first overload
        Globals.back <- this.Back
        Globals.addDialog <- dialogs.Add
        Globals.setCursorVisible <- (fun b -> cursor <- b)
        this.Add toolbar
        Globals.logo
        |> positionWidget(-300.0f, 0.5f, 1000.0f, 0.5f, 300.0f, 0.5f, 1600.0f, 0.5f)
        |> this.Add
        this.Animation.Add screenTransition
        this.Animation.Add Globals.globalAnimation
        current.OnEnter ScreenType.SplashScreen

    member this.Exit = exit

    member this.ChangeScreen (s: unit -> IScreen, screenType, flags) =
        if screenTransition.Complete && screenType <> Globals.currentType then
            transitionFlags <- flags
            this.Animation.Add screenTransition
            screenTransition.Add t1
            screenTransition.Add(
                new AnimationAction(
                    fun () ->
                        let s = s()
                        current.OnExit screenType
                        s.OnEnter Globals.currentType
                        match Globals.currentType with
                        | ScreenType.Play | ScreenType.Score -> current.Dispose()
                        | _ -> ()
                        Globals.currentType <- screenType
                        current <- s
                        t2.FrameSkip() //ignore frame lag spike when initialising screen
                    ))
            screenTransition.Add t2
            screenTransition.Add (new AnimationAction(fun () -> t1.Reset(); t2.Reset()))
    member this.ChangeScreen (screenType, flags) = this.ChangeScreen(K screens.[int screenType], screenType, flags)

    member this.Back flags =
        match Globals.currentType with
        | ScreenType.SplashScreen -> exit <- true
        | ScreenType.MainMenu -> this.ChangeScreen (ScreenType.SplashScreen, flags)
        | ScreenType.LevelSelect -> this.ChangeScreen (ScreenType.MainMenu, flags)
        | ScreenType.Import
        | ScreenType.Play
        | ScreenType.Score -> this.ChangeScreen (ScreenType.LevelSelect, flags)
        | _ -> ()

    override this.Update(elapsedTime, bounds) =
        Globals.updateBackground elapsedTime
        tooltip.Update(elapsedTime, bounds)
        if Render.vwidth > 0.0f then
            Globals.parallaxX.Target <- Mouse.X() / Render.vwidth
            Globals.parallaxY.Target <- Mouse.Y() / Render.vheight
        Globals.accentColor.SetColor Themes.accentColor
        if dialogs.Count > 0 then
            dialogs.[dialogs.Count - 1].Update(elapsedTime, bounds)
            if not dialogs.[dialogs.Count - 1].Enabled then
                dialogs.[dialogs.Count - 1].Dispose()
                dialogs.RemoveAt(dialogs.Count - 1)
            Input.absorbAll()
        base.Update(elapsedTime, bounds)
        current.Update(elapsedTime, toolbar.Bounds)

    override this.Draw() =
        Globals.drawBackground (this.Bounds, Color.White, 1.0f)
        Draw.rect this.Bounds (Color.FromArgb (Globals.backgroundDim.Value * 255.0f |> int, 0, 0, 0)) Sprite.Default
        current.Draw()
        base.Draw()
        if not screenTransition.Complete then
            let inbound = t1.Elapsed < ScreenTransitions.TRANSITIONTIME
            let amount = Math.Clamp((if inbound then t1.Elapsed / ScreenTransitions.TRANSITIONTIME else 1.0 - (t2.Elapsed / ScreenTransitions.TRANSITIONTIME)), 0.0, 1.0) |> float32
            ScreenTransitions.drawTransition transitionFlags inbound amount this.Bounds
            if (transitionFlags &&& ScreenTransitionFlag.UnderLogo = ScreenTransitionFlag.UnderLogo) then Globals.logo.Draw()
        for d in dialogs do d.Draw()
        if cursor then Draw.rect(Rect.createWH (Mouse.X()) (Mouse.Y()) Themes.themeConfig.CursorSize Themes.themeConfig.CursorSize) (Globals.accentShade(255, 1.0f, 0.5f)) (Themes.getTexture "cursor")
        tooltip.Draw()
