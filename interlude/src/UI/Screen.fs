namespace Interlude.UI

open System
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open OpenTK.Mathematics

module Screen =

    let private TRANSITIONTIME = 500.0

    type TransitionFlags =
        | Default = 0
        | UnderLogo = 1
    
    type Type =
        | SplashScreen = 0
        | MainMenu = 1
        | Import = 2
        | LevelSelect = 3
        | Play = 4
        | Replay = 5
        | Score = 6
        
    [<AbstractClass>]
    type T() =
        inherit Widget1()
        abstract member OnEnter: Type -> unit
        abstract member OnExit: Type -> unit

    let private screenTransition = Animation.Sequence()
    let private transitionIn = Animation.Delay TRANSITIONTIME
    let private transitionOut = Animation.Delay TRANSITIONTIME
    
    module Toolbar =
        let HEIGHT = 70.0f
        let expandAmount = Animation.Fade 1.0f
        let mutable hidden = false

        let hide() = hidden <- true
        let show() = hidden <- false

    let globalAnimation = Animation.fork [Style.accentColor; Toolbar.expandAmount]

    let logo = Logo.display
    
    let mutable private current = Unchecked.defaultof<T>
    let private screens : T array = Array.zeroCreate 4
    let init (_screens: T array) =
        for i = 0 to 3 do screens.[i] <- _screens.[i]
        current <- screens.[0]
    let mutable exit = false
    let mutable currentType = Type.SplashScreen
    
    let mutable transitionFlags = TransitionFlags.Default

    let changeNew (thunk: unit -> T) (screenType: Type) (flags: TransitionFlags) =
        if screenTransition.Complete && (screenType <> currentType || screenType = Type.Play) then
            transitionFlags <- flags
            globalAnimation.Add screenTransition
            screenTransition.Add transitionIn
            screenTransition.Add(
                Animation.Action(
                    fun () ->
                        let s = thunk()
                        current.OnExit screenType
                        s.OnEnter currentType
                        match currentType with
                        | Type.Play | Type.Replay | Type.Score -> current.Dispose()
                        | _ -> ()
                        currentType <- screenType
                        current <- s
                        transitionOut.FrameSkip() //ignore frame lag spike when initialising screen
                    ))
            screenTransition.Add transitionOut
            screenTransition.Add (Animation.Action(fun () -> transitionIn.Reset(); transitionOut.Reset()))

    let change (screenType: Type) (flags: TransitionFlags) = changeNew (K screens.[int screenType]) screenType flags

    let back (flags: TransitionFlags) =
        match currentType with
        | Type.SplashScreen -> exit <- true
        | Type.MainMenu -> change Type.SplashScreen flags
        | Type.LevelSelect -> change Type.MainMenu flags
        | Type.Import
        | Type.Play
        | Type.Replay
        | Type.Score -> change Type.LevelSelect flags
        | _ -> Logging.Critical (sprintf "No back-behaviour defined for %A" currentType)

    module private Transitions =
    
        let private wedge (centre: Vector2) (r1: float32) (r2: float32) (a1: float) (a2: float) (col: Color) =
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
    
        let private cwedge = wedge <| new Vector2(Viewport.vwidth * 0.5f, Viewport.vheight * 0.5f)
    
        let private bubble (x, y) (r1: float32) (r2: float32) (col: Color) (lo: float32) (hi: float32) (amount: float32) =
            let pos = Math.Clamp((amount - lo) / (hi - lo), 0.0f, 1.0f) |> float
            let head = float32(Math.Pow(pos, 0.5)) * (r2 - r1) + r1
            let tail = float32(Math.Pow(pos, 2.0)) * (r2 - r1) + r1
            wedge (new Vector2(x, y)) tail head 0.0 (Math.PI * 2.0) col
    
        let private wedgeAnim (r1: float32) (r2: float32) (col: Color) (lo: float32) (hi: float32) (amount: float32) =
            let pos = Math.Clamp((amount - lo) / (hi - lo), 0.0f, 1.0f) |> float
            let head = Math.Pow(pos, 0.5) * Math.PI * 2.0
            let tail = Math.Pow(pos, 2.0) * Math.PI * 2.0
            cwedge r1 r2 tail head col
    
        let private wedgeAnim2 (r1: float32) (r2: float32) (col: Color) (lo: float32) (hi: float32) (amount: float32) =
            let pos = (amount - lo) / (hi - lo)
            let head = float (Math.Clamp(pos * 2.0f, 0.0f, 1.0f)) * 2.0 * Math.PI
            let tail = float (Math.Clamp(pos * 2.0f - 1.0f, 0.0f, 1.0f)) * 2.0 * Math.PI
            cwedge r1 r2 tail head col
    
        let private fancyTransition inbound amount bounds =
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
    
        let private diamondWipe inbound amount bounds =
            let s = 150.0f
            let size x =
                let f = Math.Clamp(((if inbound then amount else 1.0f - amount) - (x - 2.0f * s) / Viewport.vwidth) / (4.0f * s / Viewport.vwidth), 0.0f, 1.0f)
                if inbound then f * s * 0.5f else (1.0f - f) * s * 0.5f
            let diamond x y =
                let r = size x
                Draw.quad(Quad.create <| new Vector2(x - r, y) <| new Vector2(x, y - r) <| new Vector2(x + r, y) <| new Vector2(x, y + r)) (Quad.colorOf Color.Transparent) Sprite.DefaultQuad
                
            Stencil.create false
            for x in 0 .. (Viewport.vwidth / s |> float |> Math.Ceiling |> int) do
                for y in 0 .. (Viewport.vheight / s |> float |> Math.Ceiling |> int) do
                    diamond (s * float32 x) (s * float32 y)
                    diamond (0.5f * s + s * float32 x) (0.5f * s + s * float32 y)
            Stencil.draw()
            Background.draw (bounds, Style.color (255.0f * amount |> int, 1.0f, 0.0f), 1.0f)
            Stencil.finish()
    
        let draw flags inbound amount bounds =
            diamondWipe inbound amount bounds

    type Container(toolbar: Widget) =
        inherit Overlay(NodeType.None)
    
        do current.OnEnter Type.SplashScreen
    
        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            Background.update elapsedTime
            if currentType <> Type.Play || Dialog.exists() then Tooltip.display.Update (elapsedTime, moved)
            if Viewport.vwidth > 0.0f then
                let x, y = Mouse.pos()
                Background.setParallaxPos(x / Viewport.vwidth, y / Viewport.vheight)
            Style.accentColor.SetColor Content.accentColor
            Dialog.display.Update(elapsedTime, moved)

            globalAnimation.Update elapsedTime
            toolbar.Update (elapsedTime, moved)
            logo.Update (elapsedTime, this.Bounds)
            let screenBounds = if Toolbar.hidden then this.Bounds else this.Bounds.Shrink(0.0f, Toolbar.HEIGHT * Toolbar.expandAmount.Value)
            current.Update (elapsedTime, screenBounds)
    
        override this.Draw() =
            Background.drawWithDim (this.Bounds, Color.White, 1.0f)
            current.Draw()
            logo.Draw()
            toolbar.Draw()
            if not screenTransition.Complete then
                let inbound = transitionIn.Elapsed < TRANSITIONTIME
                let amount = Math.Clamp((if inbound then transitionIn.Elapsed / TRANSITIONTIME else 1.0 - (transitionOut.Elapsed / TRANSITIONTIME)), 0.0, 1.0) |> float32
                Transitions.draw transitionFlags inbound amount this.Bounds
                if (transitionFlags &&& TransitionFlags.UnderLogo = TransitionFlags.UnderLogo) then logo.Draw()
            Dialog.display.Draw()
            if currentType <> Type.Play || Dialog.exists() then 
                let x, y = Mouse.pos()
                Draw.sprite (Rect.Box(x, y, Content.themeConfig().CursorSize, Content.themeConfig().CursorSize)) (Style.color(255, 1.0f, 0.5f)) (Content.getTexture "cursor")
                Tooltip.display.Draw()

        override this.Init(parent: Widget) =
            base.Init parent
            toolbar.Init this
            Tooltip.display.Init this
            Dialog.display.Init this