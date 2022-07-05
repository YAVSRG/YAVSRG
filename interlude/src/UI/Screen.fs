namespace Interlude.UI

open System
open SixLabors.ImageSharp
open Percyqaz.Common
open Prelude.Common
open Interlude
open Interlude.Utils
open Interlude.Graphics
open Interlude.Input
open Interlude.UI.Animation
open Interlude.UI.Components
open OpenTK.Mathematics

module Screen =

    let private TRANSITIONTIME = 500.0

    type TransitionFlag =
        | Default = 0
        | UnderLogo = 1
        //more transition animations go here
    
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
        inherit Widget()
        abstract member OnEnter: Type -> unit
        abstract member OnExit: Type -> unit

    let private parallaxX = AnimationFade 0.0f
    let private parallaxY = AnimationFade 0.0f
    let private parallaxZ = AnimationFade 40.0f

    let private screenTransition = new AnimationSequence()
    let private transitionIn = AnimationTimer TRANSITIONTIME
    let private transitionOut = AnimationTimer TRANSITIONTIME

    let backgroundDim = AnimationFade 1.0f

    let globalAnimation = Animation.Fork(parallaxX, parallaxY, parallaxZ, backgroundDim, Style.accentColor)

    let logo = Logo.display
    
    let mutable private current = Unchecked.defaultof<T>
    let private screens : T array = Array.zeroCreate 4
    let init (_screens: T array) =
        for i = 0 to 3 do screens.[i] <- _screens.[i]
        current <- screens.[0]
    let mutable exit = false
    let mutable hideToolbar = false
    let mutable currentType = Type.SplashScreen
    
    let mutable transitionFlags = TransitionFlag.Default

    let changeNew (thunk: unit -> T) (screenType: Type) (flags: TransitionFlag) =
        if screenTransition.Complete && (screenType <> currentType || screenType = Type.Play) then
            transitionFlags <- flags
            globalAnimation.Add screenTransition
            screenTransition.Add transitionIn
            screenTransition.Add(
                new AnimationAction(
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
            screenTransition.Add (new AnimationAction(fun () -> transitionIn.Reset(); transitionOut.Reset()))

    let change (screenType: Type) (flags: TransitionFlag) = changeNew (K screens.[int screenType]) screenType flags

    let back (flags: TransitionFlag) =
        match currentType with
        | Type.SplashScreen -> exit <- true
        | Type.MainMenu -> change Type.SplashScreen flags
        | Type.LevelSelect -> change Type.MainMenu flags
        | Type.Import
        | Type.Play
        | Type.Replay
        | Type.Score -> change Type.LevelSelect flags
        | _ -> Logging.Critical (sprintf "No back-behaviour defined for %A" currentType)

    module Background =

        let mutable private background: (Sprite * AnimationFade * bool) list = []

        let load =
            let worker = 
                { new Async.SingletonWorker<string, Bitmap option>() with
                    member this.Handle(file: string) =
                        match System.IO.Path.GetExtension(file).ToLower() with
                        | ".png" | ".bmp" | ".jpg" | ".jpeg" ->
                            try Some (Image.Load file)
                            with err -> Logging.Warn("Failed to load background image: " + file, err); None
                        | ext -> None
                    member this.Callback(sprite: Bitmap option) =
                        match sprite with
                        | Some bmp ->
                            let col =
                                if Content.themeConfig().OverrideAccentColor then Content.themeConfig().DefaultAccentColor else
                                    let vibrance (c: Color) = Math.Abs(int c.R - int c.B) + Math.Abs(int c.B - int c.G) + Math.Abs(int c.G - int c.R)
                                    seq {
                                        let w = bmp.Width / 50
                                        let h = bmp.Height / 50
                                        for x = 0 to 49 do
                                            for y = 0 to 49 do
                                                yield Color.FromArgb(int bmp.[w * x, h * x].R, int bmp.[w * x, h * x].G, int bmp.[w * x, h * x].B) }
                                    |> Seq.maxBy vibrance
                                    |> fun c -> if vibrance c > 127 then Color.FromArgb(255, c) else Content.themeConfig().DefaultAccentColor
                            globalAnimation.Add(
                                AnimationAction( fun () ->
                                    let sprite = Sprite.upload(bmp, 1, 1, true) |> Sprite.cache "loaded background"
                                    bmp.Dispose()
                                    Content.accentColor <- col
                                    background <- (sprite, AnimationFade(0.0f, Target = 1.0f), false) :: background
                                )
                            )
                        | None ->
                            globalAnimation.Add(
                                AnimationAction(fun () ->
                                    background <- (Content.getTexture "background", AnimationFade(0.0f, Target = 1.0f), true) :: background
                                    Content.accentColor <- Content.themeConfig().DefaultAccentColor
                                )
                            )
                }
            fun (path: string) ->
                List.iter (fun (_, fade: AnimationFade, _) -> fade.Target <- 0.0f) background
                worker.Request path

        let update elapsedTime =
            background <-
            List.filter
                (fun (sprite, fade, isDefault) ->
                    fade.Update elapsedTime |> ignore
                    if fade.Target = 0.0f && fade.Value < 0.01f then
                        if not isDefault then Sprite.destroy sprite
                        false
                    else true)
                background

        let drawq (q: Quad, color: Color, depth: float32) =
            List.iter
                (fun (bg, (fade: AnimationFade), isDefault) ->
                    let color = Color.FromArgb(fade.Value * 255.0f |> int, color)
                    let pwidth = Render.vwidth + parallaxZ.Value * depth
                    let pheight = Render.vheight + parallaxZ.Value * depth
                    let x = -parallaxX.Value * parallaxZ.Value * depth
                    let y = -parallaxY.Value * parallaxZ.Value * depth
                    let screenaspect = pwidth / pheight
                    let bgaspect = float32 bg.Width / float32 bg.Height
                    Draw.quad q (Quad.colorOf color)
                        (bg.WithUV(
                            Sprite.tilingUV(
                                if bgaspect > screenaspect then
                                    let scale = pheight / float32 bg.Height
                                    let left = (float32 bg.Width * scale - pwidth) * -0.5f
                                    (scale, left + x, 0.0f + y)
                                else
                                    let scale = pwidth / float32 bg.Width
                                    let top = (float32 bg.Height * scale - pheight) * -0.5f
                                    (scale, 0.0f + x, top + y)
                                ) bg q))
                )
                background

        let draw (bounds: Rect, color, depth) = drawq (Quad.ofRect bounds, color, depth)

    module Transitions =
    
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
    
        let private cwedge = wedge <| new Vector2(Render.vwidth * 0.5f, Render.vheight * 0.5f)
    
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
            Background.draw (bounds, Style.accentShade (255.0f * amount |> int, 1.0f, 0.0f), 1.0f)
            Stencil.finish()
    
        let drawTransition flags inbound amount bounds =
            diamondWipe inbound amount bounds
            //fancyTransition inbound amount bounds

    type Container(toolbar: Widget) as this =
        inherit Widget()
    
        do
            this.Add logo
            this.Add toolbar
            this.Animation.Add screenTransition
            this.Animation.Add globalAnimation
            current.OnEnter Type.SplashScreen
    
        override this.Update(elapsedTime, bounds) =
            Background.update elapsedTime
            if currentType <> Type.Play || Dialog.any() then Tooltip.display.Update(elapsedTime, bounds)
            if Render.vwidth > 0.0f then
                parallaxX.Target <- Mouse.X() / Render.vwidth
                parallaxY.Target <- Mouse.Y() / Render.vheight
            Style.accentColor.SetColor Content.accentColor
            Dialog.update(elapsedTime, bounds)
            base.Update(elapsedTime, bounds)
            current.Update(elapsedTime, toolbar.Bounds)
    
        override this.Draw() =
            Background.draw (this.Bounds, Color.White, 1.0f)
            Draw.rect this.Bounds (Color.FromArgb (backgroundDim.Value * 255.0f |> int, 0, 0, 0)) Sprite.Default
            current.Draw()
            base.Draw()
            if not screenTransition.Complete then
                let inbound = transitionIn.Elapsed < TRANSITIONTIME
                let amount = Math.Clamp((if inbound then transitionIn.Elapsed / TRANSITIONTIME else 1.0 - (transitionOut.Elapsed / TRANSITIONTIME)), 0.0, 1.0) |> float32
                Transitions.drawTransition transitionFlags inbound amount this.Bounds
                if (transitionFlags &&& TransitionFlag.UnderLogo = TransitionFlag.UnderLogo) then logo.Draw()
            Dialog.draw()
            if currentType <> Type.Play || Dialog.any() then 
                Draw.rect (Rect.Box(Mouse.X(), Mouse.Y(), Content.themeConfig().CursorSize, Content.themeConfig().CursorSize)) (Style.accentShade(255, 1.0f, 0.5f)) (Content.getTexture "cursor")
                Tooltip.display.Draw()