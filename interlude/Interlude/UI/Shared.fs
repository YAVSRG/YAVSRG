namespace Interlude.UI

open System
open System.Drawing
open OpenTK.Mathematics
open Prelude.Common
open Interlude.Graphics
open Interlude
open Interlude.UI.Animation

type Logo() as this =
    inherit Widget()

    let counter = AnimationCounter(10000000.0)
    do this.Animation.Add(counter)

    override this.Draw() =
        base.Draw()
        let w = Rect.width this.Bounds
        let struct (l, t, r, b) = this.Bounds

        if (r > 0.0f) then

            Draw.quad
                (Quad.create(new Vector2(l + 0.08f * w, t + 0.09f * w)) (new Vector2(l + 0.5f * w, t + 0.76875f * w)) (new Vector2(l + 0.5f * w, t + 0.76875f * w)) (new Vector2(r - 0.08f * w, t + 0.09f * w)))
                (Quad.colorOf(Color.DarkBlue))
                Sprite.DefaultQuad
            Draw.quad
                (Quad.create(new Vector2(l + 0.08f * w, t + 0.29f * w)) (new Vector2(l + 0.22f * w, t + 0.29f * w)) (new Vector2(l + 0.5f * w, t + 0.76875f * w)) (new Vector2(l + 0.5f * w, t + 0.96875f * w)))
                (Quad.colorOf(Color.DarkBlue))
                Sprite.DefaultQuad
            Draw.quad
                (Quad.create(new Vector2(r - 0.08f * w, t + 0.29f * w)) (new Vector2(r - 0.22f * w, t + 0.29f * w)) (new Vector2(l + 0.5f * w, t + 0.76875f * w)) (new Vector2(l + 0.5f * w, t + 0.96875f * w)))
                (Quad.colorOf(Color.DarkBlue))
                Sprite.DefaultQuad

            Stencil.create(true)
            Draw.quad
                (Quad.create(new Vector2(l + 0.1f * w, t + 0.1f * w)) (new Vector2(l + 0.5f * w, t + 0.75f * w)) (new Vector2(l + 0.5f * w, t + 0.75f * w)) (new Vector2(r - 0.1f * w, t + 0.1f * w)))
                (Quad.colorOf(Color.Aqua))
                Sprite.DefaultQuad
            Draw.quad
                (Quad.create(new Vector2(l + 0.1f * w, t + 0.3f * w)) (new Vector2(l + 0.2f * w, t + 0.3f * w)) (new Vector2(l + 0.5f * w, t + 0.7875f * w)) (new Vector2(l + 0.5f * w, t + 0.95f * w)))
                (Quad.colorOf(Color.Aqua))
                Sprite.DefaultQuad
            Draw.quad
                (Quad.create(new Vector2(r - 0.1f * w, t + 0.3f * w)) (new Vector2(r - 0.2f * w, t + 0.3f * w)) (new Vector2(l + 0.5f * w, t + 0.7875f * w)) (new Vector2(l + 0.5f * w, t + 0.95f * w)))
                (Quad.colorOf(Color.Aqua))
                Sprite.DefaultQuad
            Draw.rect this.Bounds Color.White (Themes.getTexture "logo")

            Stencil.draw()
            //chart background
            Draw.rect this.Bounds Color.Aqua Sprite.Default
            let rain = Themes.getTexture "rain"
            let v = float32 counter.Time
            let q = Quad.ofRect this.Bounds
            Draw.quad <| q <| Quad.colorOf (Color.FromArgb(80, 0, 0, 255))  <| rain.WithUV(Sprite.tilingUV(0.625f, v * 0.06f, v * 0.07f) rain q)
            Draw.quad <| q <| Quad.colorOf (Color.FromArgb(150, 0, 0, 255)) <| rain.WithUV(Sprite.tilingUV(1.0f, v * 0.1f, v * 0.11f) rain q)
            Draw.quad <| q <| Quad.colorOf (Color.FromArgb(220, 0, 0, 255)) <| rain.WithUV(Sprite.tilingUV(1.5625f, v * 0.15f, v * 0.16f) rain q)

            let mutable prev = 0.0f
            let m = b - w * 0.5f
            for i in 0 .. 31 do
                let level =
                    (seq { (i * 8) .. (i * 8 + 7) }
                    |> Seq.map (fun x -> Audio.waveForm.[x])
                    |> Seq.sum) * 0.1f
                let i = float32 i
                Draw.quad
                    (Quad.create(new Vector2(l + i * w / 32.0f, m - prev)) (new Vector2(l + (i + 1.0f) * w / 32.0f, m - level)) (new Vector2(l + (i + 1.0f) * w / 32.0f, b)) (new Vector2(l + i * w / 32.0f, b)))
                    (Quad.colorOf(Color.FromArgb(127, 0, 0, 255)))
                    Sprite.DefaultQuad
                prev <- level

            Stencil.finish()
            Draw.rect this.Bounds Color.White (Themes.getTexture "logo")


[<AbstractClass>]
type Dialog() as this =
    inherit Widget()

    let fade = new Animation.AnimationFade 0.0f

    do
        this.Animation.Add(fade)
        fade.Target <- 1.0f

    // Begins closing animation
    abstract member BeginClose : unit -> unit
    default this.BeginClose() =
        fade.Target <- 0.0f

    // Called when dialog actually closes (end of animation)
    abstract member OnClose: unit -> unit

    override this.Draw() =
        Draw.rect this.Bounds (Color.FromArgb(int (200.0f * fade.Value), 0, 0, 0)) Sprite.Default
        base.Draw()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if (fade.Value < 0.02f && fade.Target = 0.0f) then
            this.Enabled <- false
            this.OnClose()

type ScreenTransitionFlag =
    | Default = 0
    | UnderLogo = 1
    //more transition animations go here

type NotificationType =
    | Info = 0
    | System = 1
    | Task = 2
    | Error = 3

type ScreenType =
    | SplashScreen = 0
    | MainMenu = 1
    | Import = 2
    | LevelSelect = 3
    | Play = 4
    | Score = 5

[<AbstractClass>]
type IScreen() =
    inherit Widget()
    abstract member OnEnter: ScreenType -> unit
    abstract member OnExit: ScreenType -> unit

(*
    Collection of mutable values to "tie the knot" in mutual dependence
       - Stuff is defined but not inialised here
       - Stuff is then referenced by screen logic
       - Overall screen manager references screen logic AND initialises values, connecting the loop
*)

module Globals =
    open Themes
    
    let mutable currentType = ScreenType.SplashScreen

    // All of these are initialised in ScreenManager.fs
    let mutable internal changeScreen: ScreenType * ScreenTransitionFlag -> unit = ignore
    let mutable internal newScreen: (unit -> IScreen) * ScreenType * ScreenTransitionFlag -> unit = ignore
    let mutable internal back: ScreenTransitionFlag -> unit = ignore
    let mutable internal addDialog: Dialog -> unit = ignore

    let mutable internal setToolbarCollapsed: bool -> unit = ignore
    let mutable internal setCursorVisible: bool -> unit = ignore

    let mutable internal addNotification: string * NotificationType -> unit = ignore
    let mutable internal addTooltip: Input.Bind * string * float * (unit -> unit) -> unit = ignore
    let mutable internal watchReplay: Prelude.Scoring.ReplayData -> unit = ignore

    let parallaxX = AnimationFade 0.0f
    let parallaxY = AnimationFade 0.0f
    let parallaxZ = AnimationFade 40.0f
    let backgroundDim = AnimationFade 1.0f
    let accentColor = AnimationColorMixer Themes.accentColor

    let globalAnimation = Animation.Fork(parallaxX, parallaxY, parallaxZ, backgroundDim, accentColor)

    let logo = new Logo()

    let mutable background: (Sprite * AnimationFade * bool) list = []
    let loadBackground =
        let future = 
            BackgroundTask.future<Bitmap option> "Background Loader"
                (fun sprite ->
                    match sprite with
                    | Some bmp ->
                        let col =
                            if themeConfig.OverrideAccentColor then themeConfig.DefaultAccentColor else
                                let vibrance (c: Color) = Math.Abs(int c.R - int c.B) + Math.Abs(int c.B - int c.G) + Math.Abs(int c.G - int c.R)
                                seq {
                                    let w = bmp.Width / 50
                                    let h = bmp.Height / 50
                                    for x in 0 .. 49 do
                                        for y in 0 .. 49 do
                                            yield bmp.GetPixel(w * x, h * x) }
                                |> Seq.maxBy vibrance
                                |> fun c -> if vibrance c > 127 then Color.FromArgb(255, c) else themeConfig.DefaultAccentColor
                        globalAnimation.Add(
                            AnimationAction(fun () ->
                                let sprite = Sprite.upload(bmp, 1, 1, true)
                                bmp.Dispose()
                                Themes.accentColor <- col
                                background <- (sprite, AnimationFade(0.0f, Target = 1.0f), false) :: background
                            )
                        )
                    | None ->
                        globalAnimation.Add(
                            AnimationAction(fun () ->
                                background <- (Themes.getTexture "background", AnimationFade(0.0f, Target = 1.0f), true) :: background
                                Themes.accentColor <- themeConfig.DefaultAccentColor
                            )
                        )
                )
        let bitmapLoader (file: string) =
            fun () -> 
                match System.IO.Path.GetExtension(file).ToLower() with
                | ".png" | ".bmp" | ".jpg" | ".jpeg" ->
                    try Some (new Bitmap(file))
                    with err -> Logging.Warn("Failed to load background image: " + file, err); None
                | ext -> None
        fun path ->
            List.iter (fun (_, fade: AnimationFade, _) -> fade.Target <- 0.0f) background
            future (bitmapLoader path)

    let updateBackground elapsedTime =
        background <-
        List.filter
            (fun (sprite, fade, isDefault) ->
                fade.Update elapsedTime |> ignore
                if fade.Target = 0.0f && fade.Value < 0.01f then
                    if not isDefault then Sprite.destroy sprite
                    false
                else true)
            background

    let drawBackground (bounds, color, depth) =
        List.iter
            (fun (bg, (fade: AnimationFade), isDefault) ->
                let color = Color.FromArgb(fade.Value * 255.0f |> int, color)
                let pwidth = Render.vwidth + parallaxZ.Value * depth
                let pheight = Render.vheight + parallaxZ.Value * depth
                let x = -parallaxX.Value * parallaxZ.Value * depth
                let y = -parallaxY.Value * parallaxZ.Value * depth
                let screenaspect = pwidth / pheight
                let bgaspect = float32 bg.Width / float32 bg.Height
                let q = Quad.ofRect bounds
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
    
    let accentShade (alpha, brightness, white) =
        let accentColor = accentColor.GetColor()
        let rd = float32 (255uy - accentColor.R) * white
        let gd = float32 (255uy - accentColor.G) * white
        let bd = float32 (255uy - accentColor.B) * white
        Color.FromArgb(alpha,
            int ((float32 accentColor.R + rd) * brightness),
            int ((float32 accentColor.G + gd) * brightness),
            int ((float32 accentColor.B + bd) * brightness))