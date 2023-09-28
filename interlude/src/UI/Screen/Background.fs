namespace Interlude.UI

open System
open SixLabors.ImageSharp
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude

module Background =

    let private parallaxX = Animation.Fade 0.0f
    let private parallaxY = Animation.Fade 0.0f
    let private parallaxZ = Animation.Fade 40.0f
    let private dimPercent = Animation.Fade 1.0f

    let dim(amount: float32) = dimPercent.Target <- amount
    let setParallaxPos(x: float32, y: float32) = parallaxX.Target <- x; parallaxY.Target <- y
    let setParallaxAmount(amount: float32) = parallaxZ.Target <- amount

    let mutable private background: (Sprite * Animation.Fade * bool) list = []

    let load =
        let worker = 
            { new Async.SwitchService<string option, Bitmap option>() with
                member this.Handle(file: string option) =
                    async {
                        match file with 
                        | None -> return None 
                        | Some file ->

                        try
                            let! img = Image.LoadAsync file |> Async.AwaitTask
                            return Some img
                        with err -> 
                            Logging.Warn("Failed to load background image: " + file, err)
                            return None
                    }
            }
        let mutable last_id = -1
        fun (path: string option) ->
            List.iter (fun (_, fade: Animation.Fade, _) -> fade.Target <- 0.0f) background
            last_id <- worker.Request(path,
                function
                | id, Some bmp ->
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
                    sync(fun () ->
                        if id = last_id then
                            let sprite = Sprite.upload(bmp, 1, 1, true) |> Sprite.cache "loaded background"
                            bmp.Dispose()
                            Content.accentColor <- col
                            background <- (sprite, Animation.Fade(0.0f, Target = 1.0f), false) :: background
                    )
                | id, None ->
                    sync(fun () ->
                        if id = last_id then
                            background <- (Content.getTexture "background", Animation.Fade(0.0f, Target = 1.0f), true) :: background
                            Content.accentColor <- Content.themeConfig().DefaultAccentColor
                    )
            )

    let update elapsedTime =

        parallaxX.Update(elapsedTime)
        parallaxY.Update(elapsedTime)
        parallaxZ.Update(elapsedTime)
        dimPercent.Update(elapsedTime)

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
            (fun (bg, (fade: Animation.Fade), isDefault) ->
                let color = Color.FromArgb(fade.Alpha, color)
                let pwidth = Viewport.vwidth + parallaxZ.Value * depth
                let pheight = Viewport.vheight + parallaxZ.Value * depth
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

    let drawWithDim (bounds: Rect, color, depth) = 
        draw(bounds, color, depth)
        Draw.rect bounds (Color.FromArgb(dimPercent.Alpha, 0, 0, 0))