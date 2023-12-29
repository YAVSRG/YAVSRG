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
    let private dim_percent = Animation.Fade 1.0f

    let dim (amount: float32) = dim_percent.Target <- amount

    let set_parallax_pos (x: float32, y: float32) =
        parallaxX.Target <- x
        parallaxY.Target <- y

    let set_parallax_amount (amount: float32) = parallaxZ.Target <- amount

    let mutable private background: (Sprite * Animation.Fade * bool) list = []

    let private loader =
        { new Async.SwitchService<string option, (Bitmap * Color) option>() with
            member this.Process(file: string option) =
                async {
                    match file with
                    | None -> return None
                    | Some file ->

                        try
                            let! (bmp: Bitmap) = Image.LoadAsync file |> Async.AwaitTask

                            let col =
                                if Content.theme_config().OverrideAccentColor then
                                    Content.theme_config().DefaultAccentColor
                                else
                                    let vibrance (c: Color) =
                                        Math.Abs(int c.R - int c.B)
                                        + Math.Abs(int c.B - int c.G)
                                        + Math.Abs(int c.G - int c.R)

                                    seq {
                                        let w = bmp.Width / 50
                                        let h = bmp.Height / 50

                                        for x = 0 to 49 do
                                            for y = 0 to 49 do
                                                yield
                                                    Color.FromArgb(
                                                        int bmp.[w * x, h * x].R,
                                                        int bmp.[w * x, h * x].G,
                                                        int bmp.[w * x, h * x].B
                                                    )
                                    }
                                    |> Seq.maxBy vibrance
                                    |> fun c ->
                                        if vibrance c > 127 then
                                            Color.FromArgb(255, c)
                                        else
                                            Content.theme_config().DefaultAccentColor

                            return Some(bmp, col)
                        with err ->
                            Logging.Warn("Failed to load background image: " + file, err)
                            return None
                }

            member this.Handle(res) =
                match res with
                | Some(bmp, col) ->
                    let sprite =
                        Sprite.upload_one false true (SpriteUpload.OfImage("BACKGROUND", bmp))

                    bmp.Dispose()
                    Content.accent_color <- col
                    background <- (sprite, Animation.Fade(0.0f, Target = 1.0f), false) :: background
                | None ->
                    background <-
                        (Content.get_texture "background", Animation.Fade(0.0f, Target = 1.0f), true)
                        :: background

                    Content.accent_color <- Content.theme_config().DefaultAccentColor
        }

    let load (path: string option) =
        List.iter (fun (_, fade: Animation.Fade, _) -> fade.Target <- 0.0f) background
        loader.Request(path)

    let update elapsed_ms =

        loader.Join()

        parallaxX.Update elapsed_ms
        parallaxY.Update elapsed_ms
        parallaxZ.Update elapsed_ms
        dim_percent.Update elapsed_ms

        background <-
            List.filter
                (fun (sprite, fade, is_default) ->
                    fade.Update elapsed_ms |> ignore

                    if fade.Target = 0.0f && fade.Value < 0.01f then
                        if not is_default then
                            Sprite.destroy sprite

                        false
                    else
                        true
                )
                background

    let drawq (q: Quad, color: Color, depth: float32) =
        List.iter
            (fun (bg: Sprite, (fade: Animation.Fade), is_default) ->
                let color = Color.FromArgb(fade.Alpha, color)
                let pwidth = Viewport.vwidth + parallaxZ.Value * depth
                let pheight = Viewport.vheight + parallaxZ.Value * depth
                let x = -parallaxX.Value * parallaxZ.Value * depth
                let y = -parallaxY.Value * parallaxZ.Value * depth
                let screenaspect = pwidth / pheight
                let bgaspect = float32 bg.Width / float32 bg.Height

                Draw.quad
                    q
                    (Quad.color color)
                    (Sprite.tiling
                        (if bgaspect > screenaspect then
                             let scale = pheight / float32 bg.Height
                             let left = (float32 bg.Width * scale - pwidth) * -0.5f
                             (scale, left + x, 0.0f + y)
                         else
                             let scale = pwidth / float32 bg.Width
                             let top = (float32 bg.Height * scale - pheight) * -0.5f
                             (scale, 0.0f + x, top + y))
                        bg
                        q)
            )
            background

    let draw (bounds: Rect, color, depth) =
        drawq (bounds.AsQuad, color, depth)

    let draw_with_dim (bounds: Rect, color, depth) =
        draw (bounds, color, depth)
        Draw.rect bounds (Color.Black.O4a dim_percent.Alpha)
