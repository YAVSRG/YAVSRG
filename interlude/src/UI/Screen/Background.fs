namespace Interlude.UI

open System
open SixLabors.ImageSharp.Processing
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content

module Background =

    let private parallaxX = Animation.Fade 0.0f
    let private parallaxY = Animation.Fade 0.0f
    let private parallaxZ = Animation.Fade 40.0f
    let internal dim_percent = Animation.Fade 1.0f

    let dim (amount: float32) : unit = dim_percent.Target <- amount

    let set_parallax_pos (x: float32, y: float32) : unit =
        parallaxX.Target <- x
        parallaxY.Target <- y

    let set_parallax_amount (amount: float32) : unit = parallaxZ.Target <- amount

    let mutable private background: (Sprite * Animation.Fade * bool) list = []

    let private loader =
        { new Async.CancelQueue<string option, (Bitmap * Color) option>() with
            member this.Process(file: string option) =
                async {
                    match file with
                    | None -> return None
                    | Some file ->

                        try
                            use stream = IO.File.OpenRead file

                            match Bitmap.from_stream true stream with
                            | None -> return (failwith "Unsupported or invalid image format")
                            | Some bmp ->

                            let col =
                                if Content.ThemeConfig.AlwaysUseDefaultAccentColor then
                                    Content.ThemeConfig.DefaultAccentColor
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
                                            Content.ThemeConfig.DefaultAccentColor

                            let true_screen_width = fst (Render.framebuffer_size())

                            if bmp.Width * 3 / 4 > true_screen_width && true_screen_width > 0 then
                                bmp.Mutate(fun img -> img.Resize(true_screen_width, 0) |> ignore)
                            let new_bmp = new Bitmap(bmp.Width, bmp.Height, SixLabors.ImageSharp.PixelFormats.Rgba32(0uy, 0uy, 0uy, 255uy))
                            new_bmp.Mutate(fun img -> img.DrawImage(bmp, 1.0f) |> ignore)

                            bmp.Dispose()

                            return Some(new_bmp, col)
                        with err ->
                            Logging.Warn "Failed to load background image '%s': %O" file err
                            return None
                }

            member this.Handle(res) =
                match res with
                | Some(bmp, col) ->
                    let sprite = Sprite.upload_one true true (SpriteUpload.OfImage("BACKGROUND", bmp))

                    bmp.Dispose()
                    Palette.accent_color.Target <- col
                    background <- (sprite, Animation.Fade(0.0f, Target = 1.0f), false) :: background
                | None ->
                    background <-
                        (Content.Texture "background", Animation.Fade(0.0f, Target = 1.0f), true)
                        :: background

                    Palette.accent_color.Target <- Content.ThemeConfig.DefaultAccentColor
        }

    let get_current () : Sprite option =
        match List.tryHead background with
        | Some (sprite, _, false) -> Some sprite
        | _ -> None

    let load (path: string option) : unit =
        List.iter (fun (_, fade: Animation.Fade, _) -> fade.Target <- 0.0f) background
        loader.Request(if Content.ThemeConfig.AlwaysUseDefaultBackground then None else path)

    let update (elapsed_ms: float) : unit =

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
                            Sprite.destroy sprite |> ignore

                        false
                    else
                        true
                )
                background

    let drawq (q: Quad, color: Color, depth: float32) : unit =
        Render.quad q Color.Black
        List.iter
            (fun (bg: Sprite, (fade: Animation.Fade), is_default) ->
                let color = color.O4a fade.Alpha
                let pwidth = Render.width() + parallaxZ.Value * depth
                let pheight = Render.height() + parallaxZ.Value * depth
                let x = -parallaxX.Value * parallaxZ.Value * depth
                let y = -parallaxY.Value * parallaxZ.Value * depth
                let screenaspect = pwidth / pheight
                let bgaspect = float32 bg.Width / float32 bg.Height

                Render.tex_quad
                    q
                    color.AsQuad
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

    let draw (bounds: Rect, color: Color, depth: float32) : unit = drawq (bounds.AsQuad, color, depth)

    let draw_with_dim (bounds: Rect, color: Color, depth: float32) : unit =
        draw (bounds, color, depth)
        Render.rect bounds (Color.Black.O4a dim_percent.Alpha)