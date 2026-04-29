namespace Prelude.Skins.Conversions.Osu

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open Prelude

module ImageOperations =
    // All image operations return a new Bitmap for simplicity

    let pad (width: int, height: int) (image: Bitmap) : Bitmap =
        assert (image.Width <= width)

        if image.Width <> width || image.Height <> height then
            let new_image = new Bitmap(width, height)
            new_image.Mutate(fun img -> img.DrawImage(image, Point((-image.Width + width) / 2, (-image.Height + height) / 2), 1.0f) |> ignore)

            new_image
        else
            image.Clone()

    let pad_to_square (target_width: int) (image: Bitmap) : Bitmap = pad (target_width, target_width) image

    let upscale_to_square (target_width: int) (image: Bitmap) : Bitmap =
        assert(image.Width <= target_width)
        assert(image.Height <= target_width)
        let new_image = image.Clone()
        if new_image.Height > new_image.Width then
            new_image.Mutate(fun img -> img.Resize(0, target_width) |> ignore)
        else
            new_image.Mutate(fun img -> img.Resize(target_width, 0) |> ignore)
        pad_to_square target_width new_image

    let stretch_to_square (target_width: int) (image: Bitmap) : Bitmap =
        assert (image.Width = target_width)
        let new_image = image.Clone()
        new_image.Mutate(fun img -> img.Resize(target_width, target_width) |> ignore)
        new_image

    let scale_y (scale: float32) (image: Bitmap) : Bitmap =
        let new_image = image.Clone()
        new_image.Mutate(fun img -> img.Resize(new_image.Width, float32 new_image.Height * scale |> round |> int) |> ignore)
        new_image

    let rotate (rotate_mode: RotateMode) (image: Bitmap) : Bitmap =
        let image = pad_to_square image.Width image
        image.Mutate(fun img -> img.Rotate(rotate_mode) |> ignore)
        image

    let grayscale (brightness: float32) (image: Bitmap) : Bitmap =
        let new_image = image.Clone()
        new_image.Mutate(fun img -> img.Grayscale().Brightness(brightness) |> ignore)
        new_image

    type PixelFormats.Rgba32 with
        member this.BlackToTransparent =
            let new_alpha =
                min
                    this.A
                    (max this.R this.G |> max this.B)
            PixelFormats.Rgba32(this.R, this.G, this.B, new_alpha)

    let remove_black_bg (image: Bitmap) : Bitmap =
        let new_image = new Bitmap(image.Width, image.Height)
        for x = 0 to image.Width - 1 do
            for y = 0 to image.Height - 1 do
                new_image.[x, y] <- image.[x, y].BlackToTransparent
        new_image