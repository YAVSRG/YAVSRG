namespace Prelude.Scripts

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Drawing.Processing
open Prelude

module Images =

    let test() =
        let bmp : Bitmap = "C:\Users\percy\AppData\Local\osu!\Skins\- YUGEN -\mania-hit50@2x.png" |> Bitmap.Load

        let test_method(name: string, cmode: PixelColorBlendingMode, mode: PixelAlphaCompositionMode) =
            let clone : Bitmap = bmp.Clone()

            let color = SixLabors.ImageSharp.Color.FromRgb(0uy, 255uy, 160uy)
            clone.Mutate(fun img -> 
                img.Fill(
                    GraphicsOptions(
                        ColorBlendingMode = cmode,
                        AlphaCompositionMode = mode
                    ), color
                ) |> ignore
            )
            clone.Mutate(fun img -> img |> ignore)
            clone.SaveAsPng(sprintf "C:\Users\percy\Desktop\%s.png" name)

        test_method("Multiply-SrcIn", PixelColorBlendingMode.Multiply, PixelAlphaCompositionMode.SrcIn)
        test_method("Multiply-SrcAtop", PixelColorBlendingMode.Multiply, PixelAlphaCompositionMode.SrcAtop)
        test_method("Multiply-SrcOut", PixelColorBlendingMode.Multiply, PixelAlphaCompositionMode.SrcOut)
        test_method("Multiply-SrcOver", PixelColorBlendingMode.Multiply, PixelAlphaCompositionMode.SrcOver)
