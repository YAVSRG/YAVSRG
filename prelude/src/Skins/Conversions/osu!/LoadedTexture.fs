namespace Prelude.Skins.Conversions.Osu

open SixLabors.ImageSharp.Processing
open Prelude

type LoadedTexture =
    {
        Image: Bitmap
        Is2x: bool
    }
    member this.As2x : Bitmap =
        if not(this.Is2x) then
            let new_image = this.Image.Clone()
            new_image.Mutate(fun img -> img.Resize(this.Image.Width * 2, 0) |> ignore)
            new_image
        else this.Image
        
    static member TransparentFallback() : LoadedTexture =
        { Image = new Bitmap(64, 64); Is2x = true }