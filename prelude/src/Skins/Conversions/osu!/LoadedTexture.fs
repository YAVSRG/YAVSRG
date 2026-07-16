namespace Prelude.Skins.Conversions.Osu

open SixLabors.ImageSharp.Processing
open Prelude

type LoadedTexture =
    {
        Image: Bitmap
        Is2x: bool
    }
    // todo: wtf? why does As2x double the size when already 2x and nothing when not
    // todo: find out what is going on and fix before merging
    member this.As2x : Bitmap =
        if this.Is2x then
            let new_image = this.Image.Clone()
            new_image.Mutate(fun img -> img.Resize(this.Image.Width * 2, 0) |> ignore)
            new_image
        else this.Image
        
    static member TransparentFallback() : LoadedTexture =
        { Image = new Bitmap(64, 64); Is2x = true }