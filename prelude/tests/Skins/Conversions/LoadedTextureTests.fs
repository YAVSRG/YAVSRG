namespace Prelude.Tests.Skins.Conversions

open NUnit.Framework
open Prelude
open Prelude.Skins.Conversions.Osu

module LoadedTextureTests =

    let ONEPIXELIMAGE = new Bitmap(1, 1)

    [<Test>]
    let As2x_Upscales1xTexture () =
        let texture : LoadedTexture = { Image = ONEPIXELIMAGE; Is2x = false }
        
        let scaled = texture.As2x
        Assert.AreEqual(2, scaled.Width)
        Assert.AreEqual(2, scaled.Height)
        
    [<Test>]
    let As2x_Preserves2xTexture () =
        let texture : LoadedTexture = { Image = ONEPIXELIMAGE; Is2x = true }
        
        let scaled = texture.As2x
        Assert.AreEqual(1, scaled.Width)
        Assert.AreEqual(1, scaled.Height)
        Assert.AreEqual(ONEPIXELIMAGE, scaled)