namespace Percyqaz.Flux.Graphics

open SixLabors.ImageSharp
open OpenTK.Mathematics
open OpenTK.Graphics.OpenGL
open Percyqaz.Common

(*
    Sprites and content uploading
*)

type Sprite =
    { 
        ID: int; TextureUnit: int
        Width: int; Height: int
        Rows: int; Columns: int
    }
with member this.WithUV(q: Quad) : SpriteQuad = struct (this, q)
and SpriteQuad = (struct(Sprite * Quad))

module Sprite =

    let MAX_TEXTURE_UNITS = GL.GetInteger GetPName.MaxTextureImageUnits
    let TOTAL_TEXTURE_UNITS = GL.GetInteger GetPName.MaxCombinedTextureImageUnits
    let MAX_TEXTURE_SIZE = GL.GetInteger GetPName.MaxTextureSize

    // texture unit 0 is reserved for binding uncached sprites
    let texUnit_cache : int array = Array.zeroCreate MAX_TEXTURE_UNITS
    let texUnit_inUse : bool array = Array.zeroCreate MAX_TEXTURE_UNITS

    let upload (image: Image<PixelFormats.Rgba32>, rows, columns, smooth) : Sprite =
        let id = GL.GenTexture()

        let width = image.Width
        let height = image.Height

        let mutable data = System.Span<PixelFormats.Rgba32>.Empty
        let success = image.TryGetSinglePixelSpan(&data)
        if not success then Logging.Critical "Couldn't get pixel span for image!"

        GL.BindTexture(TextureTarget.Texture2D, id)
        GL.TexImage2D<PixelFormats.Rgba32>(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, width, height, 0, PixelFormat.Rgba, PixelType.UnsignedByte, data.ToArray())

        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
        if smooth then
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        else
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        { ID = id; TextureUnit = 0; Width = width; Height = height; Rows = rows; Columns = columns }

    let cache (source: string) (low_priority: bool) (sprite: Sprite) : Sprite =
        if low_priority && MAX_TEXTURE_UNITS = 16 then sprite else

        seq { 1 .. (MAX_TEXTURE_UNITS - 1) }
        |> Seq.tryFind (fun i -> not texUnit_inUse.[i])
        |> function
            | None -> Logging.Debug (sprintf "Can't cache '%s', all texture units are full" source); sprite
            | Some i ->
                texUnit_cache.[i] <- sprite.ID
                texUnit_inUse.[i] <- true

                GL.ActiveTexture(int TextureUnit.Texture0 + i |> enum)
                GL.BindTexture(TextureTarget.Texture2D, sprite.ID)
                GL.ActiveTexture(TextureUnit.Texture0)

                //Logging.Debug(sprintf "Cached sprite (%s) with ID %i to index %i" source sprite.ID i)
                { sprite with TextureUnit = i }

    let Default =
        use img = new Image<PixelFormats.Rgba32>(1, 1)
        img.[0, 0] <- new PixelFormats.Rgba32(255uy, 255uy, 255uy, 255uy)
        upload (img, 1, 1, false)
        |> cache "BLANK" false

    let DefaultQuad : SpriteQuad = struct (Default, Quad.ofRect Rect.ONE)

    let destroy (sprite: Sprite) =
        if sprite.ID <> Default.ID then
            texUnit_inUse.[sprite.TextureUnit] <- false
            GL.DeleteTexture sprite.ID

    let gridUV (x, y) (sprite: Sprite) =
        let x = float32 x
        let y = float32 y
        let sx = 1.0f / float32 sprite.Columns
        let sy = 1.0f / float32 sprite.Rows
        Rect.Create( x * sx, y * sy, (x + 1.f) * sx, (y + 1.f) * sy )
        |> Quad.ofRect
        |> sprite.WithUV

    let tilingUV (scale, left, top) (sprite: Sprite) (quad: Quad) =
        let width = float32 sprite.Width * scale
        let height = float32 sprite.Height * scale
        Quad.map (fun v -> new Vector2((v.X - left) / width, (v.Y - top) / height)) quad

    let alignedBoxX(xOrigin, yOrigin, xOffset, yOffset, xScale, yMult) (sprite: Sprite) : Rect =
        let width = xScale
        let height = float32 sprite.Height / float32 sprite.Width * width * yMult
        let left = xOrigin - xOffset * width
        let top = yOrigin - yOffset * height
        Rect.Box(left, top, width, height)