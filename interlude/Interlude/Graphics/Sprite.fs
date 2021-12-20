namespace Interlude.Graphics

open System.Drawing
open OpenTK.Mathematics
open OpenTK.Graphics.OpenGL

(*
    Sprites and content uploading
*)

type Sprite = { ID: int; Width: int; Height: int; Rows: int; Columns: int }
with member this.WithUV(q: Quad) : SpriteQuad = struct (this, q)
and SpriteQuad = (struct(Sprite * Quad))

module Sprite =

    open System.Drawing.Imaging

    let MAX_TEXTURE_UNITS = GL.GetInteger GetPName.MaxTextureImageUnits
    let TOTAL_TEXTURE_UNITS = GL.GetInteger GetPName.MaxCombinedTextureImageUnits

    let upload (bitmap: Bitmap, rows, columns, smooth) =
        let id = GL.GenTexture()
        let data = bitmap.LockBits(new Rectangle(0, 0, bitmap.Width, bitmap.Height), ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb)
        GL.BindTexture(TextureTarget.Texture2D, id)
        GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, data.Width, data.Height, 0, PixelFormat.Bgra, PixelType.UnsignedByte, data.Scan0)
        bitmap.UnlockBits(data)

        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, (int)TextureWrapMode.Repeat)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, (int)TextureWrapMode.Repeat)
        if smooth then
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Linear)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Linear)
        else
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Nearest)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Nearest)
        { ID = id; Width = bitmap.Width; Height = bitmap.Height; Rows = rows; Columns = columns }

    let Default =
        use bmp = new Bitmap(1, 1);
        bmp.SetPixel(0, 0, Color.White)
        upload (bmp, 1, 1, false)

    let DefaultQuad : SpriteQuad = struct (Default, Quad.ofRect Rect.one)

    let destroy (sprite: Sprite) =
        GL.DeleteTexture sprite.ID

    let gridUV (x, y) (sprite: Sprite) =
        let x = float32 x
        let y = float32 y
        let sx = 1.f / float32 sprite.Columns
        let sy = 1.f / float32 sprite.Rows
        Rect.create <| x * sx <| y * sy <| (x + 1.f) * sx <| (y + 1.f) * sy
        |> Quad.ofRect
        |> sprite.WithUV

    let tilingUV(scale, left, top)(sprite)(quad) =
        let width = float32 sprite.Width * scale
        let height = float32 sprite.Height * scale
        quad |>
        Quad.map (fun v -> new Vector2((v.X - left) / width, (v.Y - top) / height))

    let alignedBoxX(xOrigin, yOrigin, xOffset, yOffset, xScale, yMult) (sprite: Sprite): Rect =
        let width = xScale
        let height = float32 sprite.Height / float32 sprite.Width * width * yMult
        let left = xOrigin - xOffset * width
        let top = yOrigin - yOffset * height
        Rect.create left top (left + width) (top + height)