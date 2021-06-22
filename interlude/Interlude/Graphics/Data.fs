namespace Interlude.Graphics

open System
open System.Drawing
open OpenTK.Mathematics
open OpenTK.Graphics.OpenGL

(*
    Storage of rectangles as left, top, right, bottom
    Preferred over left, top, width, height for a handful of reasons
*)

type Rect = (struct(float32 * float32 * float32 * float32))

module Rect = 

    let create l t r b : Rect = struct(l, t, r, b)
    let createWH l t w h : Rect = struct(l, t, l + w, t + h)

    let width (struct (left, _, right, _): Rect) = right - left
    let height (struct (_, top, _, bottom): Rect) = bottom - top

    let centerX (struct (left, _, right, _): Rect) = (right + left) * 0.5f
    let centerY (struct (_, top, _, bottom): Rect) = (bottom + top) * 0.5f
    let center (r: Rect) = (centerX r, centerY r)
    let centerV (r: Rect) = new Vector2(centerX r, centerY r)

    let intersect (struct (l, t, r, b): Rect) (struct (left, top, right, bottom): Rect) : Rect =
        struct (Math.Max(l, left), Math.Max(t, top), Math.Min(r, right), Math.Min(b, bottom))

    let translate (x,y) (struct (left, top, right, bottom): Rect) : Rect =
        struct (left + x, top + y, right + x, bottom + y)

    let expand (x,y) (struct (left, top, right, bottom): Rect) : Rect =
        struct (left - x, top - y, right + x, bottom + y)

    let sliceLeft v (struct (left, top, _, bottom): Rect) : Rect =
        struct (left, top, left + v, bottom)

    let sliceTop v (struct (left, top, right, _): Rect) : Rect =
        struct (left, top, right, top + v)

    let sliceRight v (struct (_, top, right, bottom): Rect) : Rect =
        struct (right - v, top, right, bottom)

    let sliceBottom v (struct (left, _, right, bottom): Rect) : Rect =
        struct (left, bottom - v, right, bottom)

    let trimLeft v (struct (left, top, right, bottom): Rect) : Rect =
        struct (left + v, top, right, bottom)

    let trimTop v (struct (left, top, right, bottom): Rect) : Rect =
        struct (left, top + v, right, bottom)

    let trimRight v (struct (left, top, right, bottom): Rect) : Rect =
        struct (left, top, right - v, bottom)

    let trimBottom v (struct (left, top, right, bottom): Rect) : Rect =
        struct (left, top, right, bottom - v)

    let zero = create 0.f 0.f 0.f 0.f
    let one = create 0.f 0.f 1.f 1.f

module RenderHelper =
    let mutable drawing = false
    let exit() = if drawing then GL.End()
    let enter() = if drawing then GL.Begin(PrimitiveType.Quads)

(*
    Simple storage of vertices to render as a quad
*)

type Quad = (struct(Vector2 * Vector2 * Vector2 * Vector2))
type QuadColors = (struct(Color * Color * Color * Color))

module Quad =

    let ofRect (struct (l, t, r, b): Rect) : Quad =
        struct (new Vector2(l, t), new Vector2(r, t), new Vector2(r, b), new Vector2(l, b))

    let parallelogram (amount: float32) (struct (l, t, r, b): Rect): Quad =
        let a = (b - t) * 0.5f * amount
        struct (new Vector2(l + a, t), new Vector2(r + a, t), new Vector2(r - a, b), new Vector2(l - a, b))

    let create c1 c2 c3 c4 : Quad = struct (c1, c2, c3, c4)

    let colorOf c: QuadColors = struct (c, c, c, c)

    let flip (struct (c1, c2, c3, c4): Quad) : Quad = struct (c4, c3, c2, c1)

    let rotate r (struct (c1, c2, c3, c4): Quad) : Quad =
        match r with
        | 3 -> struct (c4, c1, c2, c3)
        | 2 -> struct (c3, c4, c1, c2)
        | 1 -> struct (c2, c3, c4, c1)
        | 0 | _ -> struct (c1, c2, c3, c4)

    let map f (struct (c1, c2, c3, c4): Quad) : Quad = struct (f c1, f c2, f c3, f c4)

(*
    Sprites and content uploading
*)

[<Struct>]
type Sprite = { ID: int; Width: int; Height: int; Rows: int; Columns: int }
with
    member this.WithUV(q: Quad) : SpriteQuad = struct (this, q)
    static member Default = { ID = 0; Width = 1; Height = 1; Rows = 1; Columns = 1 }
    static member DefaultQuad : SpriteQuad = struct (Sprite.Default, Quad.ofRect Rect.one)
and SpriteQuad = (struct(Sprite * Quad))

module Sprite =

    open System.Drawing.Imaging

    let upload (bitmap: Bitmap, rows, columns, smooth) =
        RenderHelper.exit()
        let id = GL.GenTexture()
        let data = bitmap.LockBits(new Rectangle(0, 0, bitmap.Width, bitmap.Height), ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb)
        GL.BindTexture(TextureTarget.Texture2D, id)
        GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, data.Width, data.Height, 0, OpenTK.Graphics.OpenGL.PixelFormat.Bgra, PixelType.UnsignedByte, data.Scan0)
        bitmap.UnlockBits(data)

        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, (int)TextureWrapMode.Repeat)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, (int)TextureWrapMode.Repeat)
        if smooth then
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Linear)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Linear)
        else
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Nearest)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Nearest)
        RenderHelper.enter()
        { ID = id; Width = bitmap.Width; Height = bitmap.Height; Rows = rows; Columns = columns }

    let destroy (sprite: Sprite) =
        RenderHelper.exit()
        GL.DeleteTexture(sprite.ID)
        RenderHelper.enter()

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