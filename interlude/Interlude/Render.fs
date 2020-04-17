namespace Interlude.Render

open OpenTK
open OpenTK.Graphics.OpenGL
open System.Collections.Generic
open System.Drawing.Text

(*
    Sprites and content uploading
*)

[<Struct>]
type Sprite = { ID:int; Width:int; Height:int; Rows:int; Columns:int }
with static member Default = { ID=0; Width=1; Height=1; Rows=1; Columns=1 }

module Sprite =

    open System.Drawing
    open System.Drawing.Imaging
    
    let upload(bitmap : Bitmap, rows, columns, smooth) =
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

        { ID=id; Width=bitmap.Width; Height=bitmap.Height; Rows=rows; Columns=columns }

    let destroy (sprite: Sprite) = GL.DeleteTexture(sprite.ID)

(*
    Storage of rectangles as left, top, right, bottom
    Preferred over left, top, width, height for a handful of reasons
*)

type Rect = (struct(float32 * float32 * float32 * float32))

module Rect = 

    let create l t r b : Rect = l, t, r, b
    
    let width ((left, _, right, _): Rect) = right - left
    let height ((_, top, _, bottom): Rect) = bottom - top

    let centerX ((left, _, right, _): Rect) = (right + left) * 0.5f
    let centerY ((_, top, _, bottom): Rect) = (bottom + top) * 0.5f
    let center (r: Rect) = (centerX r, centerY r)
    let centerV (r: Rect) = new Vector2(centerX r, centerY r)

    let expand (x,y) ((left, top, right, bottom): Rect) =
        struct (left - x, top - y, right + x, bottom + y)

    let sliceLeft v ((left, top, _, bottom): Rect) =
        struct (left, top, left + v, bottom)

    let sliceTop v ((left, top, right, _): Rect) =
        struct (left, top, right, top + v)

    let sliceRight v ((_, top, right, bottom): Rect) =
        struct (right - v, top, right, bottom)

    let sliceBottom v ((left, _, right, bottom): Rect) =
        struct (left, bottom - v, right, bottom)

(*
    Simple storage of vertices to render as a quad
*)

type Quad = (struct(Vector2 * Vector2 * Vector2 * Vector2))

module Quad =
    
    let ofRect ((l, t, r, b) : Rect) : Quad =
        (new Vector2(l, t), new Vector2(r, t), new Vector2(r, b), new Vector2(l, b))

    let create c1 c2 c3 c4 : Quad = c1, c2, c3, c4

(*
    Font
*)

type SpriteFont(font: Font) =
    let fontLookup = new Dictionary<string, Sprite>()
    let genChar(c: char) =
        let size =
            use b = new Bitmap(1, 1)
            use g = System.Drawing.Graphics.FromImage(b)
            g.MeasureString(c.ToString(), font)
        let bmp = new Bitmap(int size.Width, int size.Height)
        use g = System.Drawing.Graphics.FromImage(bmp)
        g.TextRenderingHint <- TextRenderingHint.AntiAliasGridFit
        g.DrawString(c.ToString(), font, Brushes.White, 0, 0)
        fontLookup.Add(c.ToString, Sprite.upload(bmp, 1, 1, true))

(*
    Render handling to be used from Game
*)

module Render =

    let resize(width, height) =
        GL.MatrixMode(MatrixMode.Projection)
        GL.LoadIdentity()
        GL.Ortho(0.0, width, height, 0.0, 0.0, 1.0)

    let start() = 
        GL.ClearColor(Color.Black)

    let finish() =
        GL.Finish()
        GL.Flush()

    let init(width, height) =
        GL.Enable(EnableCap.Blend)
        GL.Enable(EnableCap.Texture2D)
        GL.Arb.BlendFuncSeparate(0, BlendingFactor.SrcAlpha, BlendingFactor.OneMinusSrcAlpha, BlendingFactor.One, BlendingFactor.OneMinusSrcAlpha)
        GL.ClearStencil(0x00)
        resize(width, height)
        
        //font management here maybe

        //fbo storage here too

(*
    Drawing methods to be used by UI components
*)

module Draw =

    let rect (r: Rect) (c: Color) =
        GL.Begin(PrimitiveType.Quads)
        let struct (p1, p2, p3, p4) = Quad.ofRect r
        GL.Color4(c)
        GL.Vertex2(p1)
        GL.Vertex2(p2)
        GL.Vertex2(p3)
        GL.Vertex2(p4)
        GL.End()
