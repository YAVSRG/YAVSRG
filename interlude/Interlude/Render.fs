namespace Interlude.Render

open OpenTK
open OpenTK.Graphics.OpenGL
open System.Collections.Generic

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

    let zero = create 0.f 0.f 0.f 0.f

(*
    Simple storage of vertices to render as a quad
*)

type Quad = (struct(Vector2 * Vector2 * Vector2 * Vector2))
type QuadColors = (struct(Color * Color * Color * Color))

module Quad =
    
    let ofRect ((l, t, r, b) : Rect) : Quad =
        (new Vector2(l, t), new Vector2(r, t), new Vector2(r, b), new Vector2(l, b))

    let create c1 c2 c3 c4 : Quad = c1, c2, c3, c4

    let colorOf c : QuadColors = c, c, c, c

(*
    Sprites and content uploading
*)

[<Struct>]
type Sprite = { ID:int; Width:int; Height:int; Rows:int; Columns:int }
with
    member this.WithUV(q: Quad): SpriteQuad = (this, q)
    static member Default = { ID=0; Width=1; Height=1; Rows=1; Columns=1 }
and SpriteQuad = (struct(Sprite * Quad))

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

    let uv(x, y) (sprite: Sprite) =
        let x = float32 x
        let y = float32 y
        let sx = 1.f / float32 sprite.Columns
        let sy = 1.f / float32 sprite.Rows
        Rect.create <| x * sx <| y * sy <| (x + 1.f) * sx <| (y + 1.f) * sy
        |> Quad.ofRect
        |> sprite.WithUV

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

    let quad ((p1, p2, p3, p4): Quad) ((c1, c2, c3, c4): QuadColors) ((s,(u1,u2,u3,u4)): SpriteQuad) =
        GL.BindTexture(TextureTarget.Texture2D, s.ID)
        GL.Begin(PrimitiveType.Quads)
        GL.Color4(c1); GL.TexCoord2(u1); GL.Vertex2(p1)
        GL.Color4(c2); GL.TexCoord2(u2); GL.Vertex2(p2)
        GL.Color4(c3); GL.TexCoord2(u3); GL.Vertex2(p3)
        GL.Color4(c4); GL.TexCoord2(u4); GL.Vertex2(p4)
        GL.End()
        
    let rect (r: Rect) (c: Color) (s: Sprite) = quad <| Quad.ofRect r <| Quad.colorOf c <| Sprite.uv(0, 0) s

(*
    Font rendering
*)

module Font =
    
    open System.Drawing
    open System.Drawing.Text

    let fontscale = 100.f
    let spacing = 0.25f
    let shadow = 0.1f

    type SpriteFont(font: Font) =
        let fontLookup = new Dictionary<char, Sprite>()
        let genChar(c: char) =
            let size =
                use b = new Bitmap(1, 1)
                use g = System.Drawing.Graphics.FromImage(b)
                g.MeasureString(c.ToString(), font)
            let bmp = new Bitmap(int size.Width, int size.Height)
            let _ =
                use g = System.Drawing.Graphics.FromImage(bmp)
                g.TextRenderingHint <- TextRenderingHint.AntiAliasGridFit
                g.DrawString(c.ToString(), font, Brushes.White, 0.f, 0.f)
            fontLookup.Add(c, Sprite.upload(bmp, 1, 1, true))
        do
            "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890!£$%^&*()-=_+[]{};:'@#~,.<>/?¬`\\|\"\r\n"
            |> Seq.iter (genChar)
        member this.Char(c) =
            if not <| fontLookup.ContainsKey(c) then genChar(c)
            fontLookup.[c]

    let measure(font: SpriteFont, text: string): float32 =
        text |> Seq.fold (fun v c -> v + (c |> function | ' ' -> spacing | c -> -0.5f + float32 (font.Char(c).Width) / fontscale)) 0.5f

    let draw(font: SpriteFont, text, scale, x, y, color) =
        let mutable x = x
        let scale = scale / fontscale
        text
        |> Seq.iter
            (function
             | ' ' -> x <- x + spacing * scale
             | c -> 
                let s = font.Char(c)
                let w = float32 s.Width * scale
                let h = float32 s.Height * scale
                Draw.rect(Rect.create x y (x + w) (y + h)) color s
                x <- x + w - 0.5f * scale * fontscale)

    let defaultFont = SpriteFont(new Font("Akrobat Black", fontscale))

    let drawJust(font: SpriteFont, text, scale, x, y, color, just: float32) = draw(font, text, scale, x - measure(font, text) * scale * just, y, color)