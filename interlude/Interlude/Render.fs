namespace Interlude.Render

open System
open OpenTK
open OpenTK.Graphics.OpenGL
open System.Collections.Generic

(*
    Storage of rectangles as left, top, right, bottom
    Preferred over left, top, width, height for a handful of reasons
*)

type Rect = (struct(float32 * float32 * float32 * float32))

module Rect = 

    let create l t r b : Rect = struct(l, t, r, b)

    let width (struct (left, _, right, _): Rect) = right - left
    let height (struct (_, top, _, bottom): Rect) = bottom - top

    let centerX (struct (left, _, right, _): Rect) = (right + left) * 0.5f
    let centerY (struct (_, top, _, bottom): Rect) = (bottom + top) * 0.5f
    let center (r: Rect) = (centerX r, centerY r)
    let centerV (r: Rect) = new Vector2(centerX r, centerY r)

    let expand (x,y) (struct (left, top, right, bottom): Rect) =
        struct (left - x, top - y, right + x, bottom + y)

    let sliceLeft v (struct (left, top, _, bottom): Rect) =
        struct (left, top, left + v, bottom)

    let sliceTop v (struct (left, top, right, _): Rect) =
        struct (left, top, right, top + v)

    let sliceRight v (struct (_, top, right, bottom): Rect) =
        struct (right - v, top, right, bottom)

    let sliceBottom v (struct (left, _, right, bottom): Rect) =
        struct (left, bottom - v, right, bottom)

    let trimLeft v (struct (left, top, right, bottom): Rect) =
        struct (left + v, top, right, bottom)

    let trimTop v (struct (left, top, right, bottom): Rect) =
        struct (left, top + v, right, bottom)

    let trimRight v (struct (left, top, right, bottom): Rect) =
        struct (left, top, right - v, bottom)

    let trimBottom v (struct (left, top, right, bottom): Rect) =
        struct (left, top, right, bottom - v)

    let zero = create 0.f 0.f 0.f 0.f
    let one = create 0.f 0.f 1.f 1.f

(*
    Simple storage of vertices to render as a quad
*)

type Quad = (struct(Vector2 * Vector2 * Vector2 * Vector2))
type QuadColors = (struct(Color * Color * Color * Color))

module Quad =

    let ofRect (struct (l, t, r, b) : Rect) : Quad =
        struct (new Vector2(l, t), new Vector2(r, t), new Vector2(r, b), new Vector2(l, b))

    let create c1 c2 c3 c4 : Quad = struct (c1, c2, c3, c4)

    let colorOf c : QuadColors = struct (c, c, c, c)

(*
    Sprites and content uploading
*)

[<Struct>]
type Sprite = { ID:int; Width:int; Height:int; Rows:int; Columns:int }
with
    member this.WithUV(q: Quad): SpriteQuad = struct (this, q)
    member this.TilingUV(s, x, y): SpriteQuad =
        let w = float32 this.Width
        struct (this, Quad.ofRect(Rect.create (x / w / s) (y / w / s) (x / w / s + s) (y / w / s + s)))
    static member Default = { ID=0; Width=1; Height=1; Rows=1; Columns=1 }
    static member DefaultQuad: SpriteQuad = struct (Sprite.Default, Quad.ofRect Rect.one)
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

    let mutable (rwidth, rheight) = (1, 1)
    let mutable (vwidth, vheight) = (1.0f, 1.0f)
    let mutable bounds = Rect.zero

    let resize(width, height) =
        rwidth <- width
        rheight <- height
        let width, height = float32 width, float32 height
        GL.MatrixMode(MatrixMode.Projection)
        GL.LoadIdentity()
        
        let (width, height) =
            if (width < 1920.0f || height < 1000.0f) then
                let r = Math.Max(1920.0f / width, 1000.0f / height);
                (width * r, height * r)
            else (width, height)
        vwidth <- float32 <| Math.Round(float width)
        vheight <- float32 <| Math.Round(float height)
        //GL.Ortho(-3.0, 3.0, -3.0, 3.0, 0.0, 1.0)
        GL.Ortho(0.0, float vwidth, float vheight, 0.0, 0.0, 1.0)
        bounds <- Rect.create 0.f 0.f vwidth vheight

    let start() = 
        GL.Clear(ClearBufferMask.ColorBufferBit)

    let finish() =
        GL.Finish()
        GL.Flush()

    let init(width, height) =
        GL.Enable(EnableCap.Blend)
        GL.Enable(EnableCap.Texture2D)
        GL.ClearColor(Color.Black)
        GL.ClearStencil(0x00)
        GL.Arb.BlendFuncSeparate(0, BlendingFactor.SrcAlpha, BlendingFactor.OneMinusSrcAlpha, BlendingFactor.One, BlendingFactor.OneMinusSrcAlpha)
        GL.ClearStencil(0x00)
        resize(width, height)

module FBO =

    let private pool_size = 6
    let private fbo_ids = Array.zeroCreate<int> pool_size
    let private texture_ids = Array.zeroCreate<int> pool_size
    let private in_use = Array.zeroCreate<bool> pool_size

    let mutable private stack: int list = []
    
    type FBO =
        { sprite: Sprite; fbo_id: int; fbo_index: int }
        with
            member this.Bind() =
                if List.isEmpty stack then
                    GL.Ortho(-1.0, 1.0, 1.0, -1.0, -1.0, 1.0);
                    GL.Viewport(0, 0, int Render.vwidth, int Render.vheight)
                GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, this.fbo_id);
                stack <- this.fbo_id :: stack
            member this.Unbind() =
                stack <- List.tail stack
                if List.isEmpty stack then
                    GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, 0);
                    GL.Ortho(-1.0, 1.0, 1.0, -1.0, -1.0, 1.0);
                    GL.Viewport(0, 0, Render.rwidth, Render.rheight);
                else
                    GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, List.head stack);
            interface IDisposable with member this.Dispose() = in_use.[this.fbo_index] <- false

    let init() =
        for i in 0 .. (pool_size - 1) do
            if (texture_ids.[i] <> 0) then
                GL.DeleteTexture(texture_ids.[i])
                texture_ids.[i] <- 0

            if (fbo_ids.[i] <> 0) then
                GL.Ext.DeleteFramebuffer(fbo_ids.[i])
                fbo_ids.[i] <- 0

            texture_ids.[i] <- GL.GenTexture()
            GL.BindTexture(TextureTarget.Texture2D, texture_ids.[i])
            GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, int Render.vwidth, int Render.vheight, 0, PixelFormat.Rgba, PixelType.UnsignedByte, IntPtr.Zero)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Linear)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Linear)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, (int)TextureWrapMode.Repeat)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, (int)TextureWrapMode.Repeat)

            GL.Ext.GenFramebuffers(1, &fbo_ids.[i]);
            GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, fbo_ids.[i]);
            GL.RenderbufferStorage(RenderbufferTarget.RenderbufferExt, RenderbufferStorage.Depth24Stencil8, int Render.vwidth, int Render.vheight);
            GL.Ext.FramebufferTexture2D(FramebufferTarget.FramebufferExt, FramebufferAttachment.ColorAttachment0Ext, TextureTarget.Texture2D, texture_ids.[i], 0);
        
        GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, 0);

    let create() =
        { 0 .. (pool_size - 1) }
        |> Seq.tryFind (fun i -> not in_use.[i])
        |> function
            | None -> failwith "All FBOs in pool are in use. Change pool size or (far more likely) make sure you dispose of your FBOs"
            | Some i ->
                let sprite: Sprite = { ID = texture_ids.[i]; Width = int Render.vwidth; Height = int Render.vheight; Rows = 1; Columns = 1 }
                in_use.[i] <- true;
                let fbo = { sprite = sprite; fbo_id = fbo_ids.[i]; fbo_index = i }
                fbo.Bind()
                GL.Clear(ClearBufferMask.ColorBufferBit)
                fbo

module Stencil =
    let mutable depth = 0

    let create() =
        if depth = 0 then
            GL.Enable(EnableCap.StencilTest)
            GL.Enable(EnableCap.AlphaTest);
            GL.Clear(ClearBufferMask.StencilBufferBit)
            GL.AlphaFunc(AlphaFunction.Greater, 0.0f);
        GL.StencilFunc(StencilFunction.Equal, depth, 0xFF)
        GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Incr)
        depth <- depth + 1

    let draw() = 
        GL.StencilFunc(StencilFunction.Equal, depth, 0xFF)
        GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Keep)

    let finish() =
        depth <- depth - 1
        if depth = 0 then
            GL.Clear(ClearBufferMask.StencilBufferBit)
            GL.Disable(EnableCap.StencilTest)
            GL.Disable(EnableCap.AlphaTest)
        else
            GL.StencilFunc(StencilFunction.Lequal, depth, 0xFF)

(*
    Drawing methods to be used by UI components
*)

module Draw =

    let quad (struct (p1, p2, p3, p4): Quad) (struct (c1, c2, c3, c4): QuadColors) (struct (s, struct (u1,u2,u3,u4)): SpriteQuad) =
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

module Text =
    
    open System.Drawing
    open System.Drawing.Text

    let private fontscale = 100.f
    let private spacing = 0.25f
    let private shadow = 0.1f

    [<AllowNullLiteral>]
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
        //idk why i cant call dispose manually when this implements IDisposable
        member this.Dispose() =
            fontLookup.Values
            |> Seq.iter Sprite.destroy

    let measure(font: SpriteFont, text: string): float32 =
        text |> Seq.fold (fun v c -> v + (c |> function | ' ' -> spacing | c -> -0.5f + float32 (font.Char(c).Width) / fontscale)) 0.5f

    let draw(font: SpriteFont, text: string, scale, x, y, color) =
        let mutable x = x
        let scale2 = scale / fontscale
        text
        |> Seq.iter
            (function
             | ' ' -> x <- x + spacing * scale
             | c -> 
                let s = font.Char(c)
                let w = float32 s.Width * scale2
                let h = float32 s.Height * scale2
                Draw.rect(Rect.create x y (x + w) (y + h)) color s
                x <- x + w - 0.5f * scale)

    let drawJust(font: SpriteFont, text, scale, x, y, color, just: float32) = draw(font, text, scale, x - measure(font, text) * scale * just, y, color)

    let drawFill(font: SpriteFont, text, bounds, color, just: float32) =
        let w = measure(font, text)
        let scale = Math.Min(Rect.height bounds * 0.6f, (Rect.width bounds / w))
        let struct (l, _, r, _) = bounds
        let x = (1.0f - just) * (l + scale * w * 0.5f) + just * (r - scale * w * 0.5f) - w * scale * 0.5f
        draw(font, text, scale, x, Rect.centerY bounds - scale * 0.75f, color)

    let createFont (str: string) =
        let f =
            if str.Contains('.') then
                //targeting a specific file
                try
                    use pfc = new PrivateFontCollection()
                    pfc.AddFontFile(str)
                    new Font(pfc.Families.[0], fontscale)
                with
                | err ->
                    Prelude.Common.Logging.Error("Failed to load font file: " + str) (err.ToString())
                    new Font(str, fontscale)
            else
                new Font(str, fontscale)
        new SpriteFont(f)