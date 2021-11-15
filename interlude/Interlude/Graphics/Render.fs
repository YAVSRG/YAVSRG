namespace Interlude.Graphics

open System
open System.Drawing
open OpenTK.Graphics.OpenGL
open Prelude.Common

(*
    Render handling to be used from Game
*)

module Render =
    let mutable (rwidth, rheight) = (1, 1)
    let mutable (vwidth, vheight) = (1.0f, 1.0f)
    let mutable bounds = Rect.zero

    let start() = 
        GL.Clear(ClearBufferMask.ColorBufferBit)
        RenderHelper.drawing <- true
        RenderHelper.enter()

    let finish() =
        RenderHelper.exit()
        RenderHelper.drawing <- false
        GL.Finish()
        GL.Flush()

    let resize(width, height) =
        rwidth <- width
        rheight <- height
        GL.Viewport(new Rectangle(0, 0, width, height))
        let width, height = float32 width, float32 height
        
        let (width, height) =
            if (width < 1920.0f || height < 1000.0f) then
                let r = Math.Max(1920.0f / width, 1000.0f / height);
                (width * r, height * r)
            else (width, height)
        vwidth <- float32 <| Math.Round(float width)
        vheight <- float32 <| Math.Round(float height)

        GL.MatrixMode(MatrixMode.Projection)
        GL.LoadIdentity()
        GL.Ortho(0.0, float vwidth, float vheight, 0.0, 0.0, 1.0)

        bounds <- Rect.create 0.0f 0.0f vwidth vheight |> Rect.expand (1.0f, 1.0f)

    let init(width, height) =
        Logging.Debug(sprintf "GL Version: %s" (GL.GetString StringName.Version))

        GL.Enable(EnableCap.Blend)
        GL.Enable(EnableCap.Texture2D)
        GL.ClearColor(Color.FromArgb(0, 0, 0, 0))
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
            member this.Bind(clear) =
                RenderHelper.exit()
                if List.isEmpty stack then
                    GL.Ortho(-1.0, 1.0, 1.0, -1.0, -1.0, 1.0)
                    GL.Translate(0.0f, -Render.vheight, 0.0f)
                    GL.Viewport(0, 0, int Render.vwidth, int Render.vheight)
                GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, this.fbo_id)
                if clear then GL.Clear(ClearBufferMask.ColorBufferBit)
                RenderHelper.enter()
                stack <- this.fbo_id :: stack
            member this.Unbind() =
                stack <- List.tail stack
                RenderHelper.exit()
                if List.isEmpty stack then
                    GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, 0)
                    GL.Translate(0.0f, Render.vheight, 0.0f)
                    GL.Ortho(-1.0, 1.0, 1.0, -1.0, -1.0, 1.0);
                    GL.Viewport(0, 0, Render.rwidth, Render.rheight)
                else
                    GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, List.head stack)
                RenderHelper.enter()
            member this.Dispose() = in_use.[this.fbo_index] <- false

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

            GL.Ext.GenFramebuffers(1, &fbo_ids.[i])
            GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, fbo_ids.[i])
            GL.RenderbufferStorage(RenderbufferTarget.RenderbufferExt, RenderbufferStorage.Depth24Stencil8, int Render.vwidth, int Render.vheight)
            GL.Ext.FramebufferTexture2D(FramebufferTarget.FramebufferExt, FramebufferAttachment.ColorAttachment0Ext, TextureTarget.Texture2D, texture_ids.[i], 0)
        
        GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, 0)

    let create() =
        { 0 .. (pool_size - 1) }
        |> Seq.tryFind (fun i -> not in_use.[i])
        |> function
            | None -> failwith "All FBOs in pool are in use. Change pool size or (more likely) make sure you dispose of your FBOs"
            | Some i ->
                let sprite: Sprite = { ID = texture_ids.[i]; Width = int Render.vwidth; Height = int Render.vheight; Rows = 1; Columns = 1 }
                in_use.[i] <- true;
                let fbo = { sprite = sprite; fbo_id = fbo_ids.[i]; fbo_index = i }
                fbo.Bind(true)
                fbo

module Stencil =
    let mutable depth = 0

    let create(alphaMasking) =
        RenderHelper.exit()
        if depth = 0 then
            GL.Enable(EnableCap.StencilTest)
            GL.Enable(EnableCap.AlphaTest)
            GL.Clear(ClearBufferMask.StencilBufferBit)
            GL.AlphaFunc((if alphaMasking then AlphaFunction.Greater else AlphaFunction.Always), 0.0f)
        GL.StencilFunc(StencilFunction.Equal, depth, 0xFF)
        GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Incr)
        RenderHelper.enter()
        depth <- depth + 1

    let draw() = 
        RenderHelper.exit()
        GL.StencilFunc(StencilFunction.Equal, depth, 0xFF)
        GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Keep)
        RenderHelper.enter()

    let finish() =
        depth <- depth - 1
        RenderHelper.exit()
        if depth = 0 then
            GL.Clear(ClearBufferMask.StencilBufferBit)
            GL.Disable(EnableCap.StencilTest)
            GL.Disable(EnableCap.AlphaTest)
        else
            GL.StencilFunc(StencilFunction.Lequal, depth, 0xFF)
        RenderHelper.enter()

(*
    Drawing methods to be used by UI components
*)

module Draw =

    let mutable lastTex = -1;

    let quad (struct (p1, p2, p3, p4): Quad) (struct (c1, c2, c3, c4): QuadColors) (struct (s, struct (u1, u2, u3, u4)): SpriteQuad) =
        if lastTex <> s.ID then
            RenderHelper.exit()
            GL.BindTexture(TextureTarget.Texture2D, s.ID)
            RenderHelper.enter()
            lastTex <- s.ID
        GL.Color4(c1); GL.TexCoord2(u1); GL.Vertex2(p1)
        GL.Color4(c2); GL.TexCoord2(u2); GL.Vertex2(p2)
        GL.Color4(c3); GL.TexCoord2(u3); GL.Vertex2(p3)
        GL.Color4(c4); GL.TexCoord2(u4); GL.Vertex2(p4)
        
    let rect (r: Rect) (c: Color) (s: Sprite) = quad <| Quad.ofRect r <| Quad.colorOf c <| Sprite.gridUV(0, 0) s