namespace Interlude.Graphics

open System
open System.Drawing
open OpenTK.Graphics.OpenGL
open OpenTK.Mathematics
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
        Batch.start()

    let finish() =
        Batch.finish()
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

        Shader.on Shader.main
        let projection = 
            Matrix4.Identity
            * Matrix4.CreateOrthographic(vwidth, vheight, 0.0f, 1.0f)
            * Matrix4.CreateTranslation(-1.0f, -1.0f, 0.0f)
            * Matrix4.CreateScale(1.0f, -1.0f, 1.0f)
        Shader.setUniformMat4 ("uProjection", projection) Shader.main
        Shader.setUniformMat4 ("uModel", Matrix4.Identity) Shader.main
        Shader.setUniformMat4 ("uView", Matrix4.Identity) Shader.main

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
                if List.isEmpty stack then
                    GL.Ortho(-1.0, 1.0, 1.0, -1.0, -1.0, 1.0)
                    GL.Translate(0.0f, -Render.vheight, 0.0f)
                    GL.Viewport(0, 0, int Render.vwidth, int Render.vheight)
                GL.BindFramebuffer(FramebufferTarget.Framebuffer, this.fbo_id)
                if clear then GL.Clear(ClearBufferMask.ColorBufferBit)
                stack <- this.fbo_id :: stack
            member this.Unbind() =
                stack <- List.tail stack
                if List.isEmpty stack then
                    GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)
                    GL.Translate(0.0f, Render.vheight, 0.0f)
                    GL.Ortho(-1.0, 1.0, 1.0, -1.0, -1.0, 1.0);
                    GL.Viewport(0, 0, Render.rwidth, Render.rheight)
                else
                    GL.BindFramebuffer(FramebufferTarget.Framebuffer, List.head stack)
            member this.Dispose() = in_use.[this.fbo_index] <- false

    let init() =
        for i in 0 .. (pool_size - 1) do
            if (texture_ids.[i] <> 0) then
                GL.DeleteTexture(texture_ids.[i])
                texture_ids.[i] <- 0

            if (fbo_ids.[i] <> 0) then
                GL.DeleteFramebuffer(fbo_ids.[i])
                fbo_ids.[i] <- 0

            texture_ids.[i] <- GL.GenTexture()
            GL.BindTexture(TextureTarget.Texture2D, texture_ids.[i])
            GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, int Render.vwidth, int Render.vheight, 0, PixelFormat.Rgba, PixelType.UnsignedByte, IntPtr.Zero)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Linear)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Linear)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, (int)TextureWrapMode.Repeat)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, (int)TextureWrapMode.Repeat)

            GL.GenFramebuffers(1, &fbo_ids.[i])
            GL.BindFramebuffer(FramebufferTarget.Framebuffer, fbo_ids.[i])
            GL.RenderbufferStorage(RenderbufferTarget.RenderbufferExt, RenderbufferStorage.Depth24Stencil8, int Render.vwidth, int Render.vheight)
            GL.FramebufferTexture2D(FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2D, texture_ids.[i], 0)
        
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)

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
        if depth = 0 then
            GL.Enable(EnableCap.StencilTest)
            GL.Enable(EnableCap.AlphaTest)
            GL.Clear(ClearBufferMask.StencilBufferBit)
            GL.AlphaFunc((if alphaMasking then AlphaFunction.Greater else AlphaFunction.Always), 0.0f)
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

    let mutable lastTex = -1;

    let quad (struct (p1, p2, p3, p4): Quad) (struct (c1, c2, c3, c4): QuadColors) (struct (s, struct (u1, u2, u3, u4)): SpriteQuad) =
        if lastTex <> s.ID then
            GL.BindTexture(TextureTarget.Texture2D, s.ID)
            Shader.setUniformInt ("uTexture0", 0) Shader.main
            lastTex <- s.ID
        Batch.vertex 0 p1 u1
        Batch.vertex 2 p2 u2
        Batch.vertex 1 p3 u3
        Batch.vertex 3 p4 u4

        Batch.finish()
        Batch.start()
        
    let rect (r: Rect) (c: Color) (s: Sprite) = quad <| Quad.ofRect r <| Quad.colorOf c <| Sprite.gridUV(0, 0) s