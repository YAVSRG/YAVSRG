namespace Interlude.Graphics

open System
open System.Drawing
open System.Runtime.InteropServices
open System.Collections.Generic
open OpenTK.Graphics.OpenGL
open OpenTK.Mathematics
open Prelude.Common
open Interlude
open ObjectLayoutInspector

(*
    Render handling to be used from Game
*)

module Render =
    
    (*
    [<NoEquality; NoComparison>]
    [<Struct; StructLayout(LayoutKind.Sequential, Pack=1)>]
    type Vertex =
        val Position: Vector2
        val TextureCoordinate: Vector2
        val Color: Vector4
        val TextureID: float32
        new(pos, col, texCoord, texID) = { Position = pos; Color = col; TextureCoordinate = texCoord; TextureID = texID }
        with static member SizeInBytes = Marshal.SizeOf<Vertex>();

    module SpriteBatch =

        let mutable ebo = 0
        let mutable vbo = 0

        let mutable DEFAULT = 0

        let maxQuads = 3000
        let maxVertexCount = maxQuads * 4
        let maxIndexCount = maxQuads * 4
        let maxTextureCount = 32

        let textureToSlot = new Dictionary<int, float32>()
        let textureSlots = Array.create maxTextureCount 0
        let mutable textureCount = 0

        let vertex = Array.create maxVertexCount (Vertex())
        let index = Array.create maxIndexCount 0

        let mutable quadsToDraw = 0

        let init() =
            GL.VertexAttribFormat(0, 2, VertexAttribType.Float, false, 0)
            GL.VertexAttribBinding(0, 0)
            GL.EnableVertexAttribArray(0)

            GL.VertexAttribFormat(1, 4, VertexAttribType.Float, false, 2 * sizeof<float>)
            GL.VertexAttribBinding(1, 0)
            GL.EnableVertexAttribArray(1)
            
            GL.VertexAttribFormat(2, 2, VertexAttribType.Float, false, 6 * sizeof<float>)
            GL.VertexAttribBinding(2, 0)
            //GL.EnableVertexAttribArray(2)

            GL.VertexAttribFormat(3, 1, VertexAttribType.Float, false, 8 * sizeof<float>)
            GL.VertexAttribBinding(3, 0)
            GL.EnableVertexAttribArray(3)

            GL.CreateBuffers(1, &vbo)
            GL.CreateBuffers(1, &ebo)

            for i in 0 .. (maxIndexCount - 1) do index.[i] <- i
            GL.NamedBufferData(vbo, maxVertexCount * Vertex.SizeInBytes, vertex, BufferUsageHint.DynamicDraw)
            GL.NamedBufferData(ebo, maxIndexCount * sizeof<int>, index, BufferUsageHint.StaticDraw)

        let start() =
            quadsToDraw <- 0
            textureCount <- 0
            textureToSlot.Clear()

        let flush() =
            for i in 0 .. (textureCount - 1) do GL.BindTextureUnit(i, textureSlots.[i]);
            GL.NamedBufferSubData(vbo, IntPtr.Zero, quadsToDraw * 4 * Vertex.SizeInBytes, vertex)
            GL.BindVertexBuffer(0, vbo, IntPtr.Zero, Vertex.SizeInBytes)
            GL.BindBuffer(BufferTarget.ElementArrayBuffer, ebo)
            GL.DrawElements(PrimitiveType.Quads, quadsToDraw * 4, DrawElementsType.UnsignedInt, 0)

        let col2Vec (color: Color) =
            Vector4(float32 color.R / 255.0f, float32 color.G / 255.0f, float32 color.B / 255.0f, float32 color.A / 255.0f)

        let addTex (id: int) : float32 =
            if textureToSlot.ContainsKey(id) then textureToSlot.[id]
            else
                if textureCount = maxTextureCount then flush(); start()
                
                textureToSlot.Add(id, float32 textureCount)
                textureSlots.[textureCount] <- id
                textureCount <- textureCount + 1
                float32 (textureCount - 1)

        let quad (struct (p1, p2, p3, p4): Quad) (struct (c1, c2, c3, c4): QuadColors) (struct (s, struct (u1, u2, u3, u4)): SpriteQuad) =
            if quadsToDraw = maxQuads then flush(); start()
            let i = quadsToDraw * 4
            let sprite = addTex s.ID
            vertex.[i + 0] <- Vertex(p1, col2Vec c1, u1, sprite)
            vertex.[i + 1] <- Vertex(p2, col2Vec c2, u2, sprite)
            vertex.[i + 2] <- Vertex(p3, col2Vec c3, u3, sprite)
            vertex.[i + 3] <- Vertex(p4, col2Vec c4, u4, sprite)
            quadsToDraw <- quadsToDraw + 1

        *)

    //let mutable program = 0
    let mutable (rwidth, rheight) = (1, 1)
    let mutable (vwidth, vheight) = (1.0f, 1.0f)
    let mutable bounds = Rect.zero

    let start() = 
        GL.Clear(ClearBufferMask.ColorBufferBit)
        RenderHelper.drawing <- true
        RenderHelper.enter()
        //SpriteBatch.start()

    let finish() =
        //SpriteBatch.flush()
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
        //let mutable mat = Matrix4.CreateOrthographicOffCenter(0.0f, vwidth, vheight, 0.0f, 0.0f, 1.0f)
        //GL.UniformMatrix4(GL.GetUniformLocation(program, "transform"), false, &mat)

        bounds <- Rect.create 0.0f 0.0f vwidth vheight

    let init(width, height) =
        Logging.Debug("===== Render Engine Starting =====")
            
        Logging.Debug(sprintf "GL Version: %s" (GL.GetString StringName.Version))

        GL.Enable(EnableCap.Blend)
        GL.Enable(EnableCap.Texture2D)
        GL.ClearColor(Color.FromArgb(0, 40, 40, 40))
        GL.Arb.BlendFuncSeparate(0, BlendingFactor.SrcAlpha, BlendingFactor.OneMinusSrcAlpha, BlendingFactor.One, BlendingFactor.OneMinusSrcAlpha)
        GL.ClearStencil(0x00)

        (*

        let vsh = GL.CreateShader(ShaderType.VertexShader)
        GL.ShaderSource(vsh, Utils.getResourceText "shader.vsh")
        GL.CompileShader(vsh)
        let s = (GL.GetShaderInfoLog vsh) in if s <> "" then Logging.Debug(sprintf "Vertex Shader: %s" s)

        let fsh = GL.CreateShader(ShaderType.FragmentShader)
        GL.ShaderSource(fsh, Utils.getResourceText "shader.fsh")
        GL.CompileShader(fsh)
        let s = (GL.GetShaderInfoLog fsh) in if s <> "" then Logging.Debug(sprintf "Fragment Shader: %s" s)

        program <- GL.CreateProgram()
        GL.AttachShader(program, vsh)
        GL.AttachShader(program, fsh)
        GL.LinkProgram(program)
        GL.ValidateProgram(program)
        let s = (GL.GetProgramInfoLog program) in if s <> "" then Logging.Debug(sprintf "Shader Program: %s" s)

        GL.UseProgram(program)
        for i in 0..31 do GL.Uniform1(GL.GetUniformLocation(program, $"u_textures[{i}]"), i)
        SpriteBatch.init()

        use bmp = new Bitmap(1, 1)
        bmp.SetPixel(0, 0, Color.White)
        let def = Sprite.upload(bmp, 1, 1, false)
        Sprite.changeDefault def
        SpriteBatch.DEFAULT <- def.ID
        *)

        resize(width, height)
        
        Logging.Debug("===== Render Engine Started =====")

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
        //Render.SpriteBatch.quad (struct (p1, p2, p3, p4)) (struct (c1, c2, c3, c4)) (struct (s, struct (u1, u2, u3, u4)))
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