namespace Percyqaz.Flux.Graphics

open System
open System.Drawing
open OpenTK.Graphics.OpenGL
open OpenTK.Mathematics
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common

(*
    Render handling to be used from Game
*)
module Viewport =

    let mutable (rwidth, rheight) = (1, 1)
    let mutable (vwidth, vheight) = (1.0f, 1.0f)
    let mutable bounds = Rect.ZERO

    let create_projection (flip: bool) =
        Matrix4.Identity
        * Matrix4.CreateOrthographic(vwidth, vheight, 0.0f, 1.0f)
        * Matrix4.CreateTranslation(-1.0f, -1.0f, 0.0f)
        * (if flip then
               Matrix4.CreateScale(1.0f, -1.0f, 1.0f)
           else
               Matrix4.Identity)

open Viewport

module FBO =

    let private pool_size = 6
    let private fbo_ids = Array.zeroCreate<int> pool_size
    let private texture_ids = Array.zeroCreate<int> pool_size
    let private in_use = Array.zeroCreate<bool> pool_size

    let mutable private stack: int list = []

    type FBO =
        {
            sprite: Sprite
            fbo_id: int
            fbo_index: int
        }

        member this.Bind(clear) =
            Batch.draw ()

            if List.isEmpty stack then
                Shader.set_uniform_mat4 ("uProjection", create_projection false) Shader.main
                GL.Viewport(0, 0, int vwidth, int vheight)

            GL.BindFramebuffer(FramebufferTarget.Framebuffer, this.fbo_id)

            if clear then
                GL.Clear(ClearBufferMask.ColorBufferBit)

            stack <- this.fbo_id :: stack

        member this.Unbind() =
            Batch.draw ()
            stack <- List.tail stack

            if List.isEmpty stack then
                GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)
                Shader.set_uniform_mat4 ("uProjection", create_projection true) Shader.main
                GL.Viewport(0, 0, rwidth, rheight)
            else
                GL.BindFramebuffer(FramebufferTarget.Framebuffer, List.head stack)

        member this.Dispose() = in_use.[this.fbo_index] <- false

    let internal init () =
        for i in 0 .. (pool_size - 1) do
            if (texture_ids.[i] <> 0) then
                GL.DeleteTexture(texture_ids.[i])
                texture_ids.[i] <- 0

            if (fbo_ids.[i] <> 0) then
                GL.DeleteFramebuffer(fbo_ids.[i])
                fbo_ids.[i] <- 0

            texture_ids.[i] <- GL.GenTexture()
            GL.BindTexture(TextureTarget.Texture2D, texture_ids.[i])

            GL.TexImage2D(
                TextureTarget.Texture2D,
                0,
                PixelInternalFormat.Rgba,
                int vwidth,
                int vheight,
                0,
                PixelFormat.Rgba,
                PixelType.UnsignedByte,
                IntPtr.Zero
            )

            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)

            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)

            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)

            GL.GenFramebuffers(1, &fbo_ids.[i])
            GL.BindFramebuffer(FramebufferTarget.Framebuffer, fbo_ids.[i])

            GL.RenderbufferStorage(
                RenderbufferTarget.RenderbufferExt,
                RenderbufferStorage.Depth24Stencil8,
                int vwidth,
                int vheight
            )

            GL.FramebufferTexture2D(
                FramebufferTarget.Framebuffer,
                FramebufferAttachment.ColorAttachment0,
                TextureTarget.Texture2D,
                texture_ids.[i],
                0
            )

        GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)

    let create () =
        { 0 .. (pool_size - 1) }
        |> Seq.tryFind (fun i -> not in_use.[i])
        |> function
            | None -> failwith "Ran out of FBOs! (6 max) - Some are likely not being disposed after use"
            | Some i ->
                let sprite: Sprite =
                    {
                        ID = texture_ids.[i]
                        TextureUnit = 0

                        Left = 0
                        Top = 0
                        AtlasWidth = int vwidth
                        AtlasHeight = int vheight
                        GridWidth = int vwidth
                        GridHeight = int vheight

                        Rows = 1
                        Columns = 1

                        PrecomputedQuad = ValueNone
                        DefaultQuad = None
                    }

                in_use.[i] <- true

                let fbo =
                    {
                        sprite = sprite
                        fbo_id = fbo_ids.[i]
                        fbo_index = i
                    }

                fbo.Bind true
                fbo

module Render =

    let internal start () =
        GL.Clear(ClearBufferMask.ColorBufferBit)
        Batch.start ()

    let internal finish () =
        Batch.finish ()
        GL.Flush()

    let internal resize (width, height) =
        rwidth <- width
        rheight <- height
        GL.Viewport(new Rectangle(0, 0, width, height))
        let width, height = float32 width, float32 height
        vwidth <- (width / height) * 1080.0f
        vheight <- 1080.0f

        Shader.set_uniform_mat4 ("uProjection", create_projection true) Shader.main

        bounds <- Rect.Box(0.0f, 0.0f, vwidth, vheight)

        FBO.init ()

    let internal init () =
        let mutable major = 0
        let mutable minor = 0
        let mutable rev = 0
        GLFW.GetVersion(&major, &minor, &rev)

        let smoother_vsync_support =
            GLFW.ExtensionSupported("GLX_EXT_swap_control_tear")
            || GLFW.ExtensionSupported("WGL_EXT_swap_control_tear")

        Logging.Debug(
            sprintf
                "GL %s | %s | U:%i T:%i | GLFW %i.%i.%i%s | C:%i"
                (GL.GetString StringName.Version)
                (GL.GetString StringName.Renderer)
                Sprite.MAX_TEXTURE_UNITS
                Sprite.MAX_TEXTURE_SIZE
                major
                minor
                rev
                (if smoother_vsync_support then "*" else "")
                Environment.ProcessorCount
        )

        GL.Disable(EnableCap.CullFace)
        GL.Enable(EnableCap.Blend)
        GL.Enable(EnableCap.VertexArray)
        GL.Enable(EnableCap.Texture2D)
        GL.ClearColor(Color.FromArgb(0, 0, 0, 0))
        GL.BlendFunc(BlendingFactor.SrcAlpha, BlendingFactor.OneMinusSrcAlpha)
        GL.ClearStencil(0x00)
        Shader.on Shader.main
    //for i = 0 to 15 do
    //    let loc = sprintf "samplers[%i]" i
    //    Shader.setUniformInt (loc, i) Shader.main

    open SixLabors.ImageSharp
    open SixLabors.ImageSharp.Processing

    let screenshot () =
        let data = System.Runtime.InteropServices.Marshal.AllocHGlobal(rwidth * rheight * 4)

        GL.ReadPixels(0, 0, rwidth, rheight, PixelFormat.Rgba, PixelType.UnsignedByte, data)

        let image: Image<PixelFormats.Rgba32> =
            Image<PixelFormats.Rgba32>
                .LoadPixelData(new Span<byte>(data.ToPointer(), (rwidth * rheight * 4)), rwidth, rheight)

        image.Mutate(fun i -> i.RotateFlip(RotateMode.Rotate180, FlipMode.Horizontal) |> ignore)
        image

(*
    Drawing methods to be used by UI components
*)

module Draw =

    let mutable private last_texture_handle = -1
    let mutable private last_texture_default_quad = None

    let quad
        (struct (p1, p2, p3, p4): Quad)
        (struct (c1, c2, c3, c4): QuadColors)
        (struct (s, struct (u1, u2, u3, u4)): TexturedQuad)
        =
        if last_texture_handle <> s.ID && s.ID = Sprite.DEFAULT.ID && last_texture_default_quad.IsSome then

            let struct (u1, u2, u3, u4) = last_texture_default_quad.Value
            
            Batch.vertex p1 u1 c1 s.TextureUnit
            Batch.vertex p2 u2 c2 s.TextureUnit
            Batch.vertex p3 u3 c3 s.TextureUnit
            Batch.vertex p1 u1 c1 s.TextureUnit
            Batch.vertex p3 u3 c3 s.TextureUnit
            Batch.vertex p4 u4 c4 s.TextureUnit

        elif last_texture_handle <> s.ID then

            Batch.draw ()

            if s.TextureUnit = 0 then
                GL.BindTexture(TextureTarget.Texture2D, s.ID)

            Shader.set_uniform_i32 ("sampler", s.TextureUnit) Shader.main
            last_texture_default_quad <- s.DefaultQuad
            last_texture_handle <- s.ID

            Batch.vertex p1 u1 c1 s.TextureUnit
            Batch.vertex p2 u2 c2 s.TextureUnit
            Batch.vertex p3 u3 c3 s.TextureUnit
            Batch.vertex p1 u1 c1 s.TextureUnit
            Batch.vertex p3 u3 c3 s.TextureUnit
            Batch.vertex p4 u4 c4 s.TextureUnit

        else

            Batch.vertex p1 u1 c1 s.TextureUnit
            Batch.vertex p2 u2 c2 s.TextureUnit
            Batch.vertex p3 u3 c3 s.TextureUnit
            Batch.vertex p1 u1 c1 s.TextureUnit
            Batch.vertex p3 u3 c3 s.TextureUnit
            Batch.vertex p4 u4 c4 s.TextureUnit

    let sprite (r: Rect) (c: Color) (s: Sprite) =
        quad <| Quad.ofRect r <| Quad.color c <| Sprite.pick_texture (0, 0) s

    let rect (r: Rect) (c: Color) = sprite r c Sprite.DEFAULT
