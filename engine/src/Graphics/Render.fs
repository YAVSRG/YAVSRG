namespace Percyqaz.Flux.Graphics

open System
open System.Drawing
open SixLabors.ImageSharp.PixelFormats
open OpenTK.Graphics.OpenGL
open OpenTK.Mathematics
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common

(*
    Render handling to be used from Game
*)
module Viewport =

    let DEFAULT_SCREEN = (1920, 1080)
    let mutable (viewport_width, viewport_height) = DEFAULT_SCREEN
    let mutable (virtual_screen_width, virtual_screen_height) = (1920.0f, 1080.0f)
    let mutable bounds = Rect.ZERO

    let create_flipped_projection (width: float32, height: float32) =
        Matrix4.Identity
        * Matrix4.CreateOrthographic(width, height, 0.0f, 1.0f)
        * Matrix4.CreateTranslation(-1.0f, -1.0f, 0.0f)
        * Matrix4.CreateScale(1.0f, -1.0f, 1.0f)

    let create_projection (width: float32, height: float32) =
        Matrix4.Identity
        * Matrix4.CreateOrthographic(width, height, 0.0f, 1.0f)
        * Matrix4.CreateTranslation(-1.0f, -1.0f, 0.0f)

open Viewport

module FBO =

    let private POOL_SIZE = 6
    let private fbo_ids = Array.zeroCreate<int> POOL_SIZE
    let private texture_ids = Array.zeroCreate<int> POOL_SIZE
    let private in_use = Array.zeroCreate<bool> POOL_SIZE

    let mutable private fbo_stack: int list = []

    type FBO =
        {
            sprite: Sprite
            fbo_id: int
            fbo_index: int
        }

        member this.Bind(clear) =
            Batch.draw ()

            if List.isEmpty fbo_stack then
                Shader.set_uniform_mat4 (Shader.projection_loc, create_projection(virtual_screen_width, virtual_screen_height))

            GL.BindFramebuffer(FramebufferTarget.Framebuffer, this.fbo_id)

            if clear then
                GL.Clear(ClearBufferMask.ColorBufferBit)

            fbo_stack <- this.fbo_id :: fbo_stack

        member this.Unbind() =
            Batch.draw ()
            fbo_stack <- List.tail fbo_stack

            if List.isEmpty fbo_stack then
                GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)
                Shader.set_uniform_mat4 (Shader.projection_loc, create_flipped_projection(virtual_screen_width, virtual_screen_height))
            else
                GL.BindFramebuffer(FramebufferTarget.Framebuffer, List.head fbo_stack)

        member this.Dispose() = in_use.[this.fbo_index] <- false

    let internal init () =
        for i in 0 .. (POOL_SIZE - 1) do
            if (texture_ids.[i] <> 0) then
                GL.DeleteTexture(texture_ids.[i])
                texture_ids.[i] <- 0

            if (fbo_ids.[i] <> 0) then
                GL.DeleteFramebuffer(fbo_ids.[i])
                fbo_ids.[i] <- 0

            texture_ids.[i] <- GL.GenTexture()
            GL.BindTexture(TextureTarget.Texture2DArray, texture_ids.[i])

            GL.TexImage3D(
                TextureTarget.Texture2DArray,
                0,
                PixelInternalFormat.Rgba,
                viewport_width,
                viewport_height,
                2,
                0,
                PixelFormat.Rgba,
                PixelType.UnsignedByte,
                IntPtr.Zero
            )

            GL.TexSubImage3D<Rgba32>(
                TextureTarget.Texture2DArray,
                0,
                0,
                0,
                0,
                1,
                1,
                1,
                PixelFormat.Rgba,
                PixelType.UnsignedByte,
                [| new Rgba32(255uy, 255uy, 255uy, 255uy) |]
            )

            GL.TexParameter(
                TextureTarget.Texture2DArray,
                TextureParameterName.TextureMinFilter,
                int TextureMinFilter.Linear
            )

            GL.TexParameter(
                TextureTarget.Texture2DArray,
                TextureParameterName.TextureMagFilter,
                int TextureMagFilter.Linear
            )

            GL.TexParameter(
                TextureTarget.Texture2DArray,
                TextureParameterName.TextureWrapS,
                int TextureWrapMode.ClampToEdge
            )

            GL.TexParameter(
                TextureTarget.Texture2DArray,
                TextureParameterName.TextureWrapT,
                int TextureWrapMode.ClampToEdge
            )

            GL.GenFramebuffers(1, &fbo_ids.[i])
            GL.BindFramebuffer(FramebufferTarget.Framebuffer, fbo_ids.[i])

            GL.RenderbufferStorage(
                RenderbufferTarget.RenderbufferExt,
                RenderbufferStorage.Depth24Stencil8,
                viewport_width,
                viewport_height
            )

            GL.FramebufferTextureLayer(
                FramebufferTarget.Framebuffer,
                FramebufferAttachment.ColorAttachment0,
                texture_ids.[i],
                0,
                1
            )

        GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)

    let create () =
        { 0 .. (POOL_SIZE - 1) }
        |> Seq.tryFind (fun i -> not in_use.[i])
        |> function
            | None -> failwith "Ran out of FBOs! (6 max) - Some are likely not being disposed after use"
            | Some i ->
                let texture: Texture =
                    {
                        Handle = texture_ids.[i]
                        TextureUnit = 0

                        Width = viewport_width
                        Height = viewport_height
                        Layers = 1

                        References = -1
                    }

                let sprite: Sprite =
                    {
                        Texture = texture

                        X = 0
                        Y = 0
                        Z = 1

                        GridWidth = viewport_width
                        GridHeight = viewport_height

                        Rows = 1
                        Columns = 1

                        PrecomputedQuad = ValueNone
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

    let internal resize (width, height) =
        assert(width <> 0 && height <> 0)
        
        viewport_width <- width
        viewport_height <- height
        GL.Viewport(new Rectangle(0, 0, width, height))
        let width, height = float32 width, float32 height
        virtual_screen_width <- (width / height) * 1080.0f
        virtual_screen_height <- 1080.0f

        Shader.set_uniform_mat4 (Shader.projection_loc, create_flipped_projection(virtual_screen_width, virtual_screen_height))

        bounds <- Rect.Box(0.0f, 0.0f, virtual_screen_width, virtual_screen_height)

        FBO.init ()

    let internal init () =
        let mutable major = 0
        let mutable minor = 0
        let mutable rev = 0
        GLFW.GetVersion(&major, &minor, &rev)

        Logging.Debug(
            sprintf
                "GL %s | %s | U:%i T:%i L:%i | GLFW %i.%i.%i | C:%i"
                (GL.GetString StringName.Version)
                (GL.GetString StringName.Renderer)
                Texture.MAX_TEXTURE_UNITS
                Texture.MAX_TEXTURE_SIZE
                Texture.MAX_ARRAY_TEXTURE_LAYERS
                major
                minor
                rev
                Environment.ProcessorCount
        )

        GL.Disable(EnableCap.CullFace)
        GL.Enable(EnableCap.Blend)
        GL.Enable(EnableCap.Texture2D)
        GL.ClearColor(Color.FromArgb(0, 0, 0, 0))
        GL.BlendFuncSeparate(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.OneMinusSrcAlpha, BlendingFactorSrc.One, BlendingFactorDest.One)
        GL.ClearStencil(0x00)
        Shader.init()
        Alpha.change_multiplier 1.0f |> ignore

    let internal start () =
        GL.Clear(ClearBufferMask.ColorBufferBit)
        Batch.start ()

    let internal finish () =
        Batch.finish ()
        GL.Flush()

    open SixLabors.ImageSharp
    open SixLabors.ImageSharp.Processing

    let take_screenshot () : Image<Rgba32> =
        let data = System.Runtime.InteropServices.Marshal.AllocHGlobal(viewport_width * viewport_height * 4)

        GL.ReadPixels(0, 0, viewport_width, viewport_height, PixelFormat.Rgba, PixelType.UnsignedByte, data)

        let image: Image<Rgba32> =
            Image<Rgba32>
                .LoadPixelData(new Span<byte>(data.ToPointer(), (viewport_width * viewport_height * 4)), viewport_width, viewport_height)

        image.Mutate(fun i -> i.RotateFlip(RotateMode.Rotate180, FlipMode.Horizontal) |> ignore)
        image

(*
    Drawing methods to be used by UI components
*)

module Draw =

    let mutable private last_texture_handle = -1

    let untextured_quad (struct (p1, p2, p3, p4): Quad) (struct (c1, c2, c3, c4): QuadColors) =

        Batch.vertex p1 Vector2.Zero c1 0
        Batch.vertex p2 Vector2.Zero c2 0
        Batch.vertex p3 Vector2.Zero c3 0
        Batch.vertex p1 Vector2.Zero c1 0
        Batch.vertex p3 Vector2.Zero c3 0
        Batch.vertex p4 Vector2.Zero c4 0

    let quad
        (struct (p1, p2, p3, p4): Quad)
        (struct (c1, c2, c3, c4): QuadColors)
        ({ Texture = t; Layer = layer; UV = struct (u1, u2, u3, u4) } : QuadTexture)
        =

        if last_texture_handle <> t.Handle then
            Batch.draw ()

            if t.TextureUnit = 0 then
                GL.BindTexture(TextureTarget.Texture2DArray, t.Handle)

            Shader.set_uniform_i32 (Shader.sampler_loc, t.TextureUnit)
            last_texture_handle <- t.Handle

        Batch.vertex p1 u1 c1 layer
        Batch.vertex p2 u2 c2 layer
        Batch.vertex p3 u3 c3 layer
        Batch.vertex p1 u1 c1 layer
        Batch.vertex p3 u3 c3 layer
        Batch.vertex p4 u4 c4 layer

    let sprite (r: Rect) (c: Color) (s: Sprite) =
        quad r.AsQuad c.AsQuad <| Sprite.pick_texture (0, 0) s

    let rect (r: Rect) (c: Color) =
        untextured_quad r.AsQuad c.AsQuad