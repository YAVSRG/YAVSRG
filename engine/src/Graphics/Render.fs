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

module Render =

    let internal DEFAULT_SCREEN = (1920, 1080)

    let mutable internal _viewport_width = fst DEFAULT_SCREEN
    let mutable internal _viewport_height = snd DEFAULT_SCREEN

    let mutable internal _width = fst DEFAULT_SCREEN |> float32
    let mutable internal _height = snd DEFAULT_SCREEN |> float32

    let mutable internal _bounds = Rect.ZERO
    let mutable internal _batch : Batch = Unchecked.defaultof<_> // todo: make private

    let width() = _width
    let height() = _height
    let bounds() = _bounds

    let viewport_size() = _viewport_width, _viewport_height
    
    let create_flipped_projection (width: float32, height: float32) =
        Matrix4.Identity
        * Matrix4.CreateOrthographic(width, height, 0.0f, 1.0f)
        * Matrix4.CreateTranslation(-1.0f, -1.0f, 0.0f)
        * Matrix4.CreateScale(1.0f, -1.0f, 1.0f)

    let create_projection (width: float32, height: float32) =
        Matrix4.Identity
        * Matrix4.CreateOrthographic(width, height, 0.0f, 1.0f)
        * Matrix4.CreateTranslation(-1.0f, -1.0f, 0.0f)

    let private FBO_POOL_SIZE = 6
    let private fbo_ids = Array.zeroCreate<int> FBO_POOL_SIZE
    let private texture_ids = Array.zeroCreate<int> FBO_POOL_SIZE
    let private in_use = Array.zeroCreate<bool> FBO_POOL_SIZE

    let mutable private fbo_stack: int list = []

    type FBO =
        internal {
            sprite: Sprite
            fbo_id: int
            fbo_index: int
        }
        member this.Sprite = this.sprite

        member this.Bind(clear) =
            _batch.Draw ()

            if List.isEmpty fbo_stack then
                Shader.set_uniform_mat4 (Shader.projection_loc, create_projection(_width, _height))

            GL.BindFramebuffer(FramebufferTarget.Framebuffer, this.fbo_id)

            if clear then
                GL.Clear(ClearBufferMask.ColorBufferBit)

            fbo_stack <- this.fbo_id :: fbo_stack

        member this.Unbind() =
            _batch.Draw ()
            fbo_stack <- List.tail fbo_stack

            if List.isEmpty fbo_stack then
                GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)
                Shader.set_uniform_mat4 (Shader.projection_loc, create_flipped_projection(_width, _height))
            else
                GL.BindFramebuffer(FramebufferTarget.Framebuffer, List.head fbo_stack)

        member this.Dispose() = in_use.[this.fbo_index] <- false

    let private initialise_fbos () =
        for i in 0 .. (FBO_POOL_SIZE - 1) do
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
                _viewport_width,
                _viewport_height,
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
                _viewport_width,
                _viewport_height
            )

            GL.FramebufferTextureLayer(
                FramebufferTarget.Framebuffer,
                FramebufferAttachment.ColorAttachment0,
                texture_ids.[i],
                0,
                1
            )

        GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)

    let borrow_fbo () =
        { 0 .. (FBO_POOL_SIZE - 1) }
        |> Seq.tryFind (fun i -> not in_use.[i])
        |> function
            | None -> failwith "Ran out of FBOs! (6 max) - Some are likely not being disposed after use"
            | Some i ->
                let texture: Texture =
                    {
                        Handle = texture_ids.[i]
                        TextureUnit = 0

                        Width = _viewport_width
                        Height = _viewport_height
                        Layers = 1

                        References = -1
                    }

                let sprite: Sprite =
                    {
                        Texture = texture

                        X = 0
                        Y = 0
                        Z = 1

                        GridWidth = _viewport_width
                        GridHeight = _viewport_height

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

    let internal viewport_resized (width, height) =
        assert(width <> 0 && height <> 0)
        
        _viewport_width <- width
        _viewport_height <- height
        GL.Viewport(new Rectangle(0, 0, width, height))
        let width, height = float32 width, float32 height
        _width <- (width / height) * 1080.0f
        _height <- 1080.0f

        Shader.set_uniform_mat4 (Shader.projection_loc, create_flipped_projection(_width, _height))

        _bounds <- Rect.Box(0.0f, 0.0f, _width, _height)

        initialise_fbos ()

    let mutable private stencil_depth = 0

    let stencil_create (use_alpha_masking) =
        _batch.Draw ()

        if stencil_depth = 0 then
            GL.Enable(EnableCap.StencilTest)
            GL.Clear(ClearBufferMask.StencilBufferBit)
            GL.StencilMask(0xFF)
            GL.ColorMask(false, false, false, false)
            Shader.set_uniform_i32 (Shader.alpha_masking_loc, (if use_alpha_masking then 1 else 0))

        GL.StencilFunc(StencilFunction.Equal, stencil_depth, 0xFF)
        GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Incr)
        stencil_depth <- stencil_depth + 1

    let stencil_begin_draw () =
        _batch.Draw ()

        GL.ColorMask(true, true, true, true)
        GL.StencilFunc(StencilFunction.Equal, stencil_depth, 0xFF)
        GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Keep)

    let stencil_finish () =
        _batch.Draw ()

        stencil_depth <- stencil_depth - 1

        if stencil_depth = 0 then
            GL.Clear(ClearBufferMask.StencilBufferBit)
            GL.Disable(EnableCap.StencilTest)
            GL.StencilMask(0x00)
            Shader.set_uniform_i32 (Shader.alpha_masking_loc, 0)
        else
            GL.StencilFunc(StencilFunction.Lequal, stencil_depth, 0xFF)

    let mutable private alpha_mult = 0.0f

    let alpha_multiplier_begin (m: float32) : float32 =
        assert(m >= 0.0f && m <= 1.0f)
        if m <> alpha_mult then
            _batch.Draw ()
            Shader.set_uniform_f32 (Shader.alpha_mult_loc, m)
            let previous_mult = alpha_mult
            alpha_mult <- m
            previous_mult
        else m

    let alpha_multiplier_restore (m: float32) =
        assert(m >= 0.0f && m <= 1.0f)
        if m <> alpha_mult then
            _batch.Draw ()
            Shader.set_uniform_f32 (Shader.alpha_mult_loc, m)
            let previous_mult = alpha_mult
            alpha_mult <- m

    let internal start () =
        GL.Clear(ClearBufferMask.ColorBufferBit)
        _batch.Start ()

    let internal finish () =
        _batch.Finish ()
        assert(stencil_depth = 0)
        assert(alpha_mult = 1.0f)
        GL.Flush()

    let internal init (width, height) =
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

        let width, height =
            if width <= 0 || height <= 0 then
                DEFAULT_SCREEN
            else width, height

        viewport_resized(width, height)

        Shader.init()
        _batch <- Batch.Create(1024)
        alpha_multiplier_restore 1.0f

    open SixLabors.ImageSharp
    open SixLabors.ImageSharp.Processing

    let take_screenshot () : Image<Rgba32> =
        let data = System.Runtime.InteropServices.Marshal.AllocHGlobal(_viewport_width * _viewport_height * 4)

        GL.ReadPixels(0, 0, _viewport_width, _viewport_height, PixelFormat.Rgba, PixelType.UnsignedByte, data)

        let image: Image<Rgba32> =
            Image<Rgba32>
                .LoadPixelData(new Span<byte>(data.ToPointer(), (_viewport_width * _viewport_height * 4)), _viewport_width, _viewport_height)

        image.Mutate(fun i -> i.RotateFlip(RotateMode.Rotate180, FlipMode.Horizontal) |> ignore)
        image

(*
    Drawing methods to be used by UI components
*)

module Draw =

    let mutable private last_texture_handle = -1

    let untextured_quad (struct (p1, p2, p3, p4): Quad) (struct (c1, c2, c3, c4): QuadColors) =
        Render._batch.Vertex(p1, Vector2.Zero, c1, 0)
        Render._batch.Vertex(p2, Vector2.Zero, c2, 0)
        Render._batch.Vertex(p3, Vector2.Zero, c3, 0)
        Render._batch.Vertex(p1, Vector2.Zero, c1, 0)
        Render._batch.Vertex(p3, Vector2.Zero, c3, 0)
        Render._batch.Vertex(p4, Vector2.Zero, c4, 0)

    let quad
        (struct (p1, p2, p3, p4): Quad)
        (struct (c1, c2, c3, c4): QuadColors)
        ({ Texture = t; Layer = layer; UV = struct (u1, u2, u3, u4) } : QuadTexture)
        =

        if last_texture_handle <> t.Handle then
            Render._batch.Draw ()

            if t.TextureUnit = 0 then
                GL.BindTexture(TextureTarget.Texture2DArray, t.Handle)

            Shader.set_uniform_i32 (Shader.sampler_loc, t.TextureUnit)
            last_texture_handle <- t.Handle
            
        Render._batch.Vertex(p1, u1, c1, layer)
        Render._batch.Vertex(p2, u2, c2, layer)
        Render._batch.Vertex(p3, u3, c3, layer)
        Render._batch.Vertex(p1, u1, c1, layer)
        Render._batch.Vertex(p3, u3, c3, layer)
        Render._batch.Vertex(p4, u4, c4, layer)

    let sprite (r: Rect) (c: Color) (s: Sprite) =
        quad r.AsQuad c.AsQuad <| Sprite.pick_texture (0, 0) s

    let rect (r: Rect) (c: Color) =
        untextured_quad r.AsQuad c.AsQuad