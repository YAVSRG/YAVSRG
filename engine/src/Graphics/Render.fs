namespace Percyqaz.Flux.Graphics

open System
open System.Drawing
open SixLabors.ImageSharp.PixelFormats
open OpenTK.Graphics.OpenGL
open OpenTK.Mathematics
open OpenTK.Windowing.GraphicsLibraryFramework

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
    let mutable private _batch : Batch = Unchecked.defaultof<_>

    let private create_flipped_projection (width: float32, height: float32) =
        Matrix4.Identity
        * Matrix4.CreateOrthographic(width, height, 0.0f, 1.0f)
        * Matrix4.CreateTranslation(-1.0f, -1.0f, 0.0f)
        * Matrix4.CreateScale(1.0f, -1.0f, 1.0f)

    let private create_projection (width: float32, height: float32) =
        Matrix4.Identity
        * Matrix4.CreateOrthographic(width, height, 0.0f, 1.0f)
        * Matrix4.CreateTranslation(-1.0f, -1.0f, 0.0f)

    let private FBO_POOL_SIZE = 6
    let private fbo_ids = Array.zeroCreate<int> FBO_POOL_SIZE
    let private texture_ids = Array.zeroCreate<int> FBO_POOL_SIZE
    let private in_use = Array.zeroCreate<bool> FBO_POOL_SIZE

    let mutable private fbo_stack: int list = []

    /// <summary>
    /// Represents a GL 'Frame Buffer Object' from the pool.<br/>
    /// While an FBO is bound, all draw calls render to a virtual image buffer instead of the screen.<br/>
    /// The FBO can then later be used as a sprite to be drawn to the screen.<br/><br/>
    /// FBOs must be disposed of when done with, to return them to the pool.
    /// </summary>
    /// <remarks>The current pool size is 6. Exceeding this will crash the game.</remarks>
    type FBO =
        internal {
            sprite: Sprite
            fbo_id: int
            fbo_index: int
        }
        /// The sprite representation of this FBO, to be used in drawing
        member this.Sprite = this.sprite

        /// Binds this FBO, so drawing goes to its buffer instead of the screen.
        /// Must not be called if already bound.
        member this.Bind(clear) =
            _batch.Draw ()

            if List.isEmpty fbo_stack then
                Shader.set_uniform_mat4 (Shader.projection_loc, create_projection(_width, _height))

            GL.BindFramebuffer(FramebufferTarget.Framebuffer, this.fbo_id)

            if clear then
                GL.Clear(ClearBufferMask.ColorBufferBit)

            fbo_stack <- this.fbo_id :: fbo_stack

        /// Unbinds this FBO, so drawing goes to the screen and it can be used as a sprite.
        /// Must not be called if not already bound.
        member this.Unbind() =
            _batch.Draw ()
            fbo_stack <- List.tail fbo_stack

            if List.isEmpty fbo_stack then
                GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)
                Shader.set_uniform_mat4 (Shader.projection_loc, create_flipped_projection(_width, _height))
            else
                GL.BindFramebuffer(FramebufferTarget.Framebuffer, List.head fbo_stack)

        interface IDisposable with
            override this.Dispose() = in_use.[this.fbo_index] <- false

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

    (*
        User methods for drawing things to the screen
    *)

    /// <summary>
    /// Retrieves the width of the screen in virtual pixels.<br/>
    /// Virtual pixels are scaled, preserving aspect ratio but always keeping the screen at a virtual 1080 pixels tall.
    /// </summary>
    let width() = _width

    /// <summary>
    /// Retrieves the height of the screen in virtual pixels.<br/>
    /// Virtual pixels are scaled, preserving aspect ratio but always keeping the screen at a virtual 1080 pixels tall.
    /// </summary>
    /// <remarks>
    /// This function therefore always returns 1080.0 - A future feature may come to customise virtual pixel rules
    /// </remarks>
    let height() = _height

    /// <summary>
    /// Retrieves the bounding box of the screen in virtual pixels.<br/>
    /// Virtual pixels are scaled, preserving aspect ratio but always keeping the screen at a virtual 1080 pixels tall.
    /// Equivalent to <c>Rect.Box(0.0f, 0.0f, width(), height())</c>.
    /// </summary>
    let bounds() = _bounds

    /// <summary>
    /// Gets the real dimensions of the viewport, as screen pixels.<br/>
    /// This is not necessarily the dimensions of the window, as this does not include the window border/decorations.
    /// </summary>
    let viewport_size() = _viewport_width, _viewport_height

    /// <summary>
    /// Gets and binds an <see cref="FBO"/> from the pool.<br/>
    /// While an FBO is bound, all draw calls render to a virtual image buffer instead of the screen.<br/>
    /// The FBO can then later be used as a sprite to be drawn to the screen.
    /// </summary>
    let borrow_fbo () =
        { 0 .. (FBO_POOL_SIZE - 1) }
        |> Seq.tryFind (fun i -> not in_use.[i])
        |> function
            | None -> failwithf "Ran out of FBOs! (%i max) - Some are likely not being disposed after use" FBO_POOL_SIZE
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

    let mutable private stencil_depth = 0

    /// <summary>
    /// Begins drawing a stencil to the screen.<br/>
    /// In this mode, in addition to drawing to the screen, any modified pixels are added to the stencil layer.<br/>
    /// If <paramref name="use_alpha_masking"/> is true, transparent pixels will not be added to the stencil layer.<br/><br/>
    /// <see cref="stencil_begin_draw"/> should be called next to start using the stencil as a drawing mask,
    /// in this mode stencilled pixels will be the only pixels that can be updated.<br/><br/>
    /// Stencils can be nested through nested calls to stencil_create.
    /// </summary>
    /// <remarks>To create a stencil layer without drawing anything on-screen, use transparent rectangles and quads with no alpha masking.</remarks>
    let stencil_create (use_alpha_masking: bool) =
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

    /// <summary>
    /// Begins drawing to the screen, using the stencil created by <see cref="stencil_create"/> as a mask.<br/>
    /// In this mode, pixels can only be updated if they were stencilled during the previous step.<br/>
    /// <see cref="stencil_begin_finish"/> should be called next to finish using the stencil and return to normal drawing.
    /// </summary>
    let stencil_begin_draw () =
        _batch.Draw ()

        GL.ColorMask(true, true, true, true)
        GL.StencilFunc(StencilFunction.Equal, stencil_depth, 0xFF)
        GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Keep)

    /// <summary>
    /// Begins drawing to the screen, using the stencil created by <see cref="stencil_create"/> as a mask.<br/>
    /// In this mode, pixels can only be updated if they were stencilled during the previous step.<br/>
    /// <see cref="stencil_begin_finish"/> should be called next to finish using the stencil and return to normal drawing.
    /// </summary>
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

    /// <summary>
    /// Applies an alpha multiplier to all subsequent draw calls.<br/>
    /// <paramref name="multiplier"/> is a multiplier from 0.0 to 1.0.<br/>
    /// The previous multiplier is returned so it can be restored with <see cref="alpha_multiplier_restore"/>.
    /// </summary>
    let alpha_multiplier_begin (multiplier: float32) : float32 =
        assert(multiplier >= 0.0f && multiplier <= 1.0f)
        if multiplier <> alpha_mult then
            _batch.Draw ()
            Shader.set_uniform_f32 (Shader.alpha_mult_loc, multiplier)
            let previous_mult = alpha_mult
            alpha_mult <- multiplier
            previous_mult
        else multiplier

    /// <summary>
    /// Applies an alpha multiplier to all subsequent draw calls.<br/>
    /// <paramref name="multiplier"/> is a multiplier from 0.0 to 1.0.<br/>
    /// Restore should be used to revert a multiplier previously used, and each rendered frame should end with this multiplier reset to 1.0
    /// </summary>
    let alpha_multiplier_restore (multiplier: float32) =
        assert(multiplier >= 0.0f && multiplier <= 1.0f)
        if multiplier <> alpha_mult then
            _batch.Draw ()
            Shader.set_uniform_f32 (Shader.alpha_mult_loc, multiplier)
            alpha_mult <- multiplier

    /// <summary>
    /// Draws an untextured quad to the screen.
    /// </summary>
    let quad (q: Quad) (c: QuadColors) =
        _batch.Vertex(q.TopLeft, Vector2.Zero, c.TopLeft, 0)
        _batch.Vertex(q.TopRight, Vector2.Zero, c.TopRight, 0)
        _batch.Vertex(q.BottomRight, Vector2.Zero, c.BottomRight, 0)
        _batch.Vertex(q.TopLeft, Vector2.Zero, c.TopLeft, 0)
        _batch.Vertex(q.BottomRight, Vector2.Zero, c.BottomRight, 0)
        _batch.Vertex(q.BottomLeft, Vector2.Zero, c.BottomLeft, 0)

    /// <summary>
    /// Draws a textured quad to the screen.
    /// </summary>
    let tex_quad (q: Quad) (c: QuadColors) ({ Texture = t; Layer = layer; UV = uv } : QuadTexture) =
        _batch.Texture t
        _batch.Vertex(q.TopLeft, uv.TopLeft, c.TopLeft, layer)
        _batch.Vertex(q.TopRight, uv.TopRight, c.TopRight, layer)
        _batch.Vertex(q.BottomRight, uv.BottomRight, c.BottomRight, layer)
        _batch.Vertex(q.TopLeft, uv.TopLeft, c.TopLeft, layer)
        _batch.Vertex(q.BottomRight, uv.BottomRight, c.BottomRight, layer)
        _batch.Vertex(q.BottomLeft, uv.BottomLeft, c.BottomLeft, layer)

    /// <summary>
    /// Draws a rectangular sprite to the screen.
    /// </summary>
    let sprite (r: Rect) (c: Color) (s: Sprite) =
        tex_quad r.AsQuad c.AsQuad <| Sprite.pick_texture (0, 0) s

    /// <summary>
    /// Draws an untextured rectangle to the screen.
    /// </summary>
    let rect (r: Rect) (c: Color) =
        quad r.AsQuad c.AsQuad

    (*
        Internal functions used by the Game and Window threads
    *)

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

    let internal start () =
        GL.Clear(ClearBufferMask.ColorBufferBit)
        _batch.Start ()

    let internal finish () =
        _batch.Finish ()
        assert(stencil_depth = 0)
        assert(alpha_mult = 1.0f)
        GL.Flush()

    let internal init (width, height) =
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

    (*
        Additional utilities
    *)

    let debug_info() : string =

        let glfw_version =
            let mutable major = 0
            let mutable minor = 0
            let mutable rev = 0
            GLFW.GetVersion(&major, &minor, &rev)
            sprintf "%i.%i.%i" major minor rev

        let texture_units =
            sprintf
                "%i units; %i max size; %i max layers"
                Texture.MAX_TEXTURE_UNITS
                Texture.MAX_TEXTURE_SIZE
                Texture.MAX_ARRAY_TEXTURE_LAYERS

        let specs =
            sprintf
                "x64: %A Cores: %i Memory: %i"
                Environment.Is64BitProcess
                Environment.ProcessorCount
                Environment.WorkingSet

        sprintf
            """-- RENDERER DEBUG INFO --
GLFW Version: %s
GL Version: %s
GL Renderer: %s
Texture units: %s
OS: %s
Process: %s"""
            glfw_version
            (GL.GetString StringName.Version)
            (GL.GetString StringName.Renderer)
            texture_units
            Environment.OSVersion.VersionString
            specs

    open SixLabors.ImageSharp
    open SixLabors.ImageSharp.Processing

    let take_screenshot () : Image<Rgba32> =
        let data = System.Runtime.InteropServices.Marshal.AllocHGlobal(_viewport_width * _viewport_height * 4)

        GL.ReadPixels(0, 0, _viewport_width, _viewport_height, PixelFormat.Rgba, PixelType.UnsignedByte, data)

        let image: Image<Rgba32> =
            Image<Rgba32>
                .LoadPixelData(new Span<byte>(data.ToPointer(), (_viewport_width * _viewport_height * 4)), _viewport_width, _viewport_height)

        image.Mutate(fun i -> i.Flip(FlipMode.Vertical) |> ignore)
        image