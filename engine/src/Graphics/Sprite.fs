namespace Percyqaz.Flux.Graphics

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open OpenTK.Mathematics
open OpenTK.Graphics.OpenGL
open Percyqaz.Common

(*
    Sprites and content uploading
*)

type Bitmap = Image<Rgba32>

// Represents a GL array texture and its properties
type Texture =
    {
        Handle: int
        mutable TextureUnit: int

        Width: int
        Height: int
        Layers: int

        mutable References: int
    }

// Represents a specific area of a Texture, treated as a grid of equal-sized related sprites
// For example the sprite sheet of notes might be on layer 3, offset 0, 0, 8 rows and 8 columns
// The sprite of note color 1, animation frame 1, could then be layer 3, offset 128, 128, 1 row and 1 column
type Sprite =
    {
        Texture: Texture
        X: int
        Y: int
        Z: int

        GridWidth: int
        GridHeight: int

        Rows: int
        Columns: int

        mutable PrecomputedQuad: Quad voption
    }
    member this.Width = this.GridWidth / this.Columns
    member this.Height = this.GridHeight / this.Rows
    member this.AspectRatio = float32 this.Width / float32 this.Height

[<Struct>]
type QuadTexture =
    {
        Texture: Texture
        Layer: int
        UV: Quad
    }
    member this.Transform(func: Quad -> Quad) : QuadTexture =
        {
            Texture = this.Texture
            Layer = this.Layer
            UV = func this.UV
        }

type SpriteUpload =
    {
        Label: string
        Rows: int
        Columns: int
        Image: Bitmap
        DisposeImageAfter: bool
    }
    static member OfImage(label: string, img: Bitmap) : SpriteUpload =
        {
            Label = label
            Rows = 1
            Columns = 1
            Image = img
            DisposeImageAfter = true
        }

[<Struct>]
type SpriteSmoothing =
    | NoSampling
    | LinearSampling
    | LinearSamplingFixEdges

module Texture =

    let [<Literal>] TRACE = false

    let MAX_TEXTURE_UNITS = GL.GetInteger GetPName.MaxTextureImageUnits
    let TOTAL_TEXTURE_UNITS = GL.GetInteger GetPName.MaxCombinedTextureImageUnits
    let MAX_TEXTURE_SIZE = GL.GetInteger GetPName.MaxTextureSize
    let MAX_ARRAY_TEXTURE_LAYERS = GL.GetInteger GetPName.MaxArrayTextureLayers

    // texture unit 0 is reserved for binding uncached sprites
    let private texture_unit_handles: int array = Array.zeroCreate MAX_TEXTURE_UNITS
    let private texture_unit_in_use: bool array = Array.zeroCreate MAX_TEXTURE_UNITS

    let claim_texture_unit (texture: Texture) : bool =
        if texture.TextureUnit <> 0 then
            true
        else

            let texture_unit =
                seq { 1 .. (MAX_TEXTURE_UNITS - 1) }
                |> Seq.tryFind (fun i -> not texture_unit_in_use.[i])
                |> function
                    | None ->
                        Logging.Debug "Texture unit claim failed, all texture units are full"
                        0
                    | Some i ->
                        texture_unit_handles.[i] <- texture.Handle
                        texture_unit_in_use.[i] <- true

                        GL.ActiveTexture(int TextureUnit.Texture0 + i |> enum)
                        GL.BindTexture(TextureTarget.Texture2DArray, texture.Handle)
                        GL.ActiveTexture(TextureUnit.Texture0)

                        if TRACE then Logging.Debug "Texture slot [%i] <- %i" i texture.Handle
                        i

            texture.TextureUnit <- texture_unit
            texture.TextureUnit <> 0

    let unclaim_texture_unit (texture: Texture) : unit =
        if texture.TextureUnit <> 0 then
            if TRACE then Logging.Debug "Texture slot [%i] -> %i" texture.TextureUnit texture.Handle
            texture_unit_in_use.[texture.TextureUnit] <- false
            texture.TextureUnit <- 0

    let destroy (texture: Texture) : unit =
        assert (texture.References = 0)
        unclaim_texture_unit texture
        GL.DeleteTexture texture.Handle
        if TRACE then Logging.Debug "Destroyed texture %i" texture.Handle

    let create (width: int, height: int, layers: int) : Texture =
        let id = GL.GenTexture()
        GL.BindTexture(TextureTarget.Texture2DArray, id)

        {
            Handle = id
            TextureUnit = 0

            Width = width
            Height = height
            Layers = layers

            References = 0
        }

    let create_sprite (x: int, y: int) (layer: int) (w: int, h: int) (rows: int, columns: int) (texture: Texture) : Sprite =
        texture.References <- texture.References + 1

        {
            Texture = texture

            X = x
            Y = y
            Z = layer

            GridWidth = w
            GridHeight = h

            Rows = rows
            Columns = columns

            PrecomputedQuad = ValueNone
        }

    let create_default_sprite (texture: Texture) : Sprite =
        {
            Texture = texture

            X = 0
            Y = 0
            Z = 0

            GridWidth = 1
            GridHeight = 1

            Rows = 1
            Columns = 1

            PrecomputedQuad = ValueSome Rect.Zero.AsQuad
        }

module Sprite =

    [<Literal>]
    let private FRILL_OPACITY = 1uy
    [<Literal>]
    let private FRILL_SIZE = 3
    [<Literal>]
    let private FRILL_THRESHOLD = 0uy

    /// Fix for scenario when linear sampling is used on edges within a texture:
    ///  - Some pixels that are fully opaque, part of the sprite
    ///  - Some pixels are fully transparent, empty space in the sprite
    ///  - ImageSharp sets all full transparent pixels to 0x00000000 (Black + Fully transparent)
    /// Result of sampling: Mid-opacity color partway between color at edge and black
    /// This causes black "frills" to be seen at the edges of textures ingame
    ///
    /// This fix adds a few pixels in all directions that have alpha 0x01 and the same color as the nearby opaque edge
    /// When sampling, no more frills! :)
    let private fix_frills (image: Bitmap) : unit =

        for x = 0 to image.Width - 1 do
            let mutable p: Rgba32 = Rgba32(0uy, 0uy, 0uy, 0uy)
            let mutable s: int = 0
            for y = 0 to image.Height - 1 do
                let pixel = image.[x, y]
                if pixel.A > FRILL_THRESHOLD then
                    p <- Rgba32(pixel.R, pixel.G, pixel.B, FRILL_OPACITY)
                    s <- FRILL_SIZE
                elif s > 0 then
                    s <- s - 1
                    image.[x, y] <- p

            let mutable p: Rgba32 = Rgba32(0uy, 0uy, 0uy, 0uy)
            let mutable s: int = 0
            for y = image.Height - 1 downto 0 do
                let pixel = image.[x, y]
                if pixel.A > FRILL_THRESHOLD then
                    p <- Rgba32(pixel.R, pixel.G, pixel.B, FRILL_OPACITY)
                    s <- FRILL_SIZE
                elif s > 0 then
                    s <- s - 1
                    image.[x, y] <- p

        for y = 0 to image.Height - 1 do
            let mutable p: Rgba32 = Rgba32(0uy, 0uy, 0uy, 0uy)
            let mutable s: int = 0
            for x = 0 to image.Width - 1 do
                let pixel = image.[x, y]
                if pixel.A > FRILL_THRESHOLD then
                    p <- Rgba32(pixel.R, pixel.G, pixel.B, FRILL_OPACITY)
                    s <- FRILL_SIZE
                elif s > 0 then
                    s <- s - 1
                    image.[x, y] <- p

            let mutable p: Rgba32 = Rgba32(0uy, 0uy, 0uy, 0uy)
            let mutable s: int = 0
            for x = image.Width - 1 downto 0 do
                let pixel = image.[x, y]
                if pixel.A > FRILL_THRESHOLD then
                    p <- Rgba32(pixel.R, pixel.G, pixel.B, FRILL_OPACITY)
                    s <- FRILL_SIZE
                elif s > 0 then
                    s <- s - 1
                    image.[x, y] <- p

    /// Fix for scenario when linear sampling is used at the boundaries of a texture
    ///  - Some pixels at border that are fully opaque, part of the sprite
    ///  - Some pixels out of bounds of texture (due to atlasing, etc)
    ///  These pixels get blended to produce fading frills/artifacts on textures that touch the border of an image (like hold bodies)
    ///
    /// This fix adds 1 pixel all around an image, with the same color as the edge pixel
    /// When sampling, produces a solid edge as intended :)
    let private extend_image_boundary (image: Bitmap) : Bitmap =

        let new_image = new Bitmap(image.Width + 2, image.Height + 2)
        new_image.Mutate(fun img -> img.DrawImage(image, Point(1, 1), 1.0f) |> ignore)

        for y = 0 to image.Height - 1 do
            let left = image.[0, y]
            new_image.[0, y + 1] <- left

            let right = image.[image.Width - 1, y]
            new_image.[image.Width, y + 1] <- right

        for x = 0 to new_image.Width - 1 do
            let top = new_image.[x, 1]
            new_image.[x, 0] <- top

            let bottom = new_image.[x, new_image.Height - 2]
            new_image.[x, new_image.Height - 1] <- bottom

        new_image

    let internal precompute_1x1 (sprite: Sprite) : Sprite =
        let stride_x = float32 sprite.GridWidth / float32 sprite.Texture.Width
        let stride_y = float32 sprite.GridHeight / float32 sprite.Texture.Height

        let origin_x = float32 sprite.X / float32 sprite.Texture.Width
        let origin_y = float32 sprite.X / float32 sprite.Texture.Height

        let quad = Rect.FromSize(origin_x, origin_y, stride_x, stride_y).AsQuad
        sprite.PrecomputedQuad <- ValueSome quad

        sprite

    let upload_many
        (label: string)
        (use_texture_unit: bool)
        (smoothing: SpriteSmoothing)
        (images: SpriteUpload array)
        : Texture * (string * Sprite) array =

        let width =
            if images.Length = 0 then 1 else
                (images |> Array.map _.Image.Width |> Array.max) +
                (if smoothing = LinearSamplingFixEdges then 2 else 0)

        let height =
            if images.Length = 0 then 1 else
                (images |> Array.map _.Image.Height |> Array.max) +
                (if smoothing = LinearSamplingFixEdges then 2 else 0)

        let layers = images.Length + 1

        let texture = Texture.create (width, height, layers)

        if Texture.TRACE then Logging.Debug "Texture %i created by %s" texture.Handle label

        if use_texture_unit then
            Texture.claim_texture_unit texture |> ignore
        // texture is currently bound since it was just created

        // Init array texture with transparency on all layers, plus white pixel in top left of layer 0 using any array texture as a "blank"
        GL.TexImage3D(
            TextureTarget.Texture2DArray,
            0,
            PixelInternalFormat.Rgba,
            width,
            height,
            layers,
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

        let mutable layer = 1
        let mutable pixel_data = System.Span<Rgba32>.Empty

        for image in images do

            let image_data =
                if smoothing = LinearSamplingFixEdges then
                    fix_frills image.Image
                    extend_image_boundary image.Image
                else
                    image.Image

            let success = image_data.TryGetSinglePixelSpan(&pixel_data)

            if not success then
                Logging.Critical "Couldn't get pixel span for image!"

            GL.TexSubImage3D<Rgba32>(
                TextureTarget.Texture2DArray,
                0,
                0,
                0,
                layer,
                image_data.Width,
                image_data.Height,
                1,
                PixelFormat.Rgba,
                PixelType.UnsignedByte,
                pixel_data.ToArray()
            )

            layer <- layer + 1

            if smoothing = LinearSamplingFixEdges then
                image_data.Dispose()

        // Finish the texture by setting wrap and interpolation behaviours

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

        if smoothing <> NoSampling then
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
        else
            GL.TexParameter(
                TextureTarget.Texture2DArray,
                TextureParameterName.TextureMinFilter,
                int TextureMinFilter.Nearest
            )

            GL.TexParameter(
                TextureTarget.Texture2DArray,
                TextureParameterName.TextureMagFilter,
                int TextureMagFilter.Nearest
            )

        let mutable layer = 1

        let gen_sprite (info: SpriteUpload) : string * Sprite =

            let sprite =
                Texture.create_sprite
                    (if smoothing = LinearSamplingFixEdges then (1, 1) else (0, 0))
                    layer
                    (info.Image.Width, info.Image.Height)
                    (info.Rows, info.Columns)
                    texture

            let sprite =
                if info.Rows = 1 && info.Columns = 1 then
                    precompute_1x1 sprite
                else
                    sprite

            if info.DisposeImageAfter then
                info.Image.Dispose()

            layer <- layer + 1

            info.Label, sprite

        texture, images |> Array.map gen_sprite

    let upload_one (use_texture_unit: bool) (smoothing: SpriteSmoothing) (info: SpriteUpload) : Sprite =
        let _, results = upload_many info.Label use_texture_unit smoothing [| info |]
        snd results.[0]

    let destroy (sprite: Sprite) : bool =
        if sprite.Z > 0 then
            sprite.Texture.References <- sprite.Texture.References - 1

            if sprite.Texture.References = 0 then
                Texture.destroy sprite.Texture
                true
            else false
        else false

    let pick_texture (x: int, y: int) (sprite: Sprite) : QuadTexture =
        if sprite.PrecomputedQuad.IsSome then
            { Texture = sprite.Texture; Layer = sprite.Z; UV = sprite.PrecomputedQuad.Value }
        else

            let stride_x =
                float32 sprite.GridWidth / float32 sprite.Texture.Width / float32 sprite.Columns

            let stride_y =
                float32 sprite.GridHeight / float32 sprite.Texture.Height / float32 sprite.Rows

            let origin_x =
                float32 sprite.X / float32 sprite.Texture.Width
                + float32 (x % sprite.Columns) * stride_x

            let origin_y =
                float32 sprite.Y / float32 sprite.Texture.Height
                + float32 (y % sprite.Rows) * stride_y

            let quad = Rect.FromSize(origin_x, origin_y, stride_x, stride_y).AsQuad

            if sprite.Rows = 1 && sprite.Columns = 1 then
                sprite.PrecomputedQuad <- ValueSome quad

            { Texture = sprite.Texture; Layer = sprite.Z; UV = quad }

    // todo: relocate, remove or rename this, only used for Interlude's song background image
    let tiling (scale: float32, left: float32, top: float32) (sprite: Sprite) (quad: Quad) : QuadTexture =

        let width = float32 sprite.Width * scale
        let height = float32 sprite.Height * scale

        {
            Texture = sprite.Texture
            Layer = sprite.Z
            UV = Quad.map (fun v -> new Vector2((v.X - left) / width, (v.Y - top) / height)) quad
        }

    let aligned_box_x (x_origin: float32, y_origin: float32, x_offset: float32, y_offset: float32, x_scale: float32, y_mult: float32) (sprite: Sprite) : Rect =
        let width = x_scale
        let height = float32 sprite.Height / float32 sprite.Width * width * y_mult
        let left = x_origin - x_offset * width
        let top = y_origin - y_offset * height
        Rect.FromSize(left, top, width, height)

    let fill (bounds: Rect) (sprite: Sprite) : Rect =
        let scale = min (bounds.Width / float32 sprite.Width) (bounds.Height / float32 sprite.Height)
        let w, h = float32 sprite.Width * scale, float32 sprite.Height * scale
        Rect.FromSize(bounds.CenterX - 0.5f * w, bounds.CenterY - 0.5f * h, w, h)

    let fill_left (bounds: Rect) (sprite: Sprite) : Rect =
        let scale = min (bounds.Width / float32 sprite.Width) (bounds.Height / float32 sprite.Height)
        let w, h = float32 sprite.Width * scale, float32 sprite.Height * scale
        Rect.FromSize(bounds.Left, bounds.CenterY - 0.5f * h, w, h)

    let fill_right (bounds: Rect) (sprite: Sprite) : Rect =
        let scale = min (bounds.Width / float32 sprite.Width) (bounds.Height / float32 sprite.Height)
        let w, h = float32 sprite.Width * scale, float32 sprite.Height * scale
        Rect.FromSize(bounds.Right - w, bounds.CenterY - 0.5f * h, w, h)