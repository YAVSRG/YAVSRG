namespace Percyqaz.Flux.Graphics

open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open OpenTK.Mathematics
open OpenTK.Graphics.OpenGL
open Percyqaz.Common

(*
    Sprites and content uploading
*)
[<CustomEquality; CustomComparison>]
type Sprite =
    {
        ID: int
        TextureUnit: int

        Left: int
        Top: int
        AtlasWidth: int
        AtlasHeight: int
        GridWidth: int
        GridHeight: int

        Rows: int
        Columns: int

        mutable PrecomputedQuad: Quad voption
        mutable DefaultQuad: Quad option
    }
    member this.Width = this.GridWidth / this.Columns
    member this.Height = this.GridHeight / this.Rows
    member this.AspectRatio = float32 this.Width / float32 this.Height

    override x.Equals(yobj) =
        match yobj with
        | :? Sprite as y -> x.ID = y.ID
        | _ -> false

    override x.GetHashCode() = hash x.ID

    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Sprite as y -> compare x.ID y.ID
            | _ -> invalidArg "yobj" "cannot compare values of different types"

and TexturedQuad = (struct (Sprite * Quad))

module Sprite =

    let MAX_TEXTURE_UNITS = GL.GetInteger GetPName.MaxTextureImageUnits
    let TOTAL_TEXTURE_UNITS = GL.GetInteger GetPName.MaxCombinedTextureImageUnits
    let MAX_TEXTURE_SIZE = GL.GetInteger GetPName.MaxTextureSize

    // texture unit 0 is reserved for binding uncached sprites
    let private texture_unit_handles: int array = Array.zeroCreate MAX_TEXTURE_UNITS
    let private texture_unit_in_use: bool array = Array.zeroCreate MAX_TEXTURE_UNITS

    let upload (image: Image<Rgba32>, rows, columns, smooth) : Sprite =
        let id = GL.GenTexture()

        let width = image.Width
        let height = image.Height

        let mutable data = System.Span<Rgba32>.Empty
        let success = image.TryGetSinglePixelSpan(&data)

        if not success then
            Logging.Critical "Couldn't get pixel span for image!"

        GL.BindTexture(TextureTarget.Texture2D, id)

        GL.TexImage2D<Rgba32>(
            TextureTarget.Texture2D,
            0,
            PixelInternalFormat.Rgba,
            width,
            height,
            0,
            PixelFormat.Rgba,
            PixelType.UnsignedByte,
            data.ToArray()
        )

        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)

        if smooth then
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        else
            GL.TexParameter(
                TextureTarget.Texture2D,
                TextureParameterName.TextureMinFilter,
                int TextureMinFilter.Nearest
            )

            GL.TexParameter(
                TextureTarget.Texture2D,
                TextureParameterName.TextureMagFilter,
                int TextureMagFilter.Nearest
            )

        {
            ID = id
            TextureUnit = 0

            Left = 0
            Top = 0
            AtlasWidth = width
            AtlasHeight = height
            GridWidth = width
            GridHeight = height

            Rows = rows
            Columns = columns

            PrecomputedQuad = ValueNone
            DefaultQuad = None
        }

    let cache (source: string) (low_priority: bool) (sprite: Sprite) : Sprite =
        if low_priority && MAX_TEXTURE_UNITS = 16 then
            sprite
        else

            seq { 1 .. (MAX_TEXTURE_UNITS - 1) }
            |> Seq.tryFind (fun i -> not texture_unit_in_use.[i])
            |> function
                | None ->
                    Logging.Debug(sprintf "Can't cache '%s', all texture units are full" source)
                    sprite
                | Some i ->
                    texture_unit_handles.[i] <- sprite.ID
                    texture_unit_in_use.[i] <- true

                    GL.ActiveTexture(int TextureUnit.Texture0 + i |> enum)
                    GL.BindTexture(TextureTarget.Texture2D, sprite.ID)
                    GL.ActiveTexture(TextureUnit.Texture0)

                    //Logging.Debug(sprintf "Cached sprite (%s) with ID %i to index %i" source sprite.ID i)
                    { sprite with TextureUnit = i }

    let DEFAULT =
        use img = new Image<Rgba32>(1, 1)
        img.[0, 0] <- new Rgba32(255uy, 255uy, 255uy, 255uy)
        upload (img, 1, 1, false) |> cache "BLANK" false

    let DEFAULT_QUAD: TexturedQuad = struct (DEFAULT, Rect.ONE.AsQuad)

    let destroy (sprite: Sprite) =
        if sprite.ID <> DEFAULT.ID then
            texture_unit_in_use.[sprite.TextureUnit] <- false
            GL.DeleteTexture sprite.ID

    let precompute_1x1 (sprite: Sprite) =
        let stride_x = float32 sprite.GridWidth / float32 sprite.AtlasWidth
        let stride_y = float32 sprite.GridHeight / float32 sprite.AtlasHeight

        let origin_x = float32 sprite.Left / float32 sprite.AtlasWidth
        let origin_y = float32 sprite.Top / float32 sprite.AtlasHeight

        let quad = Rect.Box(origin_x, origin_y, stride_x, stride_y).AsQuad
        sprite.PrecomputedQuad <- ValueSome quad

        sprite

    let with_default_quad_alt (quad: Quad) (sprite: Sprite) = { sprite with DefaultQuad = Some quad }

    let pick_texture (x: int, y: int) (sprite: Sprite) : TexturedQuad =
        if sprite.PrecomputedQuad.IsSome then
            struct (sprite, sprite.PrecomputedQuad.Value)
        else

            let stride_x =
                float32 sprite.GridWidth / float32 sprite.AtlasWidth / float32 sprite.Columns

            let stride_y =
                float32 sprite.GridHeight / float32 sprite.AtlasHeight / float32 sprite.Rows

            let origin_x =
                float32 sprite.Left / float32 sprite.AtlasWidth + float32 x * stride_x

            let origin_y =
                float32 sprite.Top / float32 sprite.AtlasHeight + float32 y * stride_y

            let quad = Rect.Box(origin_x, origin_y, stride_x, stride_y).AsQuad

            if sprite.Rows = 1 && sprite.Columns = 1 then
                sprite.PrecomputedQuad <- ValueSome quad

            struct (sprite, quad)

    let tiling (scale, left, top) (sprite: Sprite) (quad: Quad) : TexturedQuad =
        assert (sprite.GridHeight = sprite.AtlasHeight && sprite.GridWidth = sprite.AtlasWidth)

        let width = float32 sprite.GridWidth * scale
        let height = float32 sprite.GridHeight * scale
        struct (sprite, Quad.map (fun v -> new Vector2((v.X - left) / width, (v.Y - top) / height)) quad)

    let aligned_box_x (x_origin, y_origin, x_offset, y_offset, x_scale, y_mult) (sprite: Sprite) : Rect =
        let width = x_scale
        let height = float32 sprite.GridHeight / float32 sprite.GridWidth * width * y_mult
        let left = x_origin - x_offset * width
        let top = y_origin - y_offset * height
        Rect.Box(left, top, width, height)
