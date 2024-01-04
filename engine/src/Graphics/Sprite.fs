﻿namespace Percyqaz.Flux.Graphics

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open OpenTK.Mathematics
open OpenTK.Graphics.OpenGL
open Percyqaz.Common

(*
    Sprites and content uploading
*)

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

    member this.Copy =
        this.Texture.References <- this.Texture.References + 1
        this

[<Struct>]
type QuadTexture =
    | NoTexture
    | Texture of Texture * layer: int * uv: Quad
    member this.Transform(func: Quad -> Quad) =
        match this with
        | NoTexture -> NoTexture
        | Texture(tex, layer, uv) -> Texture(tex, layer, func uv)

type SpriteUpload =
    {
        Label: string
        Rows: int
        Columns: int
        Image: Image<Rgba32>
        DisposeImageAfter: bool
    }
    static member OfImage(label: string, img: Image<Rgba32>) =
        {
            Label = label
            Rows = 1
            Columns = 1
            Image = img
            DisposeImageAfter = true
        }

module Texture =

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
                        Logging.Debug(sprintf "Texture unit claim failed, all texture units are full")
                        0
                    | Some i ->
                        texture_unit_handles.[i] <- texture.Handle
                        texture_unit_in_use.[i] <- true

                        GL.ActiveTexture(int TextureUnit.Texture0 + i |> enum)
                        GL.BindTexture(TextureTarget.Texture2DArray, texture.Handle)
                        GL.ActiveTexture(TextureUnit.Texture0)

                        //Logging.Debug(sprintf "Texture has handle %i, index %i" texture.Handle i)
                        i

            texture.TextureUnit <- texture_unit
            texture.TextureUnit <> 0

    let unclaim_texture_unit (texture: Texture) =
        if texture.TextureUnit <> 0 then
            texture_unit_in_use.[texture.TextureUnit] <- false
            texture.TextureUnit <- 0

    let destroy (texture: Texture) =
        assert (texture.References = 0)
        unclaim_texture_unit texture
        GL.DeleteTexture texture.Handle
    //Logging.Debug(sprintf "Destroyed texture with handle %i" texture.Handle)

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

    let create_sprite (x: int, y: int) (layer: int) (w: int, h: int) (rows: int, columns: int) (texture: Texture) =
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

    let create_default_sprite (texture: Texture) =
        {
            Texture = texture

            X = 0
            Y = 0
            Z = 0

            GridWidth = 1
            GridHeight = 1

            Rows = 1
            Columns = 1

            PrecomputedQuad = ValueSome Rect.ZERO.AsQuad
        }


module Sprite =

    let precompute_1x1 (sprite: Sprite) =
        let stride_x = float32 sprite.GridWidth / float32 sprite.Texture.Width
        let stride_y = float32 sprite.GridHeight / float32 sprite.Texture.Height

        let origin_x = float32 sprite.X / float32 sprite.Texture.Width
        let origin_y = float32 sprite.X / float32 sprite.Texture.Height

        let quad = Rect.Box(origin_x, origin_y, stride_x, stride_y).AsQuad
        sprite.PrecomputedQuad <- ValueSome quad

        sprite

    let upload_many
        (label: string)
        (use_texture_unit: bool)
        (use_smoothing: bool)
        (images: SpriteUpload array)
        : Texture * (string * Sprite) array =

        let width = images |> Array.map _.Image.Width |> Array.max
        let height = images |> Array.map _.Image.Height |> Array.max
        let layers = images.Length + 1

        let texture = Texture.create (width, height, layers)

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

            let success = image.Image.TryGetSinglePixelSpan(&pixel_data)

            if not success then
                Logging.Critical "Couldn't get pixel span for image!"

            GL.TexSubImage3D<Rgba32>(
                TextureTarget.Texture2DArray,
                0,
                0,
                0,
                layer,
                image.Image.Width,
                image.Image.Height,
                1,
                PixelFormat.Rgba,
                PixelType.UnsignedByte,
                pixel_data.ToArray()
            )

            layer <- layer + 1

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

        if use_smoothing then
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

        let gen_sprite (info: SpriteUpload) =

            let sprite =
                Texture.create_sprite
                    (0, 0)
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

    let upload_one (use_texture_unit: bool) (use_smoothing: bool) (info: SpriteUpload) : Sprite =
        let _, results = upload_many info.Label use_texture_unit use_smoothing [| info |]
        snd results.[0]

    let destroy (sprite: Sprite) =
        if sprite.Z > 0 then
            sprite.Texture.References <- sprite.Texture.References - 1

            if sprite.Texture.References = 0 then
                Texture.destroy sprite.Texture

    let pick_texture (x: int, y: int) (sprite: Sprite) : QuadTexture =
        if sprite.PrecomputedQuad.IsSome then
            Texture(sprite.Texture, sprite.Z, sprite.PrecomputedQuad.Value)
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

            let quad = Rect.Box(origin_x, origin_y, stride_x, stride_y).AsQuad

            if sprite.Rows = 1 && sprite.Columns = 1 then
                sprite.PrecomputedQuad <- ValueSome quad

            Texture(sprite.Texture, sprite.Z, quad)

    // todo: relocate, remove or rename this, only used for Interlude's song background image
    let tiling (scale, left, top) (sprite: Sprite) (quad: Quad) : QuadTexture =

        let width = float32 sprite.GridWidth * scale
        let height = float32 sprite.GridHeight * scale

        Texture(
            sprite.Texture,
            sprite.Z,
            Quad.map (fun v -> new Vector2((v.X - left) / width, (v.Y - top) / height)) quad
        )

    let aligned_box_x (x_origin, y_origin, x_offset, y_offset, x_scale, y_mult) (sprite: Sprite) : Rect =
        let width = x_scale
        let height = float32 sprite.GridHeight / float32 sprite.GridWidth * width * y_mult
        let left = x_origin - x_offset * width
        let top = y_origin - y_offset * height
        Rect.Box(left, top, width, height)
