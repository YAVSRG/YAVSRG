namespace Percyqaz.Flux.Graphics

open System
open System.IO
open SixLabors.Fonts
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Flux.Icons

type private GlyphInfo =
    {
        Code: int32
        Size: FontRectangle
        Offset: float32
    }

    member this.Width = this.Size.Width
    member this.Height = this.Size.Height

type SpriteFontOptions =
    {
        BaseScale: float32
        CharSpacing: float32
        SpaceWidth: float32
        ShadowDepth: float32
    }
    static member Default =
        {
            BaseScale = 30.0f
            CharSpacing = -0.04f
            SpaceWidth = 0.25f
            ShadowDepth = 0.09f
        }

type SpriteFont(font_family: FontFamily, fallbacks: FontFamily list, options: SpriteFontOptions) =

    let FONT_SM = font_family.CreateFont(options.BaseScale * 4.0f / 3.0f)
    let FONT_MD = font_family.CreateFont(options.BaseScale * 8.0f / 3.0f)
    let FONT_LG = font_family.CreateFont(options.BaseScale * 16.0f / 3.0f)

    let char_lookup_sm = new Dictionary<int32, Sprite>()
    let char_lookup_md = new Dictionary<int32, Sprite>()
    let char_lookup_lg = new Dictionary<int32, Sprite>()

    let text_options =
        let x = new TextOptions() in
        x.FallbackFonts.AddRange(fallbacks)
        x

    let draw_options =
        new DrawingOptions(
            TextOptions = text_options,
            GraphicsOptions = new GraphicsOptions(Antialias = true, AntialiasSubpixelDepth = 24)
        )

    let code_to_string (c: int32) : string = Char.ConvertFromUtf32 c

    let render_char_level (font: Font) (c: int32) : Bitmap =
        let s = code_to_string c
        let render_options = new RendererOptions(font, ApplyKerning = false, FallbackFontFamilies = fallbacks)
        let size = TextMeasurer.Measure(s, render_options)
        let img = new Bitmap(max 1 (int size.Width), max 1 (int size.Height))

        try
            img.Mutate<PixelFormats.Rgba32>(fun img ->
                img.DrawText(draw_options, s, font, Color.White, new PointF(0f, 0f))
                |> ignore
            )
        with err ->
            Logging.Warn "Exception occurred rendering glyph with code point %i: %O" (int c) err
        img

    let render_char (c: int32) : unit =
        let sm = render_char_level FONT_SM c
        let md = render_char_level FONT_MD c
        let lg = render_char_level FONT_LG c

        let texture, sprites =
            Sprite.upload_many
                "LOOSE_CHAR"
                false
                true
                [|
                    {
                        Label = "LOOSE_CHAR_SM"
                        Rows = 1
                        Columns = 1
                        Image = sm
                        DisposeImageAfter = true
                    }
                    {
                        Label = "LOOSE_CHAR_MD"
                        Rows = 1
                        Columns = 1
                        Image = md
                        DisposeImageAfter = true
                    }
                    {
                        Label = "LOOSE_CHAR_LG"
                        Rows = 1
                        Columns = 1
                        Image = lg
                        DisposeImageAfter = true
                    }
                |]

        if sprites.Length = 3 then
            char_lookup_sm.Add(c, snd sprites.[0])
            char_lookup_md.Add(c, snd sprites.[1])
            char_lookup_lg.Add(c, snd sprites.[2])
        else failwithf "Sprites for glyph with code point %i didn't upload correctly" (int c)

    let render_atlas_level (font: Font) : Bitmap * GlyphInfo list list * float32 =
        let row_spacing = font.Size * 1.5f
        let render_options = new RendererOptions(font, ApplyKerning = false, FallbackFontFamilies = fallbacks)

        let row_glyph_info chars =
            let mutable w = 0.0f
            let mutable high_surrogate: char = ' '

            seq {
                for c in chars do
                    if Char.IsHighSurrogate c then
                        high_surrogate <- c
                    else
                        let code =
                            if Char.IsLowSurrogate c then
                                Char.ConvertToUtf32(high_surrogate, c)
                            else
                                int32 c

                        let size = TextMeasurer.Measure(code_to_string code, render_options)
                        w <- w + size.Width + 2.0f

                        yield
                            {
                                Code = code
                                Size = size
                                Offset = w - size.Width - 1.0f
                            }
            }
            |> List.ofSeq

        let rows =
            "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890!\"£$%^&*()-=_+[]{};:'@#~,.<>/?¬`\\|\r\n•∞"
            + Feather.CONCAT
            |> Seq.chunkBySize 25
            |> Seq.map (String)

        let glyphs = Seq.map row_glyph_info rows |> List.ofSeq

        let h = float32 glyphs.Length * row_spacing |> int

        let w =
            glyphs
            |> List.map (fun x -> let l = List.last x in l.Offset + l.Size.Width + 2.0f)
            |> List.max
            |> int

        assert(int w <= 4096)
        if int w > Texture.MAX_TEXTURE_SIZE then
            Logging.Critical "Font atlas width of %i exceeds max texture size of %i!" w Texture.MAX_TEXTURE_SIZE
        assert(int h <= 4096)
        if int h > Texture.MAX_TEXTURE_SIZE then
            Logging.Critical "Font atlas height of %i exceeds max texture size of %i!" w Texture.MAX_TEXTURE_SIZE

        let img = new Bitmap(w, h)

        for i, row in List.indexed glyphs do
            for glyph in row do
                img.Mutate<PixelFormats.Rgba32>(fun img ->
                    img.DrawText(
                        draw_options,
                        code_to_string glyph.Code,
                        font,
                        Color.White,
                        new PointF(glyph.Offset, row_spacing * float32 i)
                    )
                    |> ignore
                )

        img.[w - 1, 0] <- new PixelFormats.Rgba32(255uy, 255uy, 255uy, 255uy)

        img, glyphs, row_spacing

    // render a font texture atlas, containing common characters + icons
    // characters outside this set are dynamically generated on use
    let render_atlas() : unit =

        let sm, sm_glyphs, sm_spacing = render_atlas_level FONT_SM
        let md, md_glyphs, md_spacing = render_atlas_level FONT_MD
        let lg, lg_glyphs, lg_spacing = render_atlas_level FONT_LG

        let atlas_texture, unused_layer_sprites =
            Sprite.upload_many
                "FONT_ATLASES"
                true
                true
                [|
                    {
                        Label = "FONT_ATLAS_SM"
                        Rows = 1
                        Columns = 1
                        Image = sm
                        DisposeImageAfter = true
                    }
                    {
                        Label = "FONT_ATLAS_MD"
                        Rows = 1
                        Columns = 1
                        Image = md
                        DisposeImageAfter = true
                    }
                    {
                        Label = "FONT_ATLAS_LG"
                        Rows = 1
                        Columns = 1
                        Image = lg
                        DisposeImageAfter = true
                    }
                |]

        for i, row in List.indexed sm_glyphs do
            for glyph in row do
                let sprite =
                    Texture.create_sprite
                        (glyph.Offset |> int, sm_spacing * float32 i |> int)
                        1
                        (int glyph.Width, int glyph.Height)
                        (1, 1)
                        atlas_texture

                char_lookup_sm.Add(
                    glyph.Code,
                    { sprite with
                        PrecomputedQuad =
                            ValueSome(
                                Rect
                                    .FromSize(
                                        glyph.Offset / float32 atlas_texture.Width,
                                        (sm_spacing * float32 i) / float32 atlas_texture.Height,
                                        glyph.Width / float32 atlas_texture.Width,
                                        glyph.Height / float32 atlas_texture.Height
                                    )
                                    .AsQuad
                            )
                    }
                )

        for i, row in List.indexed md_glyphs do
            for glyph in row do
                let sprite =
                    Texture.create_sprite
                        (glyph.Offset |> int, md_spacing * float32 i |> int)
                        2
                        (int glyph.Width, int glyph.Height)
                        (1, 1)
                        atlas_texture

                char_lookup_md.Add(
                    glyph.Code,
                    { sprite with
                        PrecomputedQuad =
                            ValueSome(
                                Rect
                                    .FromSize(
                                        glyph.Offset / float32 atlas_texture.Width,
                                        (md_spacing * float32 i) / float32 atlas_texture.Height,
                                        glyph.Width / float32 atlas_texture.Width,
                                        glyph.Height / float32 atlas_texture.Height
                                    )
                                    .AsQuad
                            )
                    }
                )

        for i, row in List.indexed lg_glyphs do
            for glyph in row do
                let sprite =
                    Texture.create_sprite
                        (glyph.Offset |> int, lg_spacing * float32 i |> int)
                        3
                        (int glyph.Width, int glyph.Height)
                        (1, 1)
                        atlas_texture

                char_lookup_lg.Add(
                    glyph.Code,
                    { sprite with
                        PrecomputedQuad =
                            ValueSome(
                                Rect
                                    .FromSize(
                                        glyph.Offset / float32 atlas_texture.Width,
                                        (lg_spacing * float32 i) / float32 atlas_texture.Height,
                                        glyph.Width / float32 atlas_texture.Width,
                                        glyph.Height / float32 atlas_texture.Height
                                    )
                                    .AsQuad
                            )
                    }
                )

        unused_layer_sprites |> Seq.map snd |> Seq.iter (Sprite.destroy >> ignore)

    do render_atlas ()

    member this.CharLookup (level: int) : int32 -> Sprite =

        let target = match level with 2 -> char_lookup_lg | 1 -> char_lookup_md | _ -> char_lookup_sm

        fun (c: int32) ->
            if not <| target.ContainsKey c then
                render_char c

            target.[c]

    member this.Dispose() =
        char_lookup_sm.Values |> Seq.iter (Sprite.destroy >> ignore)
        char_lookup_md.Values |> Seq.iter (Sprite.destroy >> ignore)
        char_lookup_lg.Values |> Seq.iter (Sprite.destroy >> ignore)

    member this.BaseScale = options.BaseScale
    member this.CharSpacing = options.CharSpacing
    member this.SpaceWidth = options.SpaceWidth
    member this.ShadowDepth = options.ShadowDepth

module Fonts =

    let collection = new FontCollection()

    let add (stream: Stream) : unit = collection.Install stream |> ignore

    let create (name: string) (options: SpriteFontOptions) : SpriteFont =
        let found, family = collection.TryFind name

        let family =
            if found then
                family
            else
                Logging.Error "Couldn't find font '%s', defaulting" name
                collection.Find "Inconsolata"

        new SpriteFont(family, [ collection.Find "feather" ], options)

    open System.Reflection

    let init () : unit =
        Assembly
            .GetCallingAssembly()
            .GetManifestResourceStream("Percyqaz.Flux.Resources.feather.ttf")
        |> add
        Assembly
            .GetCallingAssembly()
            .GetManifestResourceStream("Percyqaz.Flux.Resources.Inconsolata.ttf")
        |> add

(*
    Font rendering
*)

// todo: combine this text api with the Render api
module Text =

    let measure (font: SpriteFont, text: string) : float32 =
        let mutable width = -font.CharSpacing
        let mutable high_surrogate = ' '
        let char_lookup = font.CharLookup 0

        for char in text.AsSpan() do

            if char = ' ' then
                width <- width + font.SpaceWidth
            elif Char.IsHighSurrogate char then
                high_surrogate <- char
            else
                let code =
                    if Char.IsLowSurrogate char then
                        Char.ConvertToUtf32(high_surrogate, char)
                    else
                        int32 char

                let s = char_lookup code
                width <- width + (float32 s.GridWidth) / font.BaseScale + font.CharSpacing

        width

    let draw_b (font: SpriteFont, text: string, scale: float32, x: float32, y: float32, (fg: Drawing.Color, bg: Drawing.Color)) : unit =

        let level, scale_mult =
            let l1 = scale / font.BaseScale
            if l1 < 1.05f then
                0, l1
            elif l1 < 2.1f then
                1, l1 / 2.0f
            else
                2, l1 / 4.0f
        let char_lookup = font.CharLookup level
        let shadow_spacing = font.ShadowDepth * scale
        let mutable x = x
        let mutable high_surrogate = ' '

        for char in text.AsSpan() do

            if char = ' ' then
                x <- x + font.SpaceWidth * scale
            elif Char.IsHighSurrogate char then
                high_surrogate <- char
            else
                let code =
                    if Char.IsLowSurrogate char then
                        Char.ConvertToUtf32(high_surrogate, char)
                    else
                        int32 char

                let s = char_lookup code
                let w = float32 s.Width * scale_mult
                let h = float32 s.Height * scale_mult
                let r = Rect.FromSize(x, y, w, h)

                if (bg: Drawing.Color).A <> 0uy then
                    Render.tex_quad
                        ((r.Translate(shadow_spacing, shadow_spacing)).AsQuad)
                        bg.AsQuad
                        { Texture = s.Texture; Layer = s.Z; UV = s.PrecomputedQuad.Value }

                Render.tex_quad r.AsQuad fg.AsQuad { Texture = s.Texture; Layer = s.Z; UV = s.PrecomputedQuad.Value }
                x <- x + w + font.CharSpacing * scale

    let draw (font, text, scale, x, y, color) : unit =
        draw_b (font, text, scale, x, y, (color, Drawing.Color.Transparent))

    let draw_aligned (font: SpriteFont, text, scale, x, y, color, alignment: float32) : unit =
        draw (font, text, scale, x - measure (font, text) * scale * alignment, y, color)

    let draw_aligned_b (font: SpriteFont, text, scale, x, y, color, alignment: float32) : unit =
        draw_b (font, text, scale, x - measure (font, text) * scale * alignment, y, color)

    let fill_b (font: SpriteFont, text: string, bounds: Rect, colors: Drawing.Color * Drawing.Color, just: float32) : unit =
        let w = measure (font, text)
        let scale = Math.Min(bounds.Height * 0.6f, (bounds.Width / w))

        let x =
            (1.0f - just) * (bounds.Left + scale * w * 0.5f)
            + just * (bounds.Right - scale * w * 0.5f)
            - w * scale * 0.5f

        draw_b (font, text, scale, x, bounds.CenterY - scale * 0.75f, colors)

    let fill (font: SpriteFont, text: string, bounds: Rect, color: Drawing.Color, alignment: float32) : unit =
        fill_b (font, text, bounds, (color, Drawing.Color.Transparent), alignment)