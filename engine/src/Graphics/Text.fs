namespace Percyqaz.Flux.Graphics

open System
open SixLabors.Fonts
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Flux.Utils
open Percyqaz.Flux.Icons

module Fonts =

    open System.IO

    let SCALE = 80f

    type private GlyphInfo =
        {
            Code: int32
            Size: FontRectangle
            Offset: float32
        }

        member this.Width = this.Size.Width
        member this.Height = this.Size.Height

    [<AllowNullLiteral>]
    type SpriteFont(font: Font, fallbacks: FontFamily list) =
        let char_lookup = new Dictionary<int32, Sprite>()

        let render_options =
            new RendererOptions(font, ApplyKerning = false, FallbackFontFamilies = fallbacks)

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

        let render_char (c: int32) =
            let s = code_to_string c
            let size = TextMeasurer.Measure(s, render_options)
            use img = new Bitmap(max 1 (int size.Width), max 1 (int size.Height))

            try
                img.Mutate<PixelFormats.Rgba32>(fun img ->
                    img.DrawText(draw_options, s, font, SixLabors.ImageSharp.Color.White, new PointF(0f, 0f))
                    |> ignore
                )
            with err ->
                Logging.Warn(sprintf "Exception occurred rendering glyph with code point %i" (int c))

            char_lookup.Add(
                c,
                Sprite.upload_one
                    false
                    true
                    {
                        Label = "LOOSE_CHAR"
                        Rows = 1
                        Columns = 1
                        Image = img
                        DisposeImageAfter = true
                    }
            )

        // render a font texture atlas, containing common characters + icons
        // characters outside this set are dynamically generated on use
        let render_atlas () =
            let row_spacing = SCALE * 1.6f

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
                |> Seq.chunkBySize 30
                |> Seq.map (String)

            let glyphs = Seq.map row_glyph_info rows |> List.ofSeq

            let h = float32 glyphs.Length * row_spacing |> int

            let w =
                glyphs
                |> List.map (fun x -> let l = List.last x in l.Offset + l.Size.Width + 2.0f)
                |> List.max
                |> int

            if int w > Texture.MAX_TEXTURE_SIZE then
                Logging.Critical(
                    sprintf "Font atlas width of %i exceeds max texture size of %i!" w Texture.MAX_TEXTURE_SIZE
                )

            use img = new Bitmap(w, h)

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

            let atlas_sprite =
                Sprite.upload_one
                    true
                    true
                    {
                        Label = "FONT_ATLAS"
                        Rows = 1
                        Columns = 1
                        Image = img
                        DisposeImageAfter = true
                    }

            for i, row in List.indexed glyphs do
                for glyph in row do
                    let sprite =
                        Texture.create_sprite
                            (glyph.Offset |> int, row_spacing * float32 i |> int)
                            1
                            (int glyph.Width, int glyph.Height)
                            (1, 1)
                            atlas_sprite.Texture

                    char_lookup.Add(
                        glyph.Code,
                        { sprite with
                            PrecomputedQuad =
                                ValueSome(
                                    Rect
                                        .Box(
                                            glyph.Offset / float32 w,
                                            (row_spacing * float32 i) / float32 h,
                                            glyph.Width / float32 w,
                                            glyph.Height / float32 h
                                        )
                                        .AsQuad
                                )
                        }
                    )

        do render_atlas ()

        member this.Char(c: int32) =
            if not <| char_lookup.ContainsKey c then
                render_char c

            char_lookup.[c]

        member this.Dispose() =
            char_lookup.Values |> Seq.iter (Sprite.destroy >> ignore)

        member val CharSpacing = -0.04f with get, set
        member val SpaceWidth = 0.25f with get, set
        member val ShadowDepth = 0.09f with get, set


    let collection = new FontCollection()

    let add (stream: Stream) = collection.Install stream |> ignore

    let create (name: string) =
        let found, family = collection.TryFind name

        let family =
            if found then
                family
            else
                Logging.Error(sprintf "Couldn't find font '%s', defaulting" name)
                collection.Find "Inconsolata"

        let font = family.CreateFont(SCALE * 4.0f / 3.0f)
        new SpriteFont(font, [ collection.Find "feather" ])

    let init () =
        add (get_resource_stream "feather.ttf")
        add (get_resource_stream "Inconsolata.ttf")

(*
    Font rendering
*)

open Fonts

module Text =

    let measure (font: SpriteFont, text: string) : float32 =
        let mutable width = -font.CharSpacing
        let mutable high_surrogate = ' '

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

                let s = font.Char code
                width <- width + (float32 s.GridWidth) / SCALE + font.CharSpacing

        width

    let draw_b (font: SpriteFont, text: string, scale, x, y, (fg: Drawing.Color, bg: Drawing.Color)) =
        let scale2 = scale / SCALE
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

                let s = font.Char code
                let w = float32 s.Width * scale2
                let h = float32 s.Height * scale2
                let r = Rect.Box(x, y, w, h)

                if (bg: Drawing.Color).A <> 0uy then
                    Draw.quad
                        ((r.Translate(shadow_spacing, shadow_spacing)).AsQuad)
                        (Quad.color bg)
                        (Texture(s.Texture, s.Z, s.PrecomputedQuad.Value))

                Draw.quad r.AsQuad (Quad.color fg) (Texture(s.Texture, s.Z, s.PrecomputedQuad.Value))
                x <- x + w + font.CharSpacing * scale

    let draw (font, text, scale, x, y, color) =
        draw_b (font, text, scale, x, y, (color, Drawing.Color.Transparent))

    let draw_aligned (font: SpriteFont, text, scale, x, y, color, just: float32) =
        draw (font, text, scale, x - measure (font, text) * scale * just, y, color)

    let draw_aligned_b (font: SpriteFont, text, scale, x, y, color, just: float32) =
        draw_b (font, text, scale, x - measure (font, text) * scale * just, y, color)

    let fill_b (font: SpriteFont, text: string, bounds: Rect, colors: Drawing.Color * Drawing.Color, just: float32) =
        let w = measure (font, text)
        let scale = Math.Min(bounds.Height * 0.6f, (bounds.Width / w))

        let x =
            (1.0f - just) * (bounds.Left + scale * w * 0.5f)
            + just * (bounds.Right - scale * w * 0.5f)
            - w * scale * 0.5f

        draw_b (font, text, scale, x, bounds.CenterY - scale * 0.75f, colors)

    let fill (font, text, bounds, color, just) =
        fill_b (font, text, bounds, (color, Drawing.Color.Transparent), just)
