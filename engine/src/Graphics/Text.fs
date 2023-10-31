namespace Percyqaz.Flux.Graphics

open System
open SixLabors.Fonts
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Flux.Utils
open Percyqaz.Flux.Resources

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
        let fontLookup = new Dictionary<int32, SpriteQuad>()

        let renderOptions =
            new RendererOptions(font, ApplyKerning = false, FallbackFontFamilies = fallbacks)

        let textOptions =
            let x = new TextOptions() in
            x.FallbackFonts.AddRange(fallbacks)
            x

        let drawOptions =
            new DrawingOptions(
                TextOptions = textOptions,
                GraphicsOptions = new GraphicsOptions(Antialias = true, AntialiasSubpixelDepth = 24)
            )

        let codepointToString (c: int32) : string = Char.ConvertFromUtf32 c

        let genChar (c: int32) =
            let s = codepointToString c
            let size = TextMeasurer.Measure(s, renderOptions)
            use img = new Bitmap(max 1 (int size.Width), max 1 (int size.Height))

            try
                img.Mutate<PixelFormats.Rgba32>(fun img ->
                    img.DrawText(drawOptions, s, font, SixLabors.ImageSharp.Color.White, new PointF(0f, 0f))
                    |> ignore
                )
            with err ->
                Logging.Error(sprintf "Exception occurred rendering glyph with code point %i" (int c), err)

            fontLookup.Add(c, Sprite.upload (img, 1, 1, true) |> Sprite.with_uv (0, 0))

        let genAtlas () =
            let rowspacing = SCALE * 1.6f

            let getRowGlyphs chars =
                let mutable w = 0.0f
                let mutable highSurrogate: char = ' '

                seq {
                    for c in chars do
                        if Char.IsHighSurrogate c then
                            highSurrogate <- c
                        else
                            let code =
                                if Char.IsLowSurrogate c then
                                    Char.ConvertToUtf32(highSurrogate, c)
                                else
                                    int32 c

                            let size = TextMeasurer.Measure(codepointToString code, renderOptions)
                            w <- w + size.Width + 2.0f

                            yield
                                {
                                    Code = code
                                    Size = size
                                    Offset = w - size.Width - 1.0f
                                }
                }
                |> List.ofSeq

            let chunks =
                "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890!\"£$%^&*()-=_+[]{};:'@#~,.<>/?¬`\\|\r\n•∞"
                + Feather.CONCAT
                |> Seq.chunkBySize 30
                |> Seq.map (String)

            let glyphs = Seq.map getRowGlyphs chunks |> List.ofSeq

            let h = float32 glyphs.Length * rowspacing

            let w =
                glyphs
                |> List.map (fun x -> let l = List.last x in l.Offset + l.Size.Width + 2.0f)
                |> List.max

            if int w > Sprite.MAX_TEXTURE_SIZE then
                Logging.Critical(
                    sprintf "Font atlas width of %f exceeds max texture size of %i!" w Sprite.MAX_TEXTURE_SIZE
                )

            use img = new Bitmap(int w, int h)

            for i, row in List.indexed glyphs do
                for glyph in row do
                    img.Mutate<PixelFormats.Rgba32>(fun img ->
                        img.DrawText(
                            drawOptions,
                            codepointToString glyph.Code,
                            font,
                            SixLabors.ImageSharp.Color.White,
                            new PointF(glyph.Offset, rowspacing * float32 i)
                        )
                        |> ignore
                    )

            let sprite = Sprite.upload (img, 1, 1, true) |> Sprite.cache "FONT" false

            for i, row in List.indexed glyphs do
                for glyph in row do
                    fontLookup.Add(
                        glyph.Code,
                        struct ({ sprite with
                                    Height = int glyph.Height
                                    Width = int glyph.Width
                                },
                                (Rect.Box(
                                    glyph.Offset / w,
                                    (rowspacing * float32 i) / h,
                                    glyph.Width / w,
                                    glyph.Height / h
                                 )
                                 |> Quad.ofRect))
                    )

        do genAtlas ()

        member this.Char(c: int32) =
            if not <| fontLookup.ContainsKey c then
                genChar c

            fontLookup.[c]

        member this.Dispose() =
            fontLookup.Values |> Seq.iter (fun struct (s, _) -> Sprite.destroy s)

        member val CharSpacing = -0.04f with get, set
        member val SpaceWidth = 0.25f with get, set
        member val ShadowDepth = 0.09f with get, set

    let collection = new FontCollection()

    let add (stream: Stream) =
        let family = collection.Install stream in ()

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

    let private FONTSCALE = SCALE

    let measure (font: SpriteFont, text: string) : float32 =
        let mutable width = -font.CharSpacing
        let mutable highSurrogate = ' '
        let mutable i = 0

        while i < text.Length do
            let thisChar = text.[i]

            if thisChar = ' ' then
                width <- width + font.SpaceWidth
            elif Char.IsHighSurrogate thisChar then
                highSurrogate <- thisChar
            else
                let code =
                    if Char.IsLowSurrogate thisChar then
                        Char.ConvertToUtf32(highSurrogate, thisChar)
                    else
                        int32 thisChar

                let struct (s, _) = font.Char code
                width <- width + (float32 s.Width) / FONTSCALE + font.CharSpacing

            i <- i + 1

        width

    let drawB (font: SpriteFont, text: string, scale, x, y, (fg, bg)) =
        let scale2 = scale / FONTSCALE
        let shadowAdjust = font.ShadowDepth * scale
        let mutable x = x
        let mutable highSurrogate = ' '
        let mutable i = 0

        while i < text.Length do
            let thisChar = text.[i]

            if thisChar = ' ' then
                x <- x + font.SpaceWidth * scale
            elif Char.IsHighSurrogate thisChar then
                highSurrogate <- thisChar
            else
                let code =
                    if Char.IsLowSurrogate thisChar then
                        Char.ConvertToUtf32(highSurrogate, thisChar)
                    else
                        int32 thisChar

                let struct (s, q) = font.Char code
                let w = float32 s.Width * scale2
                let h = float32 s.Height * scale2
                let r = Rect.Box(x, y, w, h)

                if (bg: Color).A <> 0uy then
                    Draw.quad (Quad.ofRect (r.Translate(shadowAdjust, shadowAdjust))) (Quad.color bg) struct (s, q)

                Draw.quad (Quad.ofRect r) (Quad.color fg) struct (s, q)
                x <- x + w + font.CharSpacing * scale

            i <- i + 1

    let draw (font, text, scale, x, y, color) =
        drawB (font, text, scale, x, y, (color, Color.Transparent))

    let drawJust (font: SpriteFont, text, scale, x, y, color, just: float32) =
        draw (font, text, scale, x - measure (font, text) * scale * just, y, color)

    let drawJustB (font: SpriteFont, text, scale, x, y, color, just: float32) =
        drawB (font, text, scale, x - measure (font, text) * scale * just, y, color)

    let drawFillB (font: SpriteFont, text: string, bounds: Rect, color: Color * Color, just: float32) =
        let w = measure (font, text)
        let scale = Math.Min(bounds.Height * 0.6f, (bounds.Width / w))

        let x =
            (1.0f - just) * (bounds.Left + scale * w * 0.5f)
            + just * (bounds.Right - scale * w * 0.5f)
            - w * scale * 0.5f

        drawB (font, text, scale, x, bounds.CenterY - scale * 0.75f, color)

    let drawFill (font, text, bounds, color, just) =
        drawFillB (font, text, bounds, (color, Color.Transparent), just)
