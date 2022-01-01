namespace Interlude.Graphics

open System
open SixLabors.Fonts
open SixLabors.ImageSharp
open System.Collections.Generic
open Prelude.Common
open SharpFont

module Fonts =

    open System.IO

    let SCALE = 100f

    type private GlyphInfo =
        {
            Char: char
            Width: float32
            Height: float32
            Offset: float32
        }
    
    [<AllowNullLiteral>]
    type SpriteFont(face: Face) =
        let fontLookup = new Dictionary<char, SpriteQuad>()

        let genChar(c: char) =
            face.LoadChar(uint c, LoadFlags.Render, LoadTarget.Normal)
            let width = face.Glyph.Bitmap.Width
            let height = face.Glyph.Bitmap.Rows
            use img = new Bitmap(width, height)
            for x = 0 to width - 1 do
                for y = 0 to height - 1 do
                    let value = face.Glyph.Bitmap.BufferData.[y * width + x]
                    img.[x, y] <- PixelFormats.Rgba32(255uy, 255uy, 255uy, value)
            fontLookup.Add(c, Sprite.upload (img, 1, 1, true) |> Sprite.gridUV (0, 0))

        let genAtlas() =
            ()
            (*
            let mutable w = 0.0f
            let glyphs =
                seq {
                    for c in "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890!£$%^&*()-=_+[]{};:'@#~,.<>/?¬`\\|\"\r\n⭐🎵∞⌛•⬅" do
                        let size = TextMeasurer.Measure(c.ToString(), renderOptions)
                        w <- w + size.Width + 2.0f
                        yield { Char = c; Size = size; Offset = w - size.Width - 1.0f }
                } |> List.ofSeq

            let h = List.map (fun (info: GlyphInfo) -> info.Height) glyphs |> List.max

            use img = new Bitmap(int w, int h)
            for glyph in glyphs do
                img.Mutate<PixelFormats.Rgba32>(
                    fun img ->
                        img.DrawText(drawOptions, glyph.Char.ToString(), font, SixLabors.ImageSharp.Color.White, new PointF(glyph.Offset, 0f))
                        |> ignore
                )
            let sprite = Sprite.upload (img, 1, 1, true) |> Sprite.cache "FONT"
            for glyph in glyphs do
                fontLookup.Add
                    ( glyph.Char,
                        struct (
                            { sprite with Height = int glyph.Height; Width = int glyph.Width },
                            (Rect.createWH (glyph.Offset / w) 0.0f (glyph.Width / w) (glyph.Height / h) |> Quad.ofRect)
                        )
                    )*)

        do genAtlas()
        member this.Char(c) =
            if not <| fontLookup.ContainsKey c then genChar c
            fontLookup.[c]
        member this.Dispose() =
            fontLookup.Values
            |> Seq.iter (fun struct (s, _) -> Sprite.destroy s)
            
    let collection = new FontCollection()
    let library = new Library()

    let init() =
        for file in Directory.EnumerateFiles(Path.Combine(Interlude.Utils.getInterludeLocation(), "Fonts")) do
            match Path.GetExtension file with
            | ".ttf" | ".otf" ->
                collection.Install file |> ignore
            | _ -> ()
        Logging.Info (sprintf "Loaded %i font families" (Seq.length collection.Families))

    let create (name: string) =
        let face = new Face(library, Path.Combine(Interlude.Utils.getInterludeLocation(), "Fonts", "Akrobat-Black.ttf"))
        (*
        let found, family = collection.TryFind (name, CultureInfo.InvariantCulture)
        let family = 
            if found then family
            else Logging.Error (sprintf "Couldn't find font '%s', defaulting to Akrobat Black" name); collection.Find ("Akrobat Black", CultureInfo.InvariantCulture)
        let font = family.CreateFont(SCALE * 4.0f / 3.0f) *)
        face.SetPixelSizes(0u, uint (SCALE * 4.0f / 3.0f))
        new SpriteFont(face)

(*
    Font rendering
*)

open Fonts

module Text =

    let private FONTSCALE = SCALE
    let private WHITESPACE = 0.25f
    let private SPACING = 0.04f
    let private SHADOW = 0.09f

    let measure (font: SpriteFont, text: string) : float32 =
        text |> Seq.fold (fun v c -> v + (c |> function | ' ' -> WHITESPACE | c -> float32 (let struct (s, _) = font.Char(c) in s.Width) / FONTSCALE) + SPACING) -SPACING

    let drawB (font: SpriteFont, text: string, scale, x, y, (fg, bg)) =
        let mutable x = x
        let scale2 = scale / FONTSCALE
        let shadowAdjust = SHADOW * scale
        text
        |> Seq.iter (
            function
            | ' ' -> x <- x + WHITESPACE * scale
            | c -> 
                let struct (s, q) = font.Char(c)
                let w = float32 s.Width * scale2
                let h = float32 s.Height * scale2
                let r = Rect.create x y (x + w) (y + h)
                if (bg: Color).A <> 0uy then
                    Draw.quad (Quad.ofRect (Rect.translate(shadowAdjust, shadowAdjust) r)) (Quad.colorOf bg) struct (s, q)
                Draw.quad (Quad.ofRect r) (Quad.colorOf fg) struct (s, q)
                x <- x + w + SPACING * scale)
    let draw (font, text, scale, x, y, color) = drawB(font, text, scale, x, y, (color, Color.Transparent))

    let drawJust (font: SpriteFont, text, scale, x, y, color, just: float32) = draw(font, text, scale, x - measure(font, text) * scale * just, y, color)
    let drawJustB (font: SpriteFont, text, scale, x, y, color, just: float32) = drawB(font, text, scale, x - measure(font, text) * scale * just, y, color)

    let drawFillB (font: SpriteFont, text, bounds, color, just: float32) =
        let w = measure(font, text)
        let scale = Math.Min(Rect.height bounds * 0.6f, (Rect.width bounds / w))
        let struct (l, _, r, _) = bounds
        let x = (1.0f - just) * (l + scale * w * 0.5f) + just * (r - scale * w * 0.5f) - w * scale * 0.5f
        drawB(font, text, scale, x, Rect.centerY bounds - scale * 0.75f, color)
    let drawFill(font, text, bounds, color, just) = drawFillB(font, text, bounds, (color, Color.Transparent), just)