namespace Interlude.Graphics

open System
open System.Drawing
open System.Collections.Generic

(*
    Font rendering
*)

module Text =
    
    open System.Drawing.Text

    let private fontscale = 100.0f
    let private spacing = 0.25f
    let private shadow = 0.09f

    [<AllowNullLiteral>]
    type SpriteFont(font: Font) =
        let fontLookup = new Dictionary<char, SpriteQuad>()
        let genChar(c: char) =
            let size =
                use b = new Bitmap(1, 1)
                use g = Graphics.FromImage(b)
                g.MeasureString(c.ToString(), font)
            let bmp = new Bitmap(int size.Width, int size.Height)
            let _ =
                use g = Graphics.FromImage(bmp)
                g.TextRenderingHint <- TextRenderingHint.AntiAlias
                g.DrawString(c.ToString(), font, Brushes.White, 0.f, 0.f)
            fontLookup.Add(c, Sprite.upload (bmp, 1, 1, true) |> Sprite.gridUV (0, 0))

        let genAtlas() =
            let mutable w = 0.0f
            let chars =
                seq {
                    for c in "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890!£$%^&*()-=_+[]{};:'@#~,.<>/?¬`\\|\"\r\n⭐♬∞⌛•⮜" do
                        use b = new Bitmap(1, 1)
                        use g = Graphics.FromImage(b)
                        let s = g.MeasureString(c.ToString(), font)
                        w <- w + s.Width
                        yield  (c, s.Width, s.Height, w - s.Width)
                } |> List.ofSeq

            let h = List.map (fun (_, _, h, _) -> h) chars |> List.max

            let bmp = new Bitmap(int w, int h)
            let _ =
                use g = Graphics.FromImage(bmp)
                g.TextRenderingHint <- TextRenderingHint.AntiAlias
                for (c, _, _, x) in chars do
                    g.DrawString(c.ToString(), font, Brushes.White, x, 0.0f)
            let sprite = Sprite.upload (bmp, 1, 1, true) |> Sprite.cache "FONT"
            for (c, width, height, x) in chars do
                fontLookup.Add(c, struct ({ sprite with Height = int height; Width = int width }, (Rect.createWH (x / w) 0.0f (width / w) (height / h) |> Quad.ofRect)))

        do genAtlas()
        member this.Char(c) =
            if not <| fontLookup.ContainsKey(c) then genChar(c)
            fontLookup.[c]
        member this.Dispose() =
            fontLookup.Values
            |> Seq.iter (fun struct (s, _) -> Sprite.destroy s)

    let measure (font: SpriteFont, text: string) : float32 =
        text |> Seq.fold (fun v c -> v + (c |> function | ' ' -> spacing | c -> -0.5f + float32 (let struct (s, _) = font.Char(c) in s.Width) / fontscale)) 0.5f

    let drawB (font: SpriteFont, text: string, scale, x, y, (fg, bg)) =
        let mutable x = x
        let scale2 = scale / fontscale
        let shadowAdjust = shadow * scale
        text
        |> Seq.iter (
            function
            | ' ' -> x <- x + spacing * scale
            | c -> 
                let struct (s, q) = font.Char(c)
                let w = float32 s.Width * scale2
                let h = float32 s.Height * scale2
                let r = Rect.create x y (x + w) (y + h)
                if (bg: Color).A <> 0uy then
                    Draw.quad (Quad.ofRect (Rect.translate(shadowAdjust, shadowAdjust) r)) (Quad.colorOf bg) struct (s, q)
                Draw.quad (Quad.ofRect r) (Quad.colorOf fg) struct (s, q)
                x <- x + w - 0.5f * scale)
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

    let pfc = new PrivateFontCollection()
    let createFont (str: string) =
        let f =
            if str.Contains('.') then
                //targeting a specific file
                try
                    pfc.AddFontFile(str)
                    new Font(pfc.Families.[0], fontscale * 4.0f / 3.0f, GraphicsUnit.Pixel)
                with
                | err ->
                    Prelude.Common.Logging.Error("Failed to load font file: " + str, err)
                    new Font(str, fontscale * 4.0f / 3.0f, GraphicsUnit.Pixel)
            else
                new Font(str, fontscale * 4.0f / 3.0f, GraphicsUnit.Pixel)
        new SpriteFont(f)