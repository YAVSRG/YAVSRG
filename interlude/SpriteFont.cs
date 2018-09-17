using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using YAVSRG.Interface;

namespace YAVSRG
{
    public class SpriteFont
    {
        //todo: comment everything and change to bounds system
        Dictionary<char, Sprite> FontLookup;
        int FONTSCALE;
        Font Font;

        public SpriteFont(int scale, string f)
        {
            FONTSCALE = scale;
            Font = new Font(f, scale);
            FontLookup = new Dictionary<char, Sprite>();

            foreach (char c in @"qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890!£$%^&*()-=_+[]{};:'@#~,.<>/?¬`\|")
            {
                GenChar(c);
            }
        }

        public float DrawText(string text, float scale, float x, float y, Color color)
        {
            float start = x;
            scale /= FONTSCALE;
            Sprite s;
            foreach (char c in text)
            {
                if (c == ' ') { x += FONTSCALE * 0.75f * scale; continue; }
                if (!FontLookup.ContainsKey(c)) { GenChar(c); }
                s = FontLookup[c];
                SpriteBatch.Draw(sprite: s, bounds: new Rect(x, y, x + s.Width * scale, y + s.Height * scale), color: color);
                x += (s.Width - FONTSCALE * 0.5f) * scale; //kerning
            }
            return x - start;
        }

        public float DrawCentredText(string text, float scale, float x, float y, Color c)
        {
            x -= scale / FONTSCALE * 0.5f * MeasureText(text);
            return DrawText(text, scale, x, y, c);
        }

        public float DrawJustifiedText(string text, float scale, float x, float y, Color c)
        {
            x -= scale / FONTSCALE * MeasureText(text);
            return DrawText(text, scale, x, y, c);
        }

        public void DrawCentredTextToFill(string text, Rect bounds, Color c)
        {
            float w = MeasureText(text);
            int h = FontLookup['T'].Height;
            float scale = Math.Min(
                bounds.Width / w,
                bounds.Height / h
                );
            DrawCentredText(text, scale * FONTSCALE, bounds.CenterX, bounds.CenterY - h * scale * 0.5f, c);
        }

        public void DrawTextToFill(string text, Rect bounds, Color c)
        {
            float w = MeasureText(text);
            int h = FontLookup['T'].Height;
            float scale = Math.Min(
                bounds.Width / w,
                bounds.Height / h
                );
            DrawText(text, scale * FONTSCALE, bounds.Left, bounds.CenterY - h * scale * 0.5f, c);
        }

        public void DrawJustifiedTextToFill(string text, Rect bounds, Color c)
        {
            float w = MeasureText(text);
            int h = FontLookup['T'].Height;
            float scale = Math.Min(
                bounds.Width / w,
                bounds.Height / h
                );
            DrawJustifiedText(text, scale * FONTSCALE, bounds.Right, bounds.CenterY - h * scale * 0.5f, c);
        }

        public float DrawDynamicText(string text, Rect bounds, Color c, AnchorType position, float size)
        {
            switch (position)
            {
                case (AnchorType.CENTER):
                    return DrawCentredText(text, size, bounds.CenterX, bounds.Top, c);
                case (AnchorType.MAX):
                    return DrawJustifiedText(text, size, bounds.Right, bounds.Top, c);
                default:
                    return DrawText(text, size, bounds.Left, bounds.Top, c);
            }
        }

        public void DrawDynamicTextToFill(string text, Rect bounds, Color c, AnchorType position)
        {
            switch (position)
            {
                case (AnchorType.CENTER):
                    DrawCentredTextToFill(text, bounds, c); return;
                case (AnchorType.MAX):
                    DrawJustifiedTextToFill(text, bounds, c); return;
                default:
                    DrawTextToFill(text, bounds, c); return;
            }
        }

        public void DrawParagraph(string text, float scale, Rect bounds, Color c)
        {
            string[] lines = text.Split('\n');
            float x = bounds.Left;
            float y = bounds.Top;
            float h = FontLookup['T'].Height * scale / FONTSCALE;
            foreach (string s in lines)
            {
                string[] split = s.Split(' ');
                foreach (string word in split)
                {
                    float w = MeasureText(word) * scale / FONTSCALE;
                    if (x + w > bounds.Right)
                    {
                        x = bounds.Left;
                        y += h;
                    }
                    DrawText(word, scale, x, y, c);
                    x += w;
                }
                x = bounds.Left;
                y += h;
            }
        }

        private float MeasureText(string text)
        {
            if (text.Length == 0) return 0;
            float w = FONTSCALE / 2;
            foreach (char c in text)
            {
                if (c == ' ') { w += FONTSCALE * 0.75f; continue; }
                if (!FontLookup.ContainsKey(c)) { w += FontLookup['?'].Width; }
                else { w += FontLookup[c].Width; }
                w -= FONTSCALE / 2;
            }
            return w;
        }

        public float MeasureText(string text, float scale)
        {
            return MeasureText(text) * scale / FONTSCALE;
        }

        private void GenChar(char c)
        {
            SizeF size;
            using (var b = new Bitmap(1, 1))
            {
                using (var g = Graphics.FromImage(b))
                {
                    size = g.MeasureString(c.ToString(), Font);
                }
            }
            var bmp = new Bitmap((int)size.Width, (int)size.Height);
            using (var g = Graphics.FromImage(bmp))
            {
                g.TextRenderingHint = System.Drawing.Text.TextRenderingHint.AntiAlias;
                g.DrawString(c.ToString(), Font, Brushes.White, 0, 0);
            }
            FontLookup.Add(c, Content.UploadTexture(bmp, 1, 1, true));
        }
    }
}
