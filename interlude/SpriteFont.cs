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

        public void DrawText(string text, float scale, float x, float y, Color color)
        {
            scale /= FONTSCALE;
            Sprite s;
            foreach (char c in text)
            {
                if (c == ' ') { x += FONTSCALE * 0.75f * scale; continue; }
                if (!FontLookup.ContainsKey(c)) { GenChar(c); }
                s = FontLookup[c];
                SpriteBatch.Draw(s, x, y, x + s.Width * scale, y + s.Height * scale, color);
                x += (s.Width - FONTSCALE * 0.5f) * scale; //kerning
            }
        }

        public void DrawCentredText(string text, float scale, float x, float y, Color c)
        {
            x -= scale / FONTSCALE * 0.5f * MeasureText(text);
            DrawText(text, scale, x, y, c);
        }

        public void DrawJustifiedText(string text, float scale, float x, float y, Color c)
        {
            x -= scale / FONTSCALE * MeasureText(text);
            DrawText(text, scale, x, y, c);
        }

        public void DrawCentredTextToFill(string text, float left, float top, float right, float bottom, Color c)
        {
            float w = MeasureText(text);
            int h = FontLookup['T'].Height;
            float scale = Math.Min(
                (right - left) / w,
                (bottom - top) / h
                );
            DrawCentredText(text, scale * FONTSCALE, (left + right) * 0.5f, (top + bottom) * 0.5f - h * scale * 0.5f, c);
        }

        public void DrawTextToFill(string text, float left, float top, float right, float bottom, Color c)
        {
            float w = MeasureText(text);
            int h = FontLookup['T'].Height;
            float scale = Math.Min(
                (right - left) / w,
                (bottom - top) / h
                );
            DrawText(text, scale * FONTSCALE, left, (top + bottom) * 0.5f - h * scale * 0.5f, c);
        }

        public float MeasureText(string text)
        {
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
