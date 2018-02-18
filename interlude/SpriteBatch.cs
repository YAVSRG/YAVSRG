using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK;
using OpenTK.Graphics.OpenGL;
using System.Drawing;

namespace YAVSRG
{
    class SpriteBatch
    {
        private static Dictionary<char, Sprite> FontLookup;

        private static readonly int FONTSCALE = 60;

        public static void Draw(Sprite texture, float left, float top, float right, float bottom, Color color, int rotation = 0)
        {
            Draw(texture, left, top, right, bottom, new Rectangle(0, 0, 1, 1), color, rotation);
        }

        public static void Draw(Sprite texture, float left, float top, float right, float bottom, Color color, int ux, int uy, int rotation = 0)
        {
            float x = 1f / texture.UV_X;
            float y = 1f / texture.UV_Y;
            RectangleF UV = new RectangleF(x * ux, y * uy, x, y);
            Draw(texture, left, top, right, bottom, UV, color, rotation);
        }

        public static void Draw(Sprite texture, float left, float top, float right, float bottom, RectangleF uv, Color color, int rotation = 0)
        {
            GL.Enable(EnableCap.Texture2D);
            Vector2[] texcoords = new[]
            {
                new Vector2(uv.Left,uv.Top),
                new Vector2(uv.Right,uv.Top),
                new Vector2(uv.Right,uv.Bottom),
                new Vector2(uv.Left,uv.Bottom)
            };

            GL.BindTexture(TextureTarget.Texture2D, texture.ID);
            GL.Begin(PrimitiveType.Quads);

            GL.Color4(color);

            GL.TexCoord2(texcoords[rotation % 4]);
            GL.Vertex2(left, top);
            GL.TexCoord2(texcoords[(1 + rotation) % 4]);
            GL.Vertex2(right, top);
            GL.TexCoord2(texcoords[(2 + rotation) % 4]);
            GL.Vertex2(right, bottom);
            GL.TexCoord2(texcoords[(3 + rotation) % 4]);
            GL.Vertex2(left, bottom);

            GL.End();
            GL.Disable(EnableCap.Texture2D);
        }

        public static void DrawTilingTexture(Sprite texture, float left, float top, float right, float bottom, float scale, float x, float y, Color color)
        {
            RectangleF uv = new RectangleF(x+left/scale,y+top/scale,(right-left)/scale,(bottom-top)/scale);
            Draw(texture, left, top, right, bottom, uv, color);
        }

        public static void DrawFrame(Sprite texture, float left, float top, float right, float bottom, float scale, Color color)
        {
            //corners
            Draw(texture, left, top, left + scale, top + scale, color, 0, 0);
            Draw(texture, right - scale, top, right, top + scale, color, 2, 0);
            Draw(texture, left, bottom - scale, left + scale, bottom, color, 0, 2);
            Draw(texture, right - scale, bottom - scale, right, bottom, color, 2, 2);
            //edges
            Draw(texture, left + scale, top, right - scale, top + scale, color, 1, 0);
            Draw(texture, left, top + scale, left + scale, bottom - scale, color, 0, 1);
            Draw(texture, right - scale, top + scale, right, bottom - scale, color, 2, 1);
            Draw(texture, left + scale, bottom - scale, right - scale, bottom, color, 1, 2);
        }

        public static void DrawRect(float left, float top, float right, float bottom, Color color)
        {
            GL.Begin(PrimitiveType.Quads);
            GL.Color4(color);

            GL.Vertex2(left, top);
            GL.Vertex2(right, top);
            GL.Vertex2(right, bottom);
            GL.Vertex2(left, bottom);

            GL.End();
        }

        public static void DrawText(string text, float scale, float x, float y, Color color)
        {
            scale /= FONTSCALE;
            Sprite s;
            foreach (char c in text)
            {
                if (c == ' ') { x += FONTSCALE * 0.75f * scale; continue; }
                if (!FontLookup.ContainsKey(c)) { s = FontLookup['?']; }
                else { s = FontLookup[c]; }
                Draw(s, x, y, x + s.Width * scale, y + s.Height * scale, color);
                x += (s.Width - FONTSCALE * 0.5f) * scale;//kerning
            }
        }

        public static void DrawCentredText(string text, float scale, float x, float y, Color c)
        {
            x -= scale / FONTSCALE * 0.5f * MeasureText(text);
            DrawText(text, scale, x, y, c);
        }

        public static void DrawJustifiedText(string text, float scale, float x, float y, Color c)
        {
            x -= scale / FONTSCALE * MeasureText(text);
            DrawText(text, scale, x, y, c);
        }

        public static void DrawCentredTextToFill(string text, float left, float top, float right, float bottom, Color c)
        {
            float w = MeasureText(text);
            int h = FontLookup['T'].Height;
            float scale = Math.Min(
                (right - left) / w,
                (bottom - top) / h
                );
            DrawCentredText(text, scale * FONTSCALE, (left + right) * 0.5f, (top + bottom) * 0.5f - h * scale * 0.5f, c);
        }

        public static void DrawTextToFill(string text, float left, float top, float right, float bottom, Color c)
        {
            float w = MeasureText(text);
            int h = FontLookup['T'].Height;
            float scale = Math.Min(
                (right - left) / w,
                (bottom - top) / h
                );
            DrawText(text, scale * FONTSCALE, left, (top + bottom) * 0.5f - h * scale * 0.5f, c);
        }

        public static float MeasureText(string text)
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

        public static void Begin(int width, int height)
        {
            GL.MatrixMode(MatrixMode.Projection);
            GL.LoadIdentity();
            GL.Ortho(-width * 0.5f, width * 0.5f, height * 0.5f, -height * 0.5f, 0f, 1f);
        }

        public static void End()
        {
        }

        public static void Init()
        {
            GL.Enable(EnableCap.Blend);
            GL.BlendFunc(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.OneMinusSrcAlpha);

            FontLookup = new Dictionary<char, Sprite>();
            Font f = new Font("Courier", FONTSCALE);
            SizeF size;

            foreach (char c in @"qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890!£$%^&*()-=_+[]{};:'@#~,.<>/?¬`\|")
            {
                using (var b = new Bitmap(1, 1))
                {
                    using (var g = Graphics.FromImage(b))
                    {
                        size = g.MeasureString(c.ToString(), f);
                    }
                }
                var bmp = new Bitmap((int)size.Width, (int)size.Height);
                using (var g = Graphics.FromImage(bmp))
                {
                    //g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.AntiAlias;
                    //g.InterpolationMode = System.Drawing.Drawing2D.InterpolationMode.Bilinear;
                    g.TextRenderingHint = System.Drawing.Text.TextRenderingHint.AntiAlias;
                    g.DrawString(c.ToString(), f, Brushes.White, 0, 0);
                }
                FontLookup.Add(c, Content.UploadTexture(bmp, 1, 1, true));
            }
        }
    }
}
