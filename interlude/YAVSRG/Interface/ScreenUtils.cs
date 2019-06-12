using System;
using System.Drawing;
using OpenTK;
using Prelude.Gameplay;
using Prelude.Gameplay.ScoreMetrics;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface
{
    class ScreenUtils
    {
        public static int ScreenWidth;

        public static int ScreenHeight;

        public static Rect Bounds
        {
            get { return new Rect(-ScreenWidth, -ScreenHeight, ScreenWidth, ScreenHeight); }
        }

        public static void UpdateBounds(int Width, int Height)
        {
            ScreenWidth = Width / 2;
            ScreenHeight = Height / 2;
            if (ScreenWidth < 960 || ScreenHeight < 500)
            {
                float r = Math.Max(960f / ScreenWidth, 500f / ScreenHeight);
                ScreenWidth = (int)(ScreenWidth * r);
                ScreenHeight = (int)(ScreenHeight * r);
            }
            DrawableFBO.ClearPool();
        }

        public static bool MouseOver(Rect bounds)
        {
            int mx = Input.MouseX;
            int my = Input.MouseY;
            return (mx > bounds.Left && mx < bounds.Right && my > bounds.Top && my < bounds.Bottom);
        }

        public static bool CheckButtonClick(Rect bounds)
        {
            return MouseOver(bounds) && Input.MouseClick(OpenTK.Input.MouseButton.Left);
        }

        public static void DrawGraph(Rect bounds, ScoreSystem scoring, HitData[] data)
        {
            //todo: make this a widget that uses FBOs
            int snapcount = data.Length;
            SpriteBatch.DrawRect(bounds, Color.FromArgb(150, 0, 0, 0));
            float w = (bounds.Width - 10) / snapcount;
            float middle = bounds.CenterY;
            float scale = (bounds.Height - 20) * 0.5f / scoring.MissWindow;
            SpriteBatch.DrawRect(new Rect(bounds.Left, middle - 3, bounds.Right, middle + 3), Color.Green);
            int j;
            float o;
            for (int i = 0; i < snapcount; i++)
            {
                for (int k = 0; k < data[i].hit.Length; k++)
                {
                    if (data[i].hit[k] > 0)
                    {
                        o = data[i].delta[k];
                        j = scoring.JudgeHit(Math.Abs(o));
                        if (j > 2)
                        {
                            SpriteBatch.DrawRect(new Rect(bounds.Left + i * w + 4, bounds.Top, bounds.Left + i * w + 6, bounds.Bottom), Color.FromArgb(80, Game.Options.Theme.JudgeColors[5]));
                        }
                        SpriteBatch.DrawRect(new Rect(bounds.Left + i * w + 3, middle - o * scale - 2, bounds.Left + i * w + 8, middle - o * scale + 2), Game.Options.Theme.JudgeColors[j]);
                    }
                }
            }
            DrawFrame(bounds, Color.White);
        }

        public static void DrawFrame(Rect bounds, Color color, byte components = 255, float shadow = 10f, float thickness = 3f)
        {
            Color back = Color.FromArgb(color.A, Color.Black);
            Color transparent = Color.FromArgb(0, 0, 0, 0);

            //todo: maybe make enum so this is easier to use
            if ((components & 16) > 0)
                SpriteBatch.Draw(bounds: bounds.SliceLeft(shadow), colors: new[] { back, transparent, transparent, back });
            if ((components & 32) > 0)
                SpriteBatch.Draw(bounds: bounds.SliceTop(shadow), colors: new[] { back, back, transparent, transparent });
            if ((components & 64) > 0)
                SpriteBatch.Draw(bounds: bounds.SliceRight(shadow), colors: new[] { transparent, back, back, transparent });
            if ((components & 128) > 0)
                SpriteBatch.Draw(bounds: bounds.SliceBottom(shadow), colors: new[] { transparent, transparent, back, back });

            if ((components & 1) > 0)
                SpriteBatch.Draw(bounds: bounds.SliceLeft(thickness), color: color);
            if ((components & 2) > 0)
                SpriteBatch.Draw(bounds: bounds.SliceTop(thickness), color: color);
            if ((components & 4) > 0)
                SpriteBatch.Draw(bounds: bounds.SliceRight(thickness), color: color);
            if ((components & 8) > 0)
                SpriteBatch.Draw(bounds: bounds.SliceBottom(thickness), color: color);
        }

        /*
        public static void DrawArrowConfetti(float left, float top, float right, float bottom, float size, Color min, Color max, float value)
        {
            left -= size; right += size; top -= size; bottom += size;
            int amount = 100;
            float width = right - left;
            float height = bottom - top;
            float l, t, s;
            for (int i = 0; i < amount; i++)
            {
                s = (149 + i * 491) % (size / 2) + (size / 2);
                l = (461 + i * 397) % (width-s);
                t = (811 + i * 433 + value * s) % (height-s);
                SpriteBatch.Draw("arrow", left + l, top + t, left + l + s, top + t + s, Utils.ColorInterp(min, max, (float)Math.Abs(Math.Sin(value + i * 83))), 0, i % 8, 0);
            }
        }*/

        public static void DrawParallelogramWithBG(Rect bounds, float amount, Color fill, Color frame)
        {
            float h = bounds.Height * 0.5f;
            float t = h * Math.Abs(amount);
            SpriteBatch.ParallelogramTransform(amount, bounds.Top + h);
            SpriteBatch.Stencil(SpriteBatch.StencilMode.Create);
            SpriteBatch.DrawRect(bounds.ExpandX(t), Color.Transparent);
            SpriteBatch.DisableTransform();
            SpriteBatch.Stencil(SpriteBatch.StencilMode.Draw);
            Game.Screens.DrawChartBackground(bounds.ExpandX(t * 2), fill, 1.5f);
            SpriteBatch.Stencil(SpriteBatch.StencilMode.Disable);
            SpriteBatch.ParallelogramTransform(amount, bounds.Top + h);
            DrawFrame(bounds.ExpandX(t), frame);
            SpriteBatch.DisableTransform();
        }
    }
}
