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
            FBO.InitBuffers();
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
