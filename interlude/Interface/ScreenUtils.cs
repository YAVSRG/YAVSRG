using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace YAVSRG.Interface
{
    class ScreenUtils
    {
        public static int ScreenWidth;

        public static int ScreenHeight;

        public static void UpdateBounds(int Width, int Height)
        {
            ScreenWidth = Width / 2;
            ScreenHeight = Height / 2;
            if (ScreenWidth < 800 || ScreenHeight < 450)
            {
                float r = Math.Max(800f / ScreenWidth, 450f / ScreenHeight);
                ScreenWidth = (int)(ScreenWidth * r);
                ScreenHeight = (int)(ScreenHeight * r);
            }
        }

        public static bool MouseOver(float left, float top, float right, float bottom)
        {
            int mx = Input.MouseX;
            int my = Input.MouseY;
            return (mx > left && mx < right && my > top && my < bottom);
        }

        public static bool CheckButtonClick(float left, float top, float right, float bottom)
        {
            return MouseOver(left, top, right, bottom) && Input.MouseClick(OpenTK.Input.MouseButton.Left);
        }

        /*
        public static void DrawBanner(float left, float top, float right, float bottom, Color c)
        {
            float height = bottom - top;
            SpriteBatch.Draw("banner", left, top, left + height, bottom, c, 0, 0);
            SpriteBatch.Draw("banner", left + height, top, right - height, bottom, c, 1, 0);
            SpriteBatch.Draw("banner", right - height, top, right, bottom, c, 2, 0);
        }*/

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
        }

        public static void DrawParallelogramWithBG(float left, float top, float right, float bottom, float amount, Color fill, Color frame)
        {
            float h = (bottom - top) * 0.5f;
            float t = h * Math.Abs(amount);
            SpriteBatch.ParallelogramTransform(amount, top + h);
            SpriteBatch.StencilMode(1);
            SpriteBatch.DrawRect(left-t, top, right+t, bottom, Color.White);
            SpriteBatch.DisableTransform();
            SpriteBatch.StencilMode(2);
            Game.Screens.DrawChartBackground(left - h, top, right + h, bottom, fill, 1.5f);
            SpriteBatch.StencilMode(0);
            SpriteBatch.ParallelogramTransform(amount, top + h);
            SpriteBatch.DrawFrame(left-t, top, right+t, bottom, 30f, frame);
            SpriteBatch.DisableTransform();
        }
    }
}
