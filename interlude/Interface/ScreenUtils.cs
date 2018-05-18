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

        public static void DrawBanner(float left, float top, float right, float bottom, Color c)
        {
            float height = bottom - top;
            SpriteBatch.Draw("banner", left, top, left + height, bottom, c, 0, 0);
            SpriteBatch.Draw("banner", left + height, top, right - height, bottom, c, 1, 0);
            SpriteBatch.Draw("banner", right - height, top, right, bottom, c, 2, 0);
        }

        public static void DrawParallelogramWithBG(float left, float top, float right, float bottom, float amount)
        {
            float h = (bottom - top)*amount;
            SpriteBatch.ParallelogramTransform(amount, top + h/amount * 0.5f);
            SpriteBatch.StencilMode(1);
            SpriteBatch.DrawRect(left-Math.Abs(h), top, right, bottom, Color.White);
            SpriteBatch.DisableTransform();
            SpriteBatch.StencilMode(2);
            Game.Screens.DrawChartBackground(left, top, right+Math.Abs(h)*0.5f, bottom, Game.Screens.DarkColor, 0.5f);
            SpriteBatch.StencilMode(0);
            SpriteBatch.ParallelogramTransform(amount, top + h / amount * 0.5f);
            SpriteBatch.DrawFrame(left-Math.Abs(h), top, right, bottom, 30f, Color.White);
            SpriteBatch.DisableTransform();
        }
    }
}
