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
        public static int Width;

        public static int Height;

        public static void UpdateBounds(int Width, int Height)
        {
            ScreenUtils.Width = Width / 2;
            ScreenUtils.Height = Height / 2;
        }

        public static bool MouseOver(float left, float top, float right, float bottom)
        {
            int mx = Input.MouseX;
            int my = Input.MouseY;
            return (mx > left && mx < right && my > top && my < bottom);
        }

        public static bool CheckButtonClick(float left, float top, float right, float bottom)
        {
            return Input.MouseClick(OpenTK.Input.MouseButton.Left) && MouseOver(left, top, right, bottom);
        }

        public static void DrawStaticChartBackground(float left, float top, float right, float bottom, Color c) //todo: reduce redundant code in these two functions
        {
            float bg = ((float)Game.CurrentChart.background.Width / Game.CurrentChart.background.Height);
            float window = ((float)Width / Height);
            float correction = window/bg;

            float l = (1 + left / Width) / 2;
            float r = (1 + right / Width) / 2;
            float t = (correction + top / Height) / (2 * correction);
            float b = (correction + bottom / Height) / (2 * correction);

            RectangleF uv = new RectangleF(l, t, r - l, b - t);
            SpriteBatch.Draw(Game.CurrentChart.background, left, top, right, bottom+1, uv, c);
        }

        public static void DrawChartBackground(float left, float top, float right, float bottom, Color c)
        {
            float bg = ((float)Game.CurrentChart.background.Width / Game.CurrentChart.background.Height);
            float window = (right-left)/(bottom-top);
            float correction = window / bg;

            RectangleF uv = new RectangleF(0, (correction - 1) * 0.5f, 1, 1.5f - correction * 0.5f);
            SpriteBatch.Draw(Game.CurrentChart.background, left, top, right, bottom + 1, uv, c);
        }
    }
}
