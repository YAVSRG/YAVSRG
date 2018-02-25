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
        //buttons
        //dragging sliders
        //tweening motions
        public static int Width;

        public static int Height;

        public static void UpdateBounds(int Width, int Height)
        {
            ScreenUtils.Width = Width / 2;
            ScreenUtils.Height = Height / 2;
        }

        public static void DrawButton(Sprite s, float left, float top, float right, float bottom, Color normal, Color hover)
        {
            SpriteBatch.Draw(s, left, top, right, bottom, MouseOver(left, top, right, bottom) ? hover : normal);
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

        public static void DrawChartBackground(float left, float top, float right, float bottom, Color c)
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
    }
}
