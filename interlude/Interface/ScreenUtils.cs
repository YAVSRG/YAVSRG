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

        public static void DrawBanner(Sprite texture, float left, float top, float right, float bottom, Color c)
        {
            float height = bottom - top;
            SpriteBatch.Draw(texture, left, top, left + height, bottom, c, 0, 0);
            SpriteBatch.Draw(texture, left + height, top, right - height, bottom, c, 1, 0);
            SpriteBatch.Draw(texture, right - height, top, right, bottom, c, 2, 0);
        }
    }
}
