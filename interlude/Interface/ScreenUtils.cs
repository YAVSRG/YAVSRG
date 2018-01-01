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
    }
}
