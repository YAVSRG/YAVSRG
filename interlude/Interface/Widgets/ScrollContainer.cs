using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    public class ScrollContainer : Widget
    {
        float padX;
        float padY;
        bool horizontal;
        float scroll;
        Sprite frame;

        public ScrollContainer(float padx, float pady, bool style) : base()
        {
            padX = padx;
            padY = pady;
            horizontal = style;
            //all widgets must be anchored to top left
            frame = Content.LoadTextureFromAssets("frame");
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            float x = padX;
            float y = padY - scroll;
            foreach (Widget w in Widgets)
            {
                if (w.State > 0)
                {
                    w.B.Target(x + w.Width, y + w.Height);
                    w.A.Target(x, y);
                    if (horizontal)
                    {
                        x += padX + w.Width;
                    }
                    else
                    {
                        y += padY + w.Height;
                    }
                }
            }
            if (ScreenUtils.MouseOver(left, top, right, bottom))
            {
                scroll -= Input.MouseScroll * 100;
                scroll = Math.Max(Math.Min(scroll, y), 0);
            }
            //B.Target(A.AbsX+x, A.AbsY+y);
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.StencilMode(1);
            Game.Screens.DrawStaticChartBackground(left, top, right, bottom, Game.Screens.DarkColor);
            SpriteBatch.StencilMode(2);
            DrawWidgets(left, top, right, bottom);
            SpriteBatch.StencilMode(0);
            SpriteBatch.DrawFrame(frame, left, top, right, bottom, 30f, Game.Screens.HighlightColor);
        }
    }
}
