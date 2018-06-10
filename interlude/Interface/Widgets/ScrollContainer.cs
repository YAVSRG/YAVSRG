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
        bool canscroll;

        public ScrollContainer(float padx, float pady, bool style, bool canscroll = true) : base()
        {
            this.canscroll = canscroll;
            padX = padx;
            padY = pady;
            horizontal = style;
            //all widgets must be anchored to top left
        }

        public override void AddChild(Widget child)
        {
            float x = padX;
            float y = padY - scroll;
            foreach (Widget w in Widgets)
            {
                if (w.State > 0)
                {
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
            base.AddChild(child);
            child.B.Position(x + child.Width, y + child.Height);
            child.A.Position(x, y);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            float x = padX;
            float y = padY - scroll;
            foreach (Widget w in Widgets)
            {
                if (w.State > 0)
                {
                    w.Update(left, top, right, bottom);
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
            if (canscroll && ScreenUtils.MouseOver(left, top, right, bottom))
            {
                scroll -= Input.MouseScroll * 100;
                scroll = Math.Max(Math.Min(scroll, y-Height), 0);
            }
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.StencilMode(1);
            Game.Screens.DrawChartBackground(left, top, right, bottom, Game.Screens.DarkColor, 1.5f);
            SpriteBatch.StencilMode(2);
            DrawWidgets(left, top, right, bottom);
            SpriteBatch.StencilMode(0);
            SpriteBatch.DrawFrame(left, top, right, bottom, 30f, Game.Screens.HighlightColor);
        }

        private bool ShouldRender(Widget w)
        {
            float t = w.A.AbsY - scroll;
            float b = w.B.AbsY - scroll;
            return (t >= 0 && t <= Height) || (b >= 0 && b <= Height);
        }
    }
}
