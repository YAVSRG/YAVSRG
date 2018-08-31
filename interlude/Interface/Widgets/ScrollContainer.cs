using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace YAVSRG.Interface.Widgets
{
    public class ScrollContainer : Widget
    {
        float padX;
        float padY;
        bool horizontal;
        protected float scroll;
        int canscroll; //2 is freely scrollable WITH scrollbar, 1 is scroll without scrollbar, 0 is no scrolling
        bool frame;
        Animations.AnimationColorMixer scrollcolor;
        float contentsHeight = 0;

        public ScrollContainer(float padx, float pady, bool horizontal, int scroll = 2, bool frame = true) : base()
        {
            canscroll = scroll;
            this.frame = frame;
            padX = padx;
            padY = pady;
            this.horizontal = horizontal;
            Animation.Add(scrollcolor = new Animations.AnimationColorMixer(Color.Transparent));
        }

        public override void AddChild(Widget child)
        {
            /*
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
            child.A.Position(x, y);*/
            base.AddChild(child);
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
                    w.B.Target(x + w.Width(left, right), y + w.Height(top, bottom));
                    w.A.Target(x, y);
                    if (horizontal)
                    {
                        x += padX + w.Width(left, right);
                    }
                    else
                    {
                        y += padY + w.Height(top, bottom);
                    }
                }
            }
            if (canscroll > 0 && ScreenUtils.MouseOver(left, top, right, bottom))
            {
                if (canscroll > 1)
                {
                    scrollcolor.Target(Color.FromArgb(127,Game.Screens.HighlightColor));
                }
                scroll -= Input.MouseScroll * 100;
                if (top + y < bottom)
                {
                    scroll -= bottom - (top + y);
                }
                scroll = Math.Max(scroll, 0);
            }
            else
            {
                scrollcolor.Target(Color.Transparent);
            }
            contentsHeight = y + scroll;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.StencilMode(1);
            SpriteBatch.DrawRect(left, top, right, bottom, Color.Transparent);
            if (frame)
            {
                Game.Screens.DrawChartBackground(left, top, right, bottom - 2, Game.Screens.DarkColor, 1.5f);
            }
            SpriteBatch.StencilMode(2);
            foreach (Widget w in Widgets)
            {
                if (w.State > 0 && w.B.Y(top, bottom) > top && w.A.Y(top, bottom) < bottom)
                {
                    w.Draw(left, top, right, bottom);
                }
            }
            SpriteBatch.StencilMode(0);
            if (frame)
            {
                SpriteBatch.DrawFrame(left, top, right, bottom, 30f, Game.Screens.HighlightColor);
            }
            if (canscroll > 1)
            {
                float y = (bottom - top) / contentsHeight;
                //if (y < 0.95f)
                {
                    float percentage = (scroll) / (contentsHeight - (bottom - top));
                    SpriteBatch.DrawRect(left, top + 25 + (bottom - top - 100) * percentage, right - 5, top + 75 + (bottom - top - 100) * percentage, scrollcolor);
                }
            }
        }

        public List<Widget> Items()
        {
            return Widgets;
        }
    }
}
