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
            Rect bounds = GetBounds();
            float x = padX;
            float y = padY - scroll;
            Rect widgetBounds;
            foreach (Widget w in Widgets)
            {
                if (w.State > 0)
                {
                    widgetBounds = w.GetBounds(bounds);
                    if (horizontal)
                    {
                        x += padX + widgetBounds.Width;
                    }
                    else
                    {
                        y += padY + widgetBounds.Height;
                    }
                }
            }
            base.AddChild(child);
            widgetBounds = child.GetBounds(bounds);
            child.TopLeft.Position(x, y, bounds);
            child.BottomRight.Position(x + widgetBounds.Width, y + widgetBounds.Height, bounds);
        }

        public override void Update(Rect bounds)
        {
            bounds = GetBounds(bounds);
            float x = padX;
            float y = padY - scroll;
            Rect widgetBounds;
            foreach (Widget w in Widgets)
            {
                if (w.State > 0)
                {
                    w.Update(bounds);
                    widgetBounds = w.GetBounds(bounds);
                    w.Move(new Rect(x, y, x + widgetBounds.Width, y + widgetBounds.Height), bounds);
                    if (horizontal)
                    {
                        x += padX + widgetBounds.Width;
                    }
                    else
                    {
                        y += padY + widgetBounds.Height;
                    }
                }
            }
            if (canscroll > 0 && ScreenUtils.MouseOver(bounds))
            {
                if (canscroll > 1)
                {
                    scrollcolor.Target(Color.FromArgb(127,Game.Screens.HighlightColor));
                }
                scroll -= Input.MouseScroll * 100;
                if (bounds.Top + y < bounds.Bottom) //prevents scrolling off the bottom
                {
                    scroll -= bounds.Bottom - (bounds.Top + y);
                }
                scroll = Math.Max(scroll, 0); //prevents scrolling off the top
            }
            else
            {
                scrollcolor.Target(Color.Transparent);
            }
            contentsHeight = y + scroll;
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            SpriteBatch.StencilMode(1); //draw a mask preventing widgets from drawing outside the bounds of this one
            SpriteBatch.DrawRect(bounds, Color.Transparent);
            if (frame)
            {
                Game.Screens.DrawChartBackground(bounds, Game.Screens.DarkColor, 1.5f);
            }
            SpriteBatch.StencilMode(2); //draw with consideration to this mask
            foreach (Widget w in Widgets)
            {
                if (w.State > 0 && w.Bottom(bounds) > bounds.Top && w.Top(bounds) < bounds.Bottom) //optimisation for large numbers of items
                {
                    w.Draw(bounds);
                }
            }
            SpriteBatch.StencilMode(0); //masking mode off
            if (frame)
            {
                ScreenUtils.DrawFrame(bounds, 30f, Game.Screens.HighlightColor);
            }
            if (canscroll > 1) //draw scroll bar (doesn't work)
            {
                float y = bounds.Height / contentsHeight;
                //if (y < 0.95f)
                {
                    float percentage = (scroll) / (contentsHeight - bounds.Height);
                    SpriteBatch.DrawRect(new Rect(bounds.Left, bounds.Top + 25 + (bounds.Height - 100) * percentage, bounds.Right - 5, bounds.Top + 75 + (bounds.Height - 100) * percentage), scrollcolor);
                }
            }
        }

        public List<Widget> Items()
        {
            return Widgets;
        }
    }
}
