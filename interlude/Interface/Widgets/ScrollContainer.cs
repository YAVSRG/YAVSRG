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
        bool autoSize;
        Animations.AnimationColorMixer scrollcolor;
        Animations.AnimationSlider scrollposition;
        float contentsSize = 0;
        public int selectedItem = -1;

        public ScrollContainer(float padx, float pady, bool horizontal, int scroll = 2, bool frame = true, bool autosize = false) : base()
        {
            canscroll = scroll;
            this.frame = frame;
            padX = padx;
            padY = pady;
            this.horizontal = horizontal;
            autoSize = autosize;
            Animation.Add(scrollcolor = new Animations.AnimationColorMixer(Color.Transparent));
            Animation.Add(scrollposition = new Animations.AnimationSlider(0));
        }

        public override void AddChild(Widget child)
        {
            Rect bounds = GetBounds();
            float x = padX - (horizontal ? scroll : 0);
            float y = padY - (horizontal ? 0 : scroll);
            Rect widgetBounds;
            lock (Children)
            {
                foreach (Widget w in Children)
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
            }
            widgetBounds = child.GetBounds(bounds);
            child.TopLeft.Position(x, y, bounds);
            child.BottomRight.Position(x + widgetBounds.Width, y + widgetBounds.Height, bounds);
            base.AddChild(child);
        }

        public override void Update(Rect bounds)
        {
            selectedItem = 0;
            Rect parentBounds = bounds;
            bounds = GetBounds(bounds);
            float x = padX - (horizontal ? scroll : 0);
            float y = padY - (horizontal ? 0 : scroll);
            Rect widgetBounds;
            lock (Children)
            {
                foreach (Widget w in Children)
                {
                    if (w.State > 0)
                    {
                        widgetBounds = w.GetBounds(bounds);
                        w.Move(new Rect(x, y, x + widgetBounds.Width, y + widgetBounds.Height), bounds);
                        if (y + widgetBounds.Height <= 0)
                        {
                            selectedItem += 1;
                        }
                        w.Update(bounds);
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
            }
            contentsSize = (horizontal ? x : y) + scroll;
            /*
            if (autoSize)
            {
                if (horizontal)
                {

                }
                else
                {
                    BottomRight.Target(bounds.Right, bounds.Top + y + scroll, parentBounds);
                }
            }*/
            if (canscroll > 0 && ScreenUtils.MouseOver(bounds))
            {
                if (canscroll > 1)
                {
                    scrollcolor.Target(Color.FromArgb(127, Game.Screens.HighlightColor));
                }
                scroll -= Input.MouseScroll * 100;
                if (horizontal)
                {
                    scroll = Math.Min(scroll, contentsSize - bounds.Width); //prevents scrolling off the side
                }
                else
                {
                    scroll = Math.Min(scroll, contentsSize - bounds.Height); //prevents scrolling off the bottom
                }
                scroll = Math.Max(scroll, 0); //prevents scrolling off the top
            }
            else
            {
                scrollcolor.Target(Color.Transparent);
            }
            Animation.Update();
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            SpriteBatch.Stencil(SpriteBatch.StencilMode.Create); //draw a mask preventing widgets from drawing outside the bounds of this one
            SpriteBatch.DrawRect(bounds, Color.Transparent);
            SpriteBatch.Stencil(SpriteBatch.StencilMode.Draw); //draw with consideration to this mask
            if (frame)
            {
                Game.Screens.DrawChartBackground(bounds, Game.Screens.DarkColor, 1.5f);
            }
            lock (Children)
            {
                foreach (Widget w in Children)
                {
                    if (w.State > 0 && w.Bottom(bounds) > bounds.Top && w.Top(bounds) < bounds.Bottom) //optimisation for large numbers of items
                    {
                        w.Draw(bounds);
                    }
                }
            }
            if (frame)
            {
                ScreenUtils.DrawFrame(bounds, 30f, Game.Screens.HighlightColor);
            }
                SpriteBatch.Stencil(SpriteBatch.StencilMode.Disable); //masking mode off
            
            if (canscroll > 1) //draw scroll bar (doesn't work)
            {
                float y = bounds.Height / contentsSize;
                if (y < 0.95f)
                {
                    float percentage = scroll / (contentsSize - bounds.Height);
                    SpriteBatch.DrawRect(new Rect(bounds.Right - 25, bounds.Top + 25 + (bounds.Height - 100) * percentage, bounds.Right - 5, bounds.Top + 75 + (bounds.Height - 100) * percentage), scrollcolor);
                }
            }
        }

        public void ScrollToItem(int id)
        {
            if (Children.Count > id)
            {
                var b = GetBounds();
                scroll += Children[id].Top(b) - b.Top;
            }
        }

        public List<Widget> Items()
        {
            return Children;
        }
    }
}
