using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface.Widgets
{
    public class FlowContainer : FrameContainer
    {
        public float MarginX, MarginY;
        public float ScrollPosition;
        protected float ContentSize;
        AnimationSlider ScrollBarPosition;
        AnimationColorMixer ScrollBarColor;

        public FlowContainer()
        {
            Animation.Add(ScrollBarPosition = new AnimationSlider(0));
            Animation.Add(ScrollBarColor = new AnimationColorMixer(Color.Transparent));
        }

        public override void Update(Rect bounds)
        {
            Rect newBounds = GetBounds(bounds);
            FlowContent(newBounds);
            base.Update(bounds);
            if (ScreenUtils.MouseOver(newBounds))
            {
                ScrollBarColor.Target(Color.FromArgb(127, Game.Screens.HighlightColor));
                ScrollPosition -= Input.MouseScroll * 100;
                ScrollPosition = Math.Max(Math.Min(ScrollPosition, ContentSize - bounds.Height), 0);
            }
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            PreDraw(bounds);
            DrawBackplate(bounds);
            PostDraw(bounds);
            Rect newBounds = bounds.Expand(-MarginX, -MarginY);
            lock (Children)
            {
                foreach (Widget w in Children)
                {
                    if (w.State > 0 && w.Bottom(newBounds) > newBounds.Top && w.Top(newBounds) < newBounds.Bottom) //optimisation for large numbers of items
                    {
                        w.Draw(newBounds);
                    }
                }
            }
        }

        private void FlowContent(Rect bounds)
        {
            Rect parentBounds = bounds;
            bounds = GetBounds(bounds).Expand(-MarginX, -MarginY);
            float x = 0;
            float y = -ScrollPosition;
            Rect widgetBounds;
            lock (Children)
            {
                foreach (Widget w in Children)
                {
                    if (w.State > 0)
                    {
                        widgetBounds = w.GetBounds(bounds);
                        x += widgetBounds.Width;
                        if (x > bounds.Width)
                        {
                            x = widgetBounds.Width;
                            y += widgetBounds.Height;
                        }
                        ContentSize = y + widgetBounds.Height;
                        w.Move(new Rect(x - widgetBounds.Width, y, x, y + widgetBounds.Height), bounds);
                    }
                }
            }
            ContentSize += ScrollPosition;
        }
    }
}
