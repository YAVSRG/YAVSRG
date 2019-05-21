using System;
using System.Drawing;
using Interlude.Interface.Animations;
using Interlude.IO;

namespace Interlude.Interface.Widgets
{
    public class FlowContainer : FrameContainer
    {
        public float MarginX = 10, MarginY = 10, RowSpacing = 5;
        public float ScrollPosition;
        public int VisibleIndexTop, VisibleIndexBottom;
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
            bounds = GetBounds(bounds);
            FlowContent(bounds);
            if (ScreenUtils.MouseOver(bounds))
            {
                ScrollBarColor.Target(Color.FromArgb(127, Game.Screens.HighlightColor));
                ScrollPosition -= Input.MouseScroll * 100;
                ScrollPosition = Math.Max(Math.Min(ScrollPosition, ContentSize - bounds.Height), 0);
            }
            Animation.Update();
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            PreDraw(bounds);
            Rect newBounds = bounds.Expand(-MarginX, -MarginY);
            //SpriteBatch.Stencil(SpriteBatch.StencilMode.Create);
            //SpriteBatch.DrawRect(newBounds, Color.Transparent);
            //SpriteBatch.Stencil(SpriteBatch.StencilMode.Draw);
            lock (Children)
            {
                foreach (Widget w in Children)
                {
                    if (w.State > 0 && w.Bottom(newBounds) > bounds.Top && w.Top(newBounds) < bounds.Bottom) //optimisation for large numbers of items
                    {
                        w.Draw(newBounds);
                    }
                }
            }
            //SpriteBatch.Stencil(SpriteBatch.StencilMode.Disable);
            ScreenUtils.DrawFrame(bounds, 30f, FrameColor(), components: Frame);
            PostDraw(bounds);
        }

        private void FlowContent(Rect bounds)
        {
            VisibleIndexTop = VisibleIndexBottom = -1;
            bounds = bounds.Expand(-MarginX, -MarginY);
            float x = 0;
            float y = -ScrollPosition;
            Rect widgetBounds;
            lock (Children)
            {
                for (int i = 0; i < Children.Count; i++)
                {
                    Widget w = Children[i];
                    if (w.State > 0)
                    {
                        widgetBounds = w.GetBounds(bounds);
                        x += widgetBounds.Width;
                        if (x > bounds.Width)
                        {
                            x = widgetBounds.Width;
                            y += widgetBounds.Height + RowSpacing;
                        }
                        ContentSize = y + widgetBounds.Height;
                        w.MoveRelative(new Rect(x - widgetBounds.Width, y, x, y + widgetBounds.Height), bounds);
                        if (ContentSize > bounds.Top && y < bounds.Bottom)
                        {
                            if (VisibleIndexTop < 0) VisibleIndexTop = i;
                            VisibleIndexBottom = i;
                        }
                        w.Update(bounds);
                    }
                }
            }
            ContentSize += ScrollPosition + 4 * MarginY + RowSpacing;
        }

        public override void AddChild(Widget child)
        {
            //inserts new object where it should be
            Rect bounds = GetBounds().Expand(-MarginX, -MarginY);
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
                            y += widgetBounds.Height + RowSpacing; 
                        }
                    }
                }
            }
            widgetBounds = child.GetBounds(bounds);
            x += widgetBounds.Width;
            if (x > bounds.Width)
            {
                x = widgetBounds.Width;
                y += widgetBounds.Height + RowSpacing;
            }
            base.AddChild(child);
            child.RepositionRelative(new Rect(x - widgetBounds.Width, y, x, y + widgetBounds.Height), bounds);
        }

        public void ScrollTo(int index)
        {
            if (index >= 0 && index < Children.Count)
            {
                Rect bounds = GetBounds().Expand(-MarginX, -MarginY);
                ScrollPosition += Children[index].TopAnchor.GetPosition(bounds.Top, bounds.Bottom, true) - bounds.Top;
            }
        }

        public void Clear()
        {
            lock (Children)
            {
                foreach (Widget w in Children)
                {
                    w.RemoveFromContainer(this);
                }
                Children.Clear();
            }
        }

        public void Filter(Func<Widget,bool> filter)
        {
            foreach (Widget w in Children)
            {
                w.SetState(filter(w) ? WidgetState.NORMAL : WidgetState.DISABLED);
            }
        }

        public void Sort(Comparison<Widget> compare)
        {
            lock (Children)
            {
                Children.Sort(compare);
            }
        }
    }
}
