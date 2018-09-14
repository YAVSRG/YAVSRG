using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface
{
    public class Widget
    {
        public AnchorPoint TopLeft; //probably need better names. this is the top left corner
        public AnchorPoint BottomRight; //this is the bottom left corner
        public int State = 1; //0 is hidden, 2 is focussed
        public AnimationGroup Animation; //animation manager for this widget
        protected Widget parent;
        protected List<Widget> Widgets; //children of the widget

        public Widget()
        {
            TopLeft = new AnchorPoint(0, 0, AnchorType.MIN, AnchorType.MIN); //these defaults were put in later for widgets to auto-fill the space they're in
            BottomRight = new AnchorPoint(0, 0, AnchorType.MAX, AnchorType.MAX);
            Animation = new AnimationGroup(true);
            Animation.Add(TopLeft);
            Animation.Add(BottomRight);
            Widgets = new List<Widget>();
        }

        public virtual void AddToContainer(Widget parent)
        {
            this.parent = parent;
        }

        public virtual void AddChild(Widget child)
        {
            lock (Widgets)
            {
                Widgets.Add(child);
            }
            child.AddToContainer(this);
        }

        public float Left(Rect bounds)
        {
            return TopLeft.X(bounds.Left, bounds.Right);
        }

        public float Top(Rect bounds)
        {
            return TopLeft.Y(bounds.Top, bounds.Bottom);
        }

        public float Right(Rect bounds)
        {
            return BottomRight.X(bounds.Left, bounds.Right);
        }

        public float Bottom(Rect bounds)
        {
            return BottomRight.Y(bounds.Top, bounds.Bottom);
        }

        public Rect GetBounds(Rect containerBounds) //returns the bounds of *this widget* given the bounds of its container
        {
            return new Rect(Left(containerBounds), Top(containerBounds), Right(containerBounds), Bottom(containerBounds));
        }

        public virtual Rect GetBounds() //returns the bounds of *this widget* when no bounds are given (useful for some unusual cases but otherwise you shouldn't be using this)
            //only use this when you need access to the widget bounds and you're not inside a draw or update call (where you're given them)
        {
            if (parent != null)
            {
                return GetBounds(parent.GetBounds());
            }
            else
            {
                return GetBounds(new Rect(-ScreenUtils.ScreenWidth, -ScreenUtils.ScreenHeight, ScreenUtils.ScreenWidth, ScreenUtils.ScreenHeight));
            }
        }

        public Widget Position(Options.WidgetPosition pos)
        {
            return PositionTopLeft(pos.Left, pos.Top, pos.LeftAnchor, pos.TopAnchor).PositionBottomRight(pos.Right, pos.Bottom, pos.RightAnchor, pos.BottomAnchor);
        }

        public Widget PositionTopLeft(float x, float y, AnchorType ax, AnchorType ay) //deprecate soon
        {
            TopLeft.Reposition(x, y, ax, ay);
            return this; //allows for method chaining
        }

        public Widget PositionBottomRight(float x, float y, AnchorType ax, AnchorType ay)
        {
            BottomRight.Reposition(x, y, ax, ay);
            return this;
        }

        public Widget Move(Rect bounds, Rect parentBounds)
        {
            TopLeft.Target(bounds.Left, bounds.Top, parentBounds);
            BottomRight.Target(bounds.Right, bounds.Bottom, parentBounds);
            return this;
        }

        public Widget Move(Rect bounds)
        {
            TopLeft.Target(bounds.Left, bounds.Top);
            BottomRight.Target(bounds.Right, bounds.Bottom);
            return this;
        }

        public virtual void Draw(Rect bounds)
        {
            DrawWidgets(GetBounds(bounds));
        }

        protected void DrawWidgets(Rect bounds)
        {
            lock (Widgets) //anti crash measure (probably temp)
            {
                foreach (Widget w in Widgets)
                {
                    if (w.State > 0)
                    {
                        w.Draw(bounds);
                    }
                }
            }
        }

        public virtual void Update(Rect bounds)
        {
            bounds = GetBounds(bounds);
            int c = Widgets.Count;
            Widget w;
            for (int i = c - 1; i >= 0; i--)
            {
                w = Widgets[i];
                if (w.State > 0)
                {
                    w.Update(bounds);
                }
            }
            Animation.Update();
        }

        public virtual void OnResize()
        {
            foreach (Widget w in Widgets)
            {
                w.OnResize();
            }
        }
    }
}
