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
        public AnchorPoint A;
        public AnchorPoint B;
        public int State = 1; //0 is hidden, 2 is focussed
        public AnimationGroup Animation;
        protected Widget parent;
        protected List<Widget> Widgets;

        public Widget()
        {
            A = new AnchorPoint(0, 0, AnchorType.MIN, AnchorType.MIN);
            B = new AnchorPoint(0, 0, AnchorType.MAX, AnchorType.MAX);
            Animation = new AnimationGroup(true);
            Animation.Add(A);
            Animation.Add(B);
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

        public float Left(float l, float r)
        {
            return A.X(l, r);
        }

        public float Top(float t, float b)
        {
            return A.Y(t, b);
        }

        public float Right(float l, float r)
        {
            return B.X(l, r);
        }

        public float Bottom(float t, float b)
        {
            return B.Y(t, b);
        }

        public float Width
        {
            get { return Math.Abs(B.TargetX - A.TargetX); }
        }

        public float Height
        {
            get { return Math.Abs(B.TargetY - A.TargetY); }
        }

        /*
        public float GetWidth(float l, float r)
        {
            return Right(l, r) - Left(l, r);
        }

        public float GetHeight(float t, float b)
        {
            return Bottom(t, b) - Top(t, b);
        }*/

        protected void ConvertCoordinates(ref float l, ref float t, ref float r, ref float b)
        {
            float ol = l;//otherwise it will fuck up
            float ot = t;
            l = Left(l, r); t = Top(t, b); r = Right(ol, r); b = Bottom(ot, b);
        }

        public Widget Position(Options.WidgetPosition pos)
        {
            PositionTopLeft(pos.Left, pos.Top, pos.LeftAnchor, pos.TopAnchor);
            PositionBottomRight(pos.Right, pos.Bottom, pos.RightAnchor, pos.BottomAnchor);
            return this;
        }

        public Widget PositionTopLeft(float x, float y, AnchorType ax, AnchorType ay)
        {
            A.Reposition(x, y, ax, ay);
            return this; //allows for method chaining
        }


        public Widget PositionBottomRight(float x, float y, AnchorType ax, AnchorType ay)
        {
            B.Reposition(x, y, ax, ay);
            return this;
        }

        public virtual void Draw(float left, float top, float right, float bottom)
        {
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            DrawWidgets(left, top, right, bottom);
        }

        protected void DrawWidgets(float left, float top, float right, float bottom)
        {
            lock (Widgets) //anti crash measure (probably temp)
            {
                foreach (Widget w in Widgets)
                {
                    if (w.State > 0)
                    {
                        w.Draw(left, top, right, bottom);
                    }
                }
            }
        }

        public virtual void Update(float left, float top, float right, float bottom)
        {
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            int c = Widgets.Count;
            Widget w;
            for (int i = c - 1; i >= 0; i--)
            {
                w = Widgets[i];
                if (w.State > 0)
                {
                    w.Update(left, top, right, bottom);
                }
            }
            Animation.Update();
        }
    }
}
