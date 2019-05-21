using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using Interlude.Interface.Animations;

namespace Interlude.Interface
{
    //Represents an element of the interface. It is an object with rectangular bounds that are anchored relative to a parent (ultimately the screen itself)
    //Can contain other widgets as children which are anchored relative to themselves.
    public class Widget
    {
        //todo: replace this with a better anchor system i came up with
        public AnimationAnchorPoint LeftAnchor, TopAnchor, RightAnchor, BottomAnchor;
        public AnimationGroup Animation; //animation manager for this widget

        protected Widget Parent;
        protected List<Widget> Children;

        protected WidgetState _State = WidgetState.NORMAL;

        public Widget()
        {
            Animation = new AnimationGroup(true);
            Animation.Add(LeftAnchor = new AnimationAnchorPoint(0, AnchorType.MIN));
            Animation.Add(TopAnchor = new AnimationAnchorPoint(0, AnchorType.MIN));
            Animation.Add(RightAnchor = new AnimationAnchorPoint(0, AnchorType.MAX));
            Animation.Add(BottomAnchor = new AnimationAnchorPoint(0, AnchorType.MAX));
            Children = new List<Widget>();
        }

        public virtual void AddToContainer(Widget parent)
        {
            Parent = parent;
        }

        public virtual void RemoveFromContainer(Widget parent)
        {
            Parent = null;
        }

        //Adds a new widget embedded inside this one
        public virtual void AddChild(Widget child)
        {
            lock (Children)
            {
                Children.Add(child);
            }
            child.AddToContainer(this);
        }

        //Removes a widget embedded inside this one. Rarely used as it is easier to make widgets invisible until the parent is destroyed shortly after
        public virtual void RemoveChild(Widget child)
        {
            lock (Children)
            {
                Children.Remove(child);
            }
            child.RemoveFromContainer(this);
        }

        public float Left(Rect bounds)
        {
            return LeftAnchor.RelativePos(bounds.Left, bounds.Right, false);
        }

        public float Top(Rect bounds)
        {
            return TopAnchor.RelativePos(bounds.Top, bounds.Bottom, false);
        }

        public float Right(Rect bounds)
        {
            return RightAnchor.RelativePos(bounds.Left, bounds.Right, false);
        }

        public float Bottom(Rect bounds)
        {
            return BottomAnchor.RelativePos(bounds.Top, bounds.Bottom, false);
        }

        public Rect GetBounds(Rect containerBounds) //returns the bounds of *this widget* given the bounds of its parent
        {
            return new Rect(Left(containerBounds), Top(containerBounds), Right(containerBounds), Bottom(containerBounds));
        }

        public virtual Rect GetBounds() //returns the bounds of *this widget* when no bounds are given (useful for some unusual cases but otherwise you shouldn't be using this)
            //only use this when you need access to the widget bounds and you're not inside a draw or update call (where you're given them)
            //it works backwards to find the bounds the widget should have and therefore takes more steps than the one above
        {
            if (Parent != null)
            {
                return GetBounds(Parent.GetBounds());
            }
            else
            {
                return GetBounds(ScreenUtils.Bounds);
            }
        }

        public Widget Position(Options.WidgetPosition pos)
        {
            return PositionTopLeft(pos.Left, pos.Top, pos.LeftAnchor, pos.TopAnchor).PositionBottomRight(pos.Right, pos.Bottom, pos.RightAnchor, pos.BottomAnchor);
        }

        public Widget PositionTopLeft(float x, float y, AnchorType ax, AnchorType ay) //todo: deprecate these
        {
            LeftAnchor.Move(x, ax);
            TopAnchor.Move(y, ay);
            return this; //allows for method chaining
        }

        public Widget PositionBottomRight(float x, float y, AnchorType ax, AnchorType ay)
        {
            RightAnchor.Move(x, ax);
            BottomAnchor.Move(y, ay);
            return this;
        }

        public Widget Move(Rect bounds, Rect parentBounds, bool instant)
        {
            LeftAnchor.Move(bounds.Left, instant, parentBounds.Left, parentBounds.Right);
            TopAnchor.Move(bounds.Top, instant, parentBounds.Top, parentBounds.Bottom);
            RightAnchor.Move(bounds.Right, instant, parentBounds.Left, parentBounds.Right);
            BottomAnchor.Move(bounds.Bottom, instant, parentBounds.Top, parentBounds.Bottom);
            return this;
        }

        public Widget Move(Rect bounds, bool instant)
        {
            LeftAnchor.Move(bounds.Left, instant);
            TopAnchor.Move(bounds.Top, instant);
            RightAnchor.Move(bounds.Right, instant);
            BottomAnchor.Move(bounds.Bottom, instant);
            return this;
        }

        public virtual void Draw(Rect bounds)
        {
            DrawWidgets(GetBounds(bounds));
        }

        public virtual void SetState(WidgetState s)
        {
            _State = s;
        }

        public virtual void ToggleState()
        {
            SetState(State != WidgetState.DISABLED ? WidgetState.DISABLED : WidgetState.NORMAL);
        }

        public WidgetState State
        {
            get
            {
                return _State;
            }
        }

        //BOUNDS HERE ARE THIS WIDGET'S BOUNDS, NOT THE PARENT BOUNDS
        protected void DrawWidgets(Rect bounds)
        {
            lock (Children) //protection for cross-thread widget operations
            {
                foreach (Widget w in Children)
                {
                    if (w._State > 0)
                    {
                        w.Draw(bounds);
                    }
                }
            }
        }

        public virtual void Update(Rect bounds)
        {
            bounds = GetBounds(bounds);
            int c = Children.Count;
            Widget w;
            for (int i = c - 1; i >= 0; i--) //updates are done from last element backwards, as these appear visually on top and should grab input first
            {
                w = Children[i];
                if (w._State > 0)
                {
                    w.Update(bounds);
                }
            }
            Animation.Update();
        }

        public virtual void OnResize()
        {
            foreach (Widget w in Children)
            {
                w.OnResize();
            }
        }
    }
}
