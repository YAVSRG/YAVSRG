using System;
using System.Collections.Generic;
using Interlude.Interface.Animations;

namespace Interlude.Interface
{
    //Represents an element of the interface. It is an object with rectangular bounds that are anchored relative to a parent (ultimately the screen itself)
    //Can contain other widgets as children which are anchored relative to themselves.
    public class Widget : IDisposable
    {
        public AnimationAnchorPoint LeftAnchor { private set; get; }
        public AnimationAnchorPoint TopAnchor { private set; get; }
        public AnimationAnchorPoint RightAnchor { private set; get; }
        public AnimationAnchorPoint BottomAnchor { private set; get; }
        public AnimationGroup Animation { private set; get; } //animation manager for this widget

        protected Widget Parent;
        protected List<Widget> Children;

        protected WidgetState _State = WidgetState.NORMAL;

        public Widget()
        {
            Animation = new AnimationGroup(true);
            Animation.Add(LeftAnchor = new AnimationAnchorPoint(0, 0));
            Animation.Add(TopAnchor = new AnimationAnchorPoint(0, 0));
            Animation.Add(RightAnchor = new AnimationAnchorPoint(0, 1));
            Animation.Add(BottomAnchor = new AnimationAnchorPoint(0, 1));
            Children = new List<Widget>();
        }

        public virtual void AddToContainer(Widget parent)
        {
            Parent = parent;
        }

        public virtual void RemoveFromContainer(Widget parent)
        {
            if (parent != Parent)
            {
                Prelude.Utilities.Logging.Log("Removed widget from container that is isn't in?", "Child side", Prelude.Utilities.Logging.LogType.Debug);
            }
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
                if (Children.Contains(child))
                {
                    Children.Remove(child);
                }
                else
                {
                    Prelude.Utilities.Logging.Log("Removed widget from container that is isn't in?", "Parent side", Prelude.Utilities.Logging.LogType.Debug);
                }
            }
            child.RemoveFromContainer(this);
        }

        public float Left(Rect bounds, bool projected = false)
        {
            return LeftAnchor.GetPosition(bounds.Left, bounds.Right, projected);
        }

        public float Top(Rect bounds, bool projected = false)
        {
            return TopAnchor.GetPosition(bounds.Top, bounds.Bottom, projected);
        }

        public float Right(Rect bounds, bool projected = false)
        {
            return RightAnchor.GetPosition(bounds.Left, bounds.Right, projected);
        }

        public float Bottom(Rect bounds, bool projected = false)
        {
            return BottomAnchor.GetPosition(bounds.Top, bounds.Bottom, projected);
        }

        public Rect GetBounds(Rect containerBounds, bool projected = false) //returns the bounds of *this widget* given the bounds of its parent
        {
            return new Rect(Left(containerBounds, projected), Top(containerBounds, projected), Right(containerBounds, projected), Bottom(containerBounds, projected));
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

        public Widget Reposition(float Left, float LeftA, float Top, float TopA, float Right, float RightA, float Bottom, float BottomA)
        {
            LeftAnchor.Reposition(Left, LeftA);
            TopAnchor.Reposition(Top, TopA);
            RightAnchor.Reposition(Right, RightA);
            BottomAnchor.Reposition(Bottom, BottomA);
            return this;
        }

        public Widget Reposition(Rect Offset, Rect Anchor)
        {
            LeftAnchor.Reposition(Offset.Left, Anchor.Left);
            TopAnchor.Reposition(Offset.Top, Anchor.Top);
            RightAnchor.Reposition(Offset.Right, Anchor.Right);
            BottomAnchor.Reposition(Offset.Bottom, Anchor.Bottom);
            return this;
        }

        public Widget Reposition(Rect NewPosition)
        {
            LeftAnchor.Reposition(NewPosition.Left);
            TopAnchor.Reposition(NewPosition.Top);
            RightAnchor.Reposition(NewPosition.Right);
            BottomAnchor.Reposition(NewPosition.Bottom);
            return this;
        }

        public Widget RepositionRelative(Rect NewPosition, Rect ParentBounds)
        {
            LeftAnchor.RepositionRelative(NewPosition.Left, ParentBounds.Left, ParentBounds.Right);
            TopAnchor.RepositionRelative(NewPosition.Top, ParentBounds.Top, ParentBounds.Bottom);
            RightAnchor.RepositionRelative(NewPosition.Right, ParentBounds.Left, ParentBounds.Right);
            BottomAnchor.RepositionRelative(NewPosition.Bottom, ParentBounds.Top, ParentBounds.Bottom);
            return this;
        }

        public Widget TL_DeprecateMe(float x, float y, AnchorType ax, AnchorType ay) //todo: deprecate these
        {
            LeftAnchor.RepositionDeprecateMe(x, ax);
            TopAnchor.RepositionDeprecateMe(y, ay);
            return this;
        }

        public Widget BR_DeprecateMe(float x, float y, AnchorType ax, AnchorType ay)
        {
            RightAnchor.RepositionDeprecateMe(x, ax);
            BottomAnchor.RepositionDeprecateMe(y, ay);
            return this;
        }

        public Widget Move(Rect Bounds)
        {
            LeftAnchor.Move(Bounds.Left);
            TopAnchor.Move(Bounds.Top);
            RightAnchor.Move(Bounds.Right);
            BottomAnchor.Move(Bounds.Bottom);
            return this;
        }

        public Widget MoveRelative(Rect Bounds, Rect ParentBounds)
        {
            LeftAnchor.MoveRelative(Bounds.Left, ParentBounds.Left, ParentBounds.Right);
            TopAnchor.MoveRelative(Bounds.Top, ParentBounds.Top, ParentBounds.Bottom);
            RightAnchor.MoveRelative(Bounds.Right, ParentBounds.Left, ParentBounds.Right);
            BottomAnchor.MoveRelative(Bounds.Bottom, ParentBounds.Top, ParentBounds.Bottom);
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

        public virtual void Dispose()
        {
            foreach (Widget w in Children)
            {
                w.Dispose();
            }
        }
    }
}
