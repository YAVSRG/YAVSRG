using System;
using System.Collections.Generic;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    public class ObjectSelector<T> : FlowContainer where T : class
    {
        List<T> Target;
        Func<T, string> GetName;
        Func<int> GetSelected;
        Action<int> Select;
        Action Create, Delete, Modify, RightClick;

        class SelectableCard : Widget
        {
            T Obj;
            protected new virtual ObjectSelector<T> Parent
            {
                get { return (ObjectSelector<T>)base.Parent; }
            }
            string label;

            public SelectableCard(T obj)
            {
                Obj = obj;
            }

            public override void Draw(Rect bounds)
            {
                base.Draw(bounds);
                bounds = GetBounds(bounds);
                SpriteBatch.DrawRect(bounds, Parent.Target[Parent.GetSelected()] == Obj ? Game.Screens.BaseColor : Game.Screens.DarkColor);
                if (label == null) label = Parent.GetName(Obj);
                SpriteBatch.Font1.DrawCentredTextToFill(label, bounds, System.Drawing.Color.White, true, System.Drawing.Color.Black);
            }

            public override void Update(Rect bounds)
            {
                base.Update(bounds);
                bounds = GetBounds(bounds);
                if (ScreenUtils.MouseOver(bounds))
                {
                    if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
                    {
                        Parent.Select(Parent.Target.IndexOf(Obj));
                    }
                    else if (Input.MouseClick(OpenTK.Input.MouseButton.Right))
                    {
                        Parent.Select(Parent.Target.IndexOf(Obj));
                        Parent.RightClick();
                        label = Parent.GetName(Obj);
                    }
                }
            }
        }

        public ObjectSelector(List<T> list, Func<T,string> getName, Func<int> getSelected, Action<int> select, Action create, Action delete, Action modify, Action rightclick)
        {
            Target = list;
            GetName = getName;
            GetSelected = getSelected;
            Select = select;
            RightClick = rightclick;
            Create = create;
            Delete = delete;
            Modify = modify;

            Refresh();
        }

        public void Refresh()
        {
            Children.Clear();

            AddChild(new SpriteButton("buttonimport", () =>
            {
                Create();
                Refresh();
            }, null)
            { Tooltip = "Create new" }.Reposition(0, 0, 0, 0, 50, 0, 50, 0));

            AddChild(new SpriteButton("buttonclose", () =>
            {
                Delete();
                Refresh();
            }, null)
            { Tooltip = "Delete selected" }.Reposition(0, 0, 0, 0, 50, 0, 50, 0));

            AddChild(new SpriteButton("buttonoptions", () =>
            {
                Modify();
                Refresh();
            }, null)
            { Tooltip = "Configure selected" }.Reposition(0, 0, 0, 0, 50, 0, 50, 0));

            for (int i = 0; i < Target.Count; i++)
            {
                AddChild(new SelectableCard(Target[i])
                    .Reposition(0, 0, 0, 0, 0, 1, 50, 0));
            }
        }
    }
}
