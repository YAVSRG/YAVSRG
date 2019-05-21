using System;
using System.Collections.Generic;
using Prelude.Gameplay.Watchers;
using Prelude.Utilities;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    public class ObjectSelector<T> : FlowContainer where T : class
    {
        List<T> Target;

        class SelectableCard : Widget
        {
            T Obj;
            Func<T> Highlight;

            public SelectableCard(string name, T obj, Func<T> selected)
            {
                Obj = obj;
                Highlight = selected;
                AddChild(new TextBox(name, AnchorType.MIN, 0, true, System.Drawing.Color.White, System.Drawing.Color.Black));
            }

            public override void Draw(Rect bounds)
            {
                bounds = GetBounds(bounds);
                SpriteBatch.DrawRect(bounds, Highlight() == Obj ? Game.Screens.HighlightColor : Game.Screens.BaseColor);
                DrawWidgets(bounds);
            }
        }

        public ObjectSelector(List<T> List, Func<T,string> GetName, Func<T> GetSelected, Action<int> OnSelect, Func<T> OnCreate, Action OnModify, Action OnDelete)
        {
            Target = List;
            AddChild(new SpriteButton("buttonimport", "Add", () => {
                var o = OnCreate();
                AddChild(new SelectableCard(GetName(o), o, GetSelected)); 
                }).PositionBottomRight(50, 50, AnchorType.MIN, AnchorType.MIN));

            AddChild(new SpriteButton("buttonclose", "Delete", () => {
                OnDelete();
                Children.RemoveAt(List.IndexOf(GetSelected()));
            }).PositionTopLeft(50, 50, AnchorType.MAX, AnchorType.MAX));

            for (int i = 0; i < List.Count; i++)
            {
                AddChild(new SelectableCard(Game.Options.Profile.GetScoreSystem(i).Name, List[i], GetSelected));
            }
        }
    }
}
