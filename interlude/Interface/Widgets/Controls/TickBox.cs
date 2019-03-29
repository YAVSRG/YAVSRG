using System;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    class TickBox : Widget
    {
        bool Ticked;
        Action<bool> Set;
        string Label;

        public TickBox(string label, bool start, Action<bool> set) : base()
        {
            Label = label;
            Set = set;
            Ticked = start;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.Font1.DrawTextToFill(Label, bounds.SliceRight(bounds.Width - bounds.Height), Game.Options.Theme.MenuFont, true, System.Drawing.Color.Black);
            if (Ticked)
            {
                SpriteBatch.DrawRect(bounds.SliceLeft(bounds.Height).Expand(-15, -15), Game.Screens.BaseColor);
            }
            ScreenUtils.DrawFrame(bounds.SliceLeft(bounds.Height).Expand(-10, -10), 20, System.Drawing.Color.White);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (ScreenUtils.CheckButtonClick(bounds))
            {
                Ticked = !Ticked;
                Set(Ticked);
            }
        }
    }
}
