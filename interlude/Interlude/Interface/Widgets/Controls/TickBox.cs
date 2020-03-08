using System;
using Prelude.Utilities;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    //GUI element that lets the user toggle the value of a boolean
    //The box is filled in if true and not if false
    class TickBox : Widget
    {
        string Label;
        SetterGetter<bool> Value;

        public TickBox(string label, SetterGetter<bool> value) : base()
        {
            Label = label;
            Value = value;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.Font1.DrawTextToFill(Label, bounds.SliceRight(bounds.Width - bounds.Height), Game.Options.Theme.MenuFont, true, System.Drawing.Color.Black);
            if (Value)
            {
                SpriteBatch.DrawRect(bounds.SliceLeft(bounds.Height).Expand(-15, -15), Game.Screens.BaseColor);
            }
            ScreenUtils.DrawFrame(bounds.SliceLeft(bounds.Height).Expand(-10, -10), System.Drawing.Color.White);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (ScreenUtils.CheckButtonClick(bounds))
            {
                Value.Set(!Value);
            }
        }
    }
}
