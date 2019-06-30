using System;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    class BannerButton : SpriteButton
    {
        string label;
        public float TextLeftPercent = 0f;
        public float TextRightPercent = 1f;
        public float Slant = 0.5f;

        public BannerButton(string label, Action onClick, Func<Bind> bind) : base("", onClick, bind)
        {
            this.label = label;
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            ScreenUtils.DrawParallelogramWithBG(bounds, Slant, color, color);
            SpriteBatch.Font1.DrawCentredTextToFill(label, bounds.SliceLeft(bounds.Width * TextRightPercent).SliceRight(bounds.Width * (1 - TextLeftPercent)).ExpandY(-10), Game.Options.Theme.MenuFont, true, Game.Screens.DarkColor);
        }
    }
}
