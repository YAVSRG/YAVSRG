using System;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    class BannerButton : SpriteButton
    {
        float T1, T2, s;

        public BannerButton(string label, Action onClick, float t1 = 0, float t2 = 1, float slant = 0.5f) : base("banner", label, onClick)
        {
            T1 = t1; T2 = t2;
            s = slant;
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            ScreenUtils.DrawParallelogramWithBG(bounds, s, color, color);
            SpriteBatch.Font1.DrawCentredTextToFill(text, bounds.SliceLeft(bounds.Width * T2).SliceRight(bounds.Width * (1 - T1)).ExpandY(-10), Game.Options.Theme.MenuFont, true, Game.Screens.DarkColor);
        }
    }
}
