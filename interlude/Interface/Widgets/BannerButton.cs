using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    class BannerButton : Button
    {
        public BannerButton(string label, Action onClick) : base("banner", label, onClick)
        {

        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            ScreenUtils.DrawBanner(icon, left, top, right, bottom, color);
            SpriteBatch.DrawJustifiedText(text, 40f, right - (bottom-top), top + 15, Game.Options.Theme.MenuFont);
        }
    }
}
