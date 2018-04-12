using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface.Widgets
{
    class BannerButton : Button
    {
        AnchorPoint textPosition;

        public BannerButton(string label, Action onClick, AnchorType text = AnchorType.CENTER) : base("banner", label, onClick)
        {
            textPosition = new AnchorPoint(0,0,text,AnchorType.CENTER);
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            ScreenUtils.DrawBanner(icon, left, top, right, bottom, color);
            SpriteBatch.Font1.DrawJustifiedText(text, 40f, right - (bottom-top), top + 15, Game.Options.Theme.MenuFont);
        }
    }
}
