using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Widgets;

namespace YAVSRG.Interface.Dialogs
{
    class ConfirmDialog : Dialog
    {
        string prompt;
        Sprite banner;

        public ConfirmDialog(string prompt, Action<string> action) : base(action)
        {
            this.prompt = prompt;
            banner = Content.LoadTextureFromAssets("banner");
            PositionTopLeft(ScreenUtils.ScreenWidth, -50, AnchorType.CENTER, AnchorType.CENTER);
            PositionBottomRight(ScreenUtils.ScreenWidth+100, 50, AnchorType.CENTER, AnchorType.CENTER);
            A.Target(-ScreenUtils.ScreenWidth, -50);
            AddChild(new BannerButton("Yes", () => { Close("Y"); }).PositionTopLeft(-100, 100, AnchorType.MIN, AnchorType.MAX).PositionBottomRight(ScreenUtils.ScreenWidth, 200, AnchorType.MIN, AnchorType.MAX));
            AddChild(new BannerButton("No", () => { Close("N"); }).PositionTopLeft(ScreenUtils.ScreenWidth, 100, AnchorType.MAX, AnchorType.MAX).PositionBottomRight(-100, 200, AnchorType.MAX, AnchorType.MAX));

        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            ScreenUtils.DrawBanner(banner, left, top, right, bottom, System.Drawing.Color.Azure);
            SpriteBatch.DrawCentredTextToFill(prompt, left, top, right, top + 100, Game.Options.Theme.MenuFont);
            DrawWidgets(left, top, right, bottom);
        }
    }
}
