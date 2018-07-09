using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Animations;
using YAVSRG.Interface.Widgets;

namespace YAVSRG.Interface.Dialogs
{
    class ConfirmDialog : Dialog
    {
        string prompt;
        AnimationSlider slide;

        public ConfirmDialog(string prompt, Action<string> action) : base(action)
        {
            this.prompt = prompt;
            AddChild(new BannerButton("Yes", () => { Close("Y"); }).PositionTopLeft(-100, 100, AnchorType.MIN, AnchorType.CENTER).PositionBottomRight(ScreenUtils.ScreenWidth - 100, 200, AnchorType.MIN, AnchorType.CENTER));
            AddChild(new BannerButton("No", () => { Close("N"); }).PositionTopLeft(ScreenUtils.ScreenWidth - 100, 100, AnchorType.MAX, AnchorType.CENTER).PositionBottomRight(-100, 200, AnchorType.MAX, AnchorType.CENTER));
            Animation.Add(slide = new AnimationSlider(0) { Target = 1f });
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            int a = (int)(slide * 255);
            float w = ScreenUtils.ScreenWidth * slide * 2;
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawRect(left, -55, left + w, -50, Color.FromArgb(a, Game.Screens.DarkColor));
            SpriteBatch.DrawRect(left, 50, left + w, 55, Color.FromArgb(a, Game.Screens.DarkColor));
            Game.Screens.DrawChartBackground(right - w, -50, right, 50, Color.FromArgb(a, Game.Screens.BaseColor));
            SpriteBatch.Font1.DrawCentredTextToFill(prompt, left, -50, right, 50, Game.Options.Theme.MenuFont);
        }
    }
}
