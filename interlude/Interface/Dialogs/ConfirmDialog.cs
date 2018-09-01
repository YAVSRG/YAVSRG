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

        public override void Draw(Rect bounds)
        {
            int a = (int)(slide * 255);
            float w = ScreenUtils.ScreenWidth * slide * 2;
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.DrawRect(new Rect(bounds.Left, -55, bounds.Left + w, -50), Color.FromArgb(a, Game.Screens.DarkColor));
            SpriteBatch.DrawRect(new Rect(bounds.Left, 50, bounds.Left + w, 55), Color.FromArgb(a, Game.Screens.DarkColor));
            Game.Screens.DrawChartBackground(new Rect(bounds.Right - w, -50, bounds.Right, 50), Color.FromArgb(a, Game.Screens.BaseColor));
            SpriteBatch.Font1.DrawCentredTextToFill(prompt, new Rect(bounds.Left, -50, bounds.Right, 50), Game.Options.Theme.MenuFont);
        }
    }
}
