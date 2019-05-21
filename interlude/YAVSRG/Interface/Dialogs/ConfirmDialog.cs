using System;
using System.Drawing;
using Interlude.Interface.Animations;
using Interlude.Interface.Widgets;
using Interlude.Graphics;

namespace Interlude.Interface.Dialogs
{
    //todo: make this not look garbo
    public class ConfirmDialog : Dialog
    {
        string prompt;
        AnimationSlider slide;

        public ConfirmDialog(string prompt, Action<string> action) : base(action)
        {
            this.prompt = prompt;
            AddChild(new BannerButton("Yes", () => { Close("Y"); }).Reposition(0, 0, 100, 0.5f, -ScreenUtils.ScreenWidth / 2, 0.5f, 200, 0.5f).Move(new Rect(0,100,-100,200)));
            AddChild(new BannerButton("No", () => { Close("N"); }, 0, 1, -0.5f).Reposition(ScreenUtils.ScreenWidth / 2, 0.5f, 100, 0.5f, 0, 1, 200, 0.5f).Move(new Rect(100, 100, 0, 200)));
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
            SpriteBatch.Font1.DrawCentredTextToFill(prompt, new Rect(bounds.Left, -50, bounds.Right, 50), Color.FromArgb(a, Game.Options.Theme.MenuFont));
        }
    }
}
