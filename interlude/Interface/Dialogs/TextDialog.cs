using System;
using System.Drawing;
using YAVSRG.IO;
using YAVSRG.Graphics;

namespace YAVSRG.Interface.Dialogs
{
    class TextDialog : FadeDialog
    {
        string prompt;
        InputMethod im;

        public TextDialog(string prompt, Action<string> action) : base(action)
        {
            PositionTopLeft(0, -100, AnchorType.MIN, AnchorType.CENTER).PositionBottomRight(0, 100, AnchorType.MAX, AnchorType.CENTER);
            this.prompt = prompt;
            Input.ChangeIM(im = new InputMethod((s) => { Output = s; }, () => { return Output; }, () => { }));
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            if (Input.KeyTap(OpenTK.Input.Key.Enter, true))
            {
                OnClosing();
            }
        }

        public override void Draw(Rect bounds)
        {
            int a = (int)(Fade * 255);
            float w = ScreenUtils.ScreenWidth * Fade * 2;
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.DrawRect(new Rect(bounds.Left, -105, bounds.Left + w, -100), Color.FromArgb(a, Game.Screens.HighlightColor));
            SpriteBatch.DrawRect(new Rect(bounds.Left, 100, bounds.Left + w, 105), Color.FromArgb(a, Game.Screens.HighlightColor));
            Game.Screens.DrawChartBackground(new Rect(bounds.Right - w, -100, bounds.Right, 100), Color.FromArgb(a, Game.Screens.DarkColor));
            SpriteBatch.Font1.DrawCentredTextToFill(prompt, new Rect(bounds.Left, -100, bounds.Right, -50), Color.FromArgb(a,Game.Options.Theme.MenuFont));
            SpriteBatch.Font2.DrawCentredTextToFill(Output, new Rect(bounds.Left, -50, bounds.Right, 100), Color.FromArgb(a,Game.Options.Theme.MenuFont));
        }

        protected override void OnClosing()
        {
            base.OnClosing();
            im.Dispose();
            Input.ChangeIM(null);
        }
    }
}
