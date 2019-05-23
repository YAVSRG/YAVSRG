using System;
using System.Drawing;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface.Dialogs
{
    public class TextDialog : FadeDialog
    {
        string prompt;
        string text = "";
        InputMethod im;

        public TextDialog(string prompt, Action<string> action) : base(action)
        {
            TL_DeprecateMe(100, -70, AnchorType.MIN, AnchorType.CENTER).BR_DeprecateMe(100, 70, AnchorType.MAX, AnchorType.CENTER);
            this.prompt = prompt;
            Input.ChangeIM(im = new InputMethod((s) => { text = s; }, () => { return text; }, () => { }));
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            if (Input.KeyTap(Game.Options.General.Binds.Select, true))
            {
                OnClosing();
                Output = text;
            }
            if (Input.KeyTap(Game.Options.General.Binds.Exit, true))
            {
                OnClosing();
            }
        }

        public override void Draw(Rect bounds)
        {
            int a = (int)(Fade * 255);
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            Game.Screens.DrawChartBackground(bounds, Color.FromArgb(a, Game.Screens.DarkColor));
            ScreenUtils.DrawFrame(bounds, Color.FromArgb(a, Game.Screens.HighlightColor));
            SpriteBatch.Font1.DrawCentredTextToFill(prompt, new Rect(bounds.Left, -100, bounds.Right, -60), Color.FromArgb(a, Game.Options.Theme.MenuFont));
            SpriteBatch.Font2.DrawCentredTextToFill(text, new Rect(bounds.Left, -60, bounds.Right, 70), Color.FromArgb(a, Game.Options.Theme.MenuFont));
        }

        protected override void OnClosing()
        {
            base.OnClosing();
            im.Dispose();
            Input.ChangeIM(null);
        }
    }
}
