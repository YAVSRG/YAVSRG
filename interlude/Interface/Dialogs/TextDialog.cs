using OpenTK;
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface.Dialogs
{
    class TextDialog : Dialog
    {
        string prompt;
        string val = "";
        InputMethod im;
        AnimationSlider slide;

        public TextDialog(string prompt, Action<string> action) : base(action)
        {
            this.prompt = prompt;
            Input.ChangeIM(im = new InputMethod((s) => { val = s; }, () => { return val; }, () => { }));
            Animation.Add(slide = new AnimationSlider(0) { Target = 1f });
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            if (Input.KeyTap(OpenTK.Input.Key.Enter, true))
            {
                im.Dispose();
                Input.ChangeIM(null);
                Close(val);
            }
        }

        public override void Draw(Rect bounds)
        {
            int a = (int)(slide * 255);
            float w = ScreenUtils.ScreenWidth * slide * 2;
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.DrawRect(new Rect(bounds.Left, -105, bounds.Left + w, -100), Color.FromArgb(a, Game.Screens.HighlightColor));
            SpriteBatch.DrawRect(new Rect(bounds.Left, 100, bounds.Left + w, 105), Color.FromArgb(a, Game.Screens.HighlightColor));
            Game.Screens.DrawChartBackground(new Rect(bounds.Right - w, -100, bounds.Right, 100), Color.FromArgb(a, Game.Screens.DarkColor));
            SpriteBatch.Font1.DrawCentredTextToFill(prompt, new Rect(bounds.Left, -100, bounds.Right, -50), Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawCentredTextToFill(val, new Rect(bounds.Left, -50, bounds.Right, 100), Game.Options.Theme.MenuFont);
        }
    }
}
