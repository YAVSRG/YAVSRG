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

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            if (Input.KeyTap(OpenTK.Input.Key.Enter, true))
            {
                im.Dispose();
                Input.ChangeIM(null);
                Close(val);
            }
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            int a = (int)(slide * 255);
            float w = ScreenUtils.ScreenWidth * slide * 2;
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawRect(left, -105, left + w, -100, Color.FromArgb(a, Game.Screens.HighlightColor));
            SpriteBatch.DrawRect(left, 100, left + w, 105, Color.FromArgb(a, Game.Screens.HighlightColor));
            Game.Screens.DrawChartBackground(right - w, -100, right, 100, Color.FromArgb(a, Game.Screens.DarkColor));
            SpriteBatch.Font1.DrawCentredTextToFill(prompt, left, -100, right, -50, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawCentredTextToFill(val, left, -50, right, 100, Game.Options.Theme.MenuFont);
        }
    }
}
