using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface.Widgets
{
    public class InfoBox : Widget
    {
        AnimationSlider fade;
        string text;

        public InfoBox() : base()
        {
            Animation.Add(fade = new AnimationSlider(0f));
            text = "";
        }

        public void SetText(string s)
        {
            if (s.Length > 0)
            {
                text = s;
                fade.Target = 1f;
            }
            else
            {
                fade.Target = 0f;
            }
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            int f = (int)(255 * fade);
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            Game.Screens.DrawChartBackground(left, top, right, bottom, Color.FromArgb(f, 100, 100, 100));
            SpriteBatch.DrawFrame(left, top, right, bottom, 20f, Color.FromArgb(f,Color.White));
            SpriteBatch.Font1.DrawParagraph(text, 30f, left+10, top+10, right-10, bottom-10, Color.FromArgb(f,Game.Options.Theme.MenuFont));
        }
    }
}
