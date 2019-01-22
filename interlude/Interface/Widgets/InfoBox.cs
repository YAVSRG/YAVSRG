using System.Drawing;
using YAVSRG.Interface.Animations;
using YAVSRG.Graphics;

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

        public override void Draw(Rect bounds)
        {
            int f = (int)(255 * fade);
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            Game.Screens.DrawChartBackground(bounds, Color.FromArgb(f, 100, 100, 100));
            ScreenUtils.DrawFrame(bounds, 30f, Color.FromArgb(f,Color.White));
            SpriteBatch.Font1.DrawParagraph(text, 30f, bounds.Expand(-10,-10), Color.FromArgb(f,Game.Options.Theme.MenuFont));
        }
    }
}
