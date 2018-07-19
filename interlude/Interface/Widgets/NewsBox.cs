using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Net.Web;

namespace YAVSRG.Interface.Widgets
{
    public class NewsBox : Widget
    {
        GithubReleaseData data;
        Animations.AnimationSlider slide;
        Animations.AnimationCounter loading;

        public NewsBox() : base()
        {
            WebUtils.DownloadJsonObject<GithubReleaseData>("https://api.github.com/repos/percyqaz/YAVSRG/releases/latest", (d) => { ReceieveData(d); });
            Animation.Add(slide = new Animations.AnimationSlider(0));
            Animation.Add(loading = new Animations.AnimationCounter(1000000,true));
            PositionTopLeft(0, 0, AnchorType.MAX, AnchorType.MIN);
            PositionBottomRight(-400, 0, AnchorType.MAX, AnchorType.MAX);
            A.Target(400, 0);
            B.Target(0, 0);
        }

        public void ReceieveData(GithubReleaseData data)
        {
            this.data = data;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            left -= 200 * slide;
            int h = 60;
            float t = top + h + (bottom - top - h * 2) * slide;
            SpriteBatch.DrawRect(left, top + h, right, t, System.Drawing.Color.FromArgb(100, Game.Screens.BaseColor));
            ScreenUtils.DrawParallelogramWithBG(left, top, right, top + h, -0.5f, Game.Screens.DarkColor, Game.Screens.BaseColor);
            SpriteBatch.Font1.DrawCentredTextToFill("Updates & News", left, top, right, top + h, Game.Options.Theme.MenuFont);
            ScreenUtils.DrawParallelogramWithBG(left + h * 0.5f * (1 - slide), t, right, t + h, slide - 0.5f, Game.Screens.DarkColor, Game.Screens.BaseColor);
            SpriteBatch.Font1.DrawCentredTextToFill("Read the Wiki", left + h * 0.5f, t, right, t + h, Game.Options.Theme.MenuFont);

            SpriteBatch.StencilMode(1);
            SpriteBatch.DrawRect(left, top + h, right, t, System.Drawing.Color.Transparent);
            SpriteBatch.StencilMode(2);
            if (data != null)
            {
                SpriteBatch.Font1.DrawCentredTextToFill(data.name, left, top + h, right, top + 2 * h, Game.Options.Theme.MenuFont);
                SpriteBatch.Font2.DrawParagraph(data.body, 20f, left + 10, top + h * 2, left + 600, t, Game.Options.Theme.MenuFont);
            }
            else
            {
                SpriteBatch.Font1.DrawCentredTextToFill("Loading...", left, top + h, right, top + 2 * h, Game.Options.Theme.MenuFont);
                ScreenUtils.DrawLoadingAnimation(100f * slide, (left + right) / 2, (top + t) / 2, loading.value * 0.01f);
            }
            SpriteBatch.StencilMode(0);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            left -= 200 * slide;
            int h = 60;
            float t = top + h + (bottom - top - h * 2) * slide;
            if (ScreenUtils.CheckButtonClick(left, top, right, top + h))
            {
                slide.Target = 1 - slide.Target;
            }
            else if (ScreenUtils.CheckButtonClick(left + h * 0.5f * (1 - slide), t, right, t + h))
            {
                System.Diagnostics.Process.Start("https://github.com/percyqaz/YAVSRG/wiki");
            }
        }
    }
}
