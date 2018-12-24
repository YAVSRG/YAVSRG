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
        bool updateAvailable;

        public NewsBox() : base()
        {
            WebUtils.DownloadJsonObject<GithubReleaseData>("https://api.github.com/repos/percyqaz/YAVSRG/releases/latest", (d) => { ReceieveData(d); });
            Animation.Add(slide = new Animations.AnimationSlider(0));
            Animation.Add(loading = new Animations.AnimationCounter(1000000, true));
            PositionTopLeft(0, 0, AnchorType.MAX, AnchorType.MIN);
            PositionBottomRight(-400, 0, AnchorType.MAX, AnchorType.MAX);
            Move(new Rect(400, 0, 0, 0), false);
        }

        public void ReceieveData(GithubReleaseData data)
        {
            this.data = data;
            updateAvailable = !Game.Version.Contains(data.tag_name);
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            bounds.Left -= 200 * slide;
            int h = 60;
            float t = bounds.Top + h + (bounds.Height - h * 2) * slide;
            SpriteBatch.DrawRect(new Rect(bounds.Left, bounds.Top + h, bounds.Right, t), System.Drawing.Color.FromArgb(127, Game.Screens.DarkColor));
            ScreenUtils.DrawParallelogramWithBG(new Rect(bounds.Left, bounds.Top, bounds.Right, bounds.Top + h), -0.5f, Game.Screens.DarkColor, Game.Screens.BaseColor);
            SpriteBatch.Font1.DrawCentredTextToFill("Updates & News", new Rect(bounds.Left, bounds.Top, bounds.Right, bounds.Top + h), updateAvailable ? System.Drawing.Color.Yellow : Game.Options.Theme.MenuFont);
            ScreenUtils.DrawParallelogramWithBG(new Rect(bounds.Left + h * 0.5f * (1 - slide), t, bounds.Right, t + h), slide - 0.5f, Game.Screens.DarkColor, Game.Screens.BaseColor);
            SpriteBatch.Font1.DrawCentredTextToFill("Read the Wiki", new Rect(bounds.Left + h * 0.5f, t, bounds.Right, t + h), Game.Options.Theme.MenuFont);

            SpriteBatch.Stencil(SpriteBatch.StencilMode.Create);
            SpriteBatch.DrawRect(new Rect(bounds.Left, bounds.Top + h, bounds.Right, t), System.Drawing.Color.Transparent);
            SpriteBatch.Stencil(SpriteBatch.StencilMode.Draw);
            if (data != null)
            {
                SpriteBatch.Font1.DrawCentredTextToFill(data.name, new Rect(bounds.Left, bounds.Top + h, bounds.Right, bounds.Top + 2 * h), Game.Options.Theme.MenuFont);
                SpriteBatch.Font2.DrawParagraph(data.body, 20f, new Rect(bounds.Left + 10, bounds.Top + h * 2, bounds.Left + 600, t), Game.Options.Theme.MenuFont);
            }
            else
            {
                SpriteBatch.Font1.DrawCentredTextToFill("Loading...", new Rect(bounds.Left, bounds.Top + h, bounds.Right, bounds.Top + 2 * h), Game.Options.Theme.MenuFont);
                ScreenUtils.DrawLoadingAnimation(100f * slide, bounds.CenterX, (bounds.Top + t) / 2, loading.value * 0.01f, 255);
            }
            SpriteBatch.Stencil(SpriteBatch.StencilMode.Disable);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            bounds.Left -= 200 * slide;
            int h = 60;
            float t = bounds.Top + h + (bounds.Height - h * 2) * slide;
            if (ScreenUtils.CheckButtonClick(new Rect(bounds.Left, bounds.Top, bounds.Right, bounds.Top + h)))
            {
                slide.Target = 1 - slide.Target;
            }
            else if (ScreenUtils.CheckButtonClick(new Rect(bounds.Left + h * 0.5f * (1 - slide), t, bounds.Right, t + h)))
            {
                System.Diagnostics.Process.Start("https://github.com/percyqaz/YAVSRG/wiki");
            }
        }
    }
}
