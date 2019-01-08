using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface.Widgets
{
    public class FrameContainer : Widget
    {
        public Func<Color> FrameColor = () => Game.Screens.HighlightColor, BackColor = () => Game.Screens.DarkColor;
        public bool UseBackground = true;
        public float VerticalFade = 0, HorizontalFade = 0;
        public byte Frame = 255;
        public AnimationSlider Alpha;
        private DrawableFBO FBO;

        public FrameContainer() : base()
        {
            Animation.Add(Alpha = new AnimationSlider(1));
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            PreDraw(bounds);
            ScreenUtils.DrawFrame(bounds, 30f, FrameColor(), components: Frame);
            PostDraw(bounds);
            DrawWidgets(bounds);
        }

        protected void PreDraw(Rect bounds)
        {
            FBO = new DrawableFBO();
            if (UseBackground)
            {
                Game.Screens.DrawChartBackground(bounds, BackColor(), 2f);
            }
            else
            {
                SpriteBatch.DrawRect(bounds, BackColor());
            }
        }

        protected void PostDraw(Rect bounds)
        {
            //todo: cleanup
            Color c = Color.FromArgb((int)(255 * Alpha), Color.White);
            FBO.Unbind();
            SpriteBatch.DrawTilingTexture(FBO, bounds.SliceTop(VerticalFade).ExpandX(-HorizontalFade), ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, 0.5f, 0.5f, new[] { Color.Transparent, Color.Transparent, c, c });
            SpriteBatch.DrawTilingTexture(FBO, bounds.SliceBottom(VerticalFade).ExpandX(-HorizontalFade), ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, 0.5f, 0.5f, new[] { c, c, Color.Transparent, Color.Transparent });
            SpriteBatch.DrawTilingTexture(FBO, bounds.SliceLeft(HorizontalFade).ExpandY(-VerticalFade), ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, 0.5f, 0.5f, new[] { Color.Transparent, c, c, Color.Transparent });
            SpriteBatch.DrawTilingTexture(FBO, bounds.SliceRight(HorizontalFade).ExpandY(-VerticalFade), ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, 0.5f, 0.5f, new[] { c, Color.Transparent, Color.Transparent, c });
            SpriteBatch.DrawTilingTexture(FBO, bounds.ExpandX(-HorizontalFade).ExpandY(-VerticalFade), ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, 0.5f, 0.5f, new[] { c, c, c, c });
            FBO.Dispose();
        }
    }
}
