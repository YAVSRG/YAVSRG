using System;
using System.Drawing;
using OpenTK;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    class Logo : Widget
    {
        public float alpha;
        Animations.AnimationCounter animation;

        public Logo() : base()
        {
            Animation.Add(animation = new Animations.AnimationCounter(1000000, true));
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            float w = bounds.Width; //equal to height hopefully
            int a = (int)(255 * alpha);
            SpriteBatch.Draw(new RenderTarget(
                new Vector2(bounds.Left + 0.08f * w, bounds.Top + 0.09f * w), new Vector2(bounds.CenterX, bounds.CenterY + 0.26875f * w),
                new Vector2(bounds.CenterX, bounds.CenterY + 0.26875f * w), new Vector2(bounds.Right - 0.08f * w, bounds.Top + 0.09f * w),  Color.FromArgb(a, Color.DarkBlue)));
            SpriteBatch.Draw(new RenderTarget(
                new Vector2(bounds.Left + 0.08f * w, bounds.Top + 0.29f * w), new Vector2(bounds.Left + 0.22f * w, bounds.Top + 0.29f * w),
                new Vector2(bounds.CenterX, bounds.CenterY + 0.26875f * w), new Vector2(bounds.CenterX, bounds.CenterY + 0.46875f * w), Color.FromArgb(a, Color.DarkBlue)));
            SpriteBatch.Draw(new RenderTarget(
                new Vector2(bounds.Right - 0.08f * w, bounds.Top + 0.29f * w), new Vector2(bounds.Right - 0.22f * w, bounds.Top + 0.29f * w),
                new Vector2(bounds.CenterX, bounds.CenterY + 0.26875f * w), new Vector2(bounds.CenterX, bounds.CenterY + 0.46875f * w), Color.FromArgb(a, Color.DarkBlue)));

            SpriteBatch.Stencil(SpriteBatch.StencilMode.Create);

            SpriteBatch.Draw(new RenderTarget(
                new Vector2(bounds.Left + 0.1f * w, bounds.Top + 0.1f * w), new Vector2(bounds.CenterX, bounds.CenterY + 0.25f * w),
                new Vector2(bounds.CenterX, bounds.CenterY + 0.25f * w), new Vector2(bounds.Right - 0.1f * w, bounds.Top + 0.1f * w), Color.Transparent));
            SpriteBatch.Draw(new RenderTarget(
                new Vector2(bounds.Left + 0.1f * w, bounds.Top + 0.3f * w), new Vector2(bounds.Left + 0.2f * w, bounds.Top + 0.3f * w),
                new Vector2(bounds.CenterX, bounds.CenterY + 0.2875f * w), new Vector2(bounds.CenterX, bounds.CenterY + 0.45f * w), Color.Transparent));
            SpriteBatch.Draw(new RenderTarget(
                new Vector2(bounds.Right - 0.1f * w, bounds.Top + 0.3f * w), new Vector2(bounds.Right - 0.2f * w, bounds.Top + 0.3f * w),
                new Vector2(bounds.CenterX, bounds.CenterY + 0.2875f * w), new Vector2(bounds.CenterX, bounds.CenterY + 0.45f * w), Color.Transparent));

            SpriteBatch.AlphaTest(true);
            SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.GetTexture("logo"), bounds, Color.FromArgb(a, Color.White)));
            SpriteBatch.AlphaTest(false);

            SpriteBatch.Stencil(SpriteBatch.StencilMode.Draw);

            Game.Screens.DrawChartBackground(bounds, Color.FromArgb(a, Color.Aqua));
            SpriteBatch.DrawRect(bounds, Color.FromArgb((int)(alpha * 180), Color.Aqua));
            SpriteBatch.DrawTilingTexture("rain", bounds, 320, animation.value * -0.0006f, animation.value * -0.0007f, color: Color.FromArgb((int)(alpha * 80), Color.Blue));
            SpriteBatch.DrawTilingTexture("rain", bounds, 512, animation.value * -0.001f, animation.value * -0.0011f, color: Color.FromArgb((int)(alpha * 150), Color.Blue));
            SpriteBatch.DrawTilingTexture("rain", bounds, 800, animation.value * -0.0015f, animation.value * -0.0016f, color: Color.FromArgb((int)(alpha * 220), Color.Blue));

            float prev = 0;
            float m = bounds.Bottom - w * alpha * 0.5f;
            for (int i = 0; i < 32; i++) //draws the waveform
            {
                float level = 0;
                for (int t = 0; t < 8; t++)
                {
                    level += Game.Audio.WaveForm[i * 8 + t];
                }
                level *= 0.1f;
                SpriteBatch.Draw(new RenderTarget(
                new Vector2(bounds.Left + i * w/32, m - prev), new Vector2(bounds.Left + (i+1) * w/32, m - level),
                new Vector2(bounds.Left + (i+1) * w/32, bounds.Bottom), new Vector2(bounds.Left + (i) * w/32, bounds.Bottom), Color.FromArgb(a >> 1, Color.Blue)));
                prev = level;
            }

            SpriteBatch.Stencil(SpriteBatch.StencilMode.Disable);

            SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.GetTexture("logo"), bounds, Color.FromArgb(Math.Min(255, a * 2), Color.White)));
        }
    }
}
