using System.Collections.Generic;
using System.Drawing;
using Interlude.Interface.Animations;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets.Gameplay
{
    class JudgementCounter : GameplayWidget
    {
        List<AnimationSlider> flashes;

        public JudgementCounter(Interlude.Gameplay.ScoreTracker scoreTracker, Options.WidgetPosition pos) : base(scoreTracker, pos)
        {
            flashes = new List<AnimationSlider>();
            for (int i = 0; i < 6; i++)
            {
                flashes.Add(new AnimationSlider(0));
                Animation.Add(flashes[i]);
            }
            scoreTracker.OnHit += (k, j, d) => { flashes[j].Val = 1; };
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            float h = bounds.Height / 6f;
            float w = bounds.Width;
            float r = bounds.Top;
            for (int i = 0; i < 6; i++)
            {
                SpriteBatch.DrawRect(new Rect(bounds.Left, r, bounds.Right, r + h), Color.FromArgb((int)(((Color)scoreTracker.WidgetColor).A/255f * (80+(flashes[i]*140))), Game.Options.Theme.JudgeColors[i]));
                SpriteBatch.Font2.DrawTextToFill(Game.Options.Theme.Judges[i], new Rect(bounds.Left, r, bounds.Left + w * 0.75f, r + h), scoreTracker.WidgetColor);
                SpriteBatch.Font2.DrawJustifiedTextToFill(scoreTracker.Scoring.Judgements[i].ToString(), new Rect(bounds.Right - w * 0.25f, r, bounds.Right, r + h), scoreTracker.WidgetColor);
                r += h;
            }
            ScreenUtils.DrawFrame(bounds, scoreTracker.WidgetColor);
        }
    }
}
