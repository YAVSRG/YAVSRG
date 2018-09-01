using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface.Widgets.Gameplay
{
    class JudgementDisplay : GameplayWidget
    {
        List<AnimationSlider> flashes;

        public JudgementDisplay(YAVSRG.Gameplay.ScoreTracker st) : base(st)
        {
            flashes = new List<AnimationSlider>();
            for (int i = 0; i < 6; i++)
            {
                flashes.Add(new AnimationSlider(0));
                Animation.Add(flashes[i]);
            }
            st.OnHit += (k, j, d) => { flashes[j].Val = 1; };
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
            ScreenUtils.DrawFrame(bounds, 30f, scoreTracker.WidgetColor);
        }
    }
}
