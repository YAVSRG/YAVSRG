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

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            float h = (bottom - top) / 6f;
            float w = (right - left);
            float r = top;
            for (int i = 0; i < 6; i++)
            {
                SpriteBatch.DrawRect(left, r, right, r + h, Color.FromArgb((int)(((Color)scoreTracker.WidgetColor).A/255f * (80+(flashes[i]*140))), Game.Options.Theme.JudgeColors[i]));
                SpriteBatch.Font2.DrawTextToFill(Game.Options.Theme.Judges[i], left, r, left + w * 0.75f, r + h, scoreTracker.WidgetColor);
                SpriteBatch.Font2.DrawJustifiedTextToFill(scoreTracker.Scoring.Judgements[i].ToString(), right - w * 0.25f, r, right, r + h, scoreTracker.WidgetColor);
                r += h;
            }
            SpriteBatch.DrawFrame(left, top, right, bottom, 15f, scoreTracker.WidgetColor);
        }
    }
}
