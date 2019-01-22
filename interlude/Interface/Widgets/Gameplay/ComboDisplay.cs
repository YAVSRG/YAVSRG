using System;
using YAVSRG.Gameplay;
using YAVSRG.Interface.Animations;
using YAVSRG.Graphics;

namespace YAVSRG.Interface.Widgets.Gameplay
{
    public class ComboDisplay : GameplayWidget
    {
        AnimationSlider size;
        int baseSize, bumpAmount, cbAmount, comboCap;
        float scaleWithCombo;

        public ComboDisplay(ScoreTracker scoreTracker, Options.WidgetPosition pos) : base(scoreTracker, pos)
        {
            baseSize = pos.GetValue("baseSize", 40);
            bumpAmount = pos.GetValue("hitBumpAmount", 5);
            cbAmount = pos.GetValue("missBumpAmount", 40);
            comboCap = pos.GetValue("comboCap", 1000);
            scaleWithCombo = pos.GetValue("scaleWithCombo", 0.02f);

            size = new AnimationSlider(baseSize);
            scoreTracker.OnHit += (x,y,z) =>
            {
                size.Val = baseSize + bumpAmount;
                if (scoreTracker.Scoring.Combo == 0)
                {
                    size.Val = baseSize + cbAmount;
                }
            };
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            float s = Math.Min(comboCap, scoreTracker.Scoring.Combo) * scaleWithCombo + size;
            SpriteBatch.Font1.DrawCentredText(scoreTracker.Scoring.Combo.ToString(), s, bounds.CenterX, bounds.Top - s / 2, scoreTracker.WidgetColor);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            size.Update();
        }
    }
}
