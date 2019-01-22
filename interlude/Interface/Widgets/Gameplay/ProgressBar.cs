using YAVSRG.Gameplay;
using YAVSRG.Graphics;

namespace YAVSRG.Interface.Widgets.Gameplay
{
    public class ProgressBar : GameplayWidget
    {
        public ProgressBar(ScoreTracker scoreTracker, Options.WidgetPosition pos) : base(scoreTracker, pos)
        {
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.DrawRect(bounds, Game.Screens.DarkColor);
            float temp;
            float x = bounds.Left;
            for(int i = 5; i >= 0; i--)
            {
                temp = scoreTracker.Scoring.Judgements[i] * bounds.Width / scoreTracker.MaxCombo;
                SpriteBatch.DrawRect(new Rect(x, bounds.Top, x + temp, bounds.Bottom), Game.Options.Theme.JudgeColors[i]);
                x += temp;
            }
        }
    }
}
