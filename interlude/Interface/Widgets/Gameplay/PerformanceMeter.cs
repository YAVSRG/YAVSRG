using System;
using YAVSRG.Gameplay.DifficultyRating;
using YAVSRG.Graphics;

namespace YAVSRG.Interface.Widgets.Gameplay
{
    public class PerformanceMeter : GameplayWidget
    {
        double value = 1f;
        int i = 1;
        int lasti;
        double nps;

        public PerformanceMeter(YAVSRG.Gameplay.ScoreTracker scoreTracker) : base(scoreTracker, new Options.WidgetPosition() { Enable = true })
        {
            scoreTracker.OnHit += HandleHit;
            lasti = scoreTracker.Chart.Notes.Count;
        }

        void HandleHit(int column, int judge, float delta)
        {
            //nothing
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            SpriteBatch.DrawRect(new Rect(ScreenUtils.ScreenWidth - 100, (float)(ScreenUtils.ScreenHeight - value * 25), ScreenUtils.ScreenWidth - 50, ScreenUtils.ScreenHeight), System.Drawing.Color.White);
            SpriteBatch.DrawRect(new Rect(ScreenUtils.ScreenWidth - 50, (float)(ScreenUtils.ScreenHeight - Game.Gameplay.ChartDifficulty.OverallPhysical[i - 1] * 25), ScreenUtils.ScreenWidth, ScreenUtils.ScreenHeight), System.Drawing.Color.White);
            SpriteBatch.Font2.DrawJustifiedText(Utils.RoundNumber(value), 40f, ScreenUtils.ScreenWidth - 200, 0, System.Drawing.Color.White);
            SpriteBatch.Font2.DrawJustifiedText(Utils.RoundNumber(nps), 40f, ScreenUtils.ScreenWidth - 200, 100, System.Drawing.Color.White);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            float now = (float)Game.Audio.Now();
            value *= CalcUtils.StaminaDecayFunc(16.7f);
            while (i < lasti && scoreTracker.Chart.Notes.Points[i].Offset <= now)
            {
                for (byte k = 0; k < scoreTracker.Chart.Notes.Points[i].Count; k++)
                {
                    CalcUtils.StaminaAlgorithm(ref value, Game.Gameplay.ChartDifficulty.OverallPhysical[i], scoreTracker.Chart.Notes.Points[i].Offset - scoreTracker.Chart.Notes.Points[i - 1].Offset);
                }
                i++;
            }
            nps = 0;
            for (int index = scoreTracker.Chart.Notes.GetNextIndex(now - 100); index < scoreTracker.Chart.Notes.Count && scoreTracker.Chart.Notes.Points[index].Offset < now + 100; index++)
            {
                nps += scoreTracker.Chart.Notes.Points[index].Count * (1 - Math.Pow((scoreTracker.Chart.Notes.Points[index].Offset - now) / 100f, 2));
            }
        }
    }
}
