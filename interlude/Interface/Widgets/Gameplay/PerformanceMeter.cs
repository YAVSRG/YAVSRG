using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets.Gameplay
{
    public class PerformanceMeter : GameplayWidget
    {
        double value = 1f;
        int i = 0;
        int lasti;
        double nps;

        public PerformanceMeter(YAVSRG.Gameplay.ScoreTracker scoreTracker): base(scoreTracker)
        {
            scoreTracker.OnHit += HandleHit;
            lasti = scoreTracker.c.Notes.Count;
        }

        void HandleHit(int column, int judge, float delta)
        {
            //nothing
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            SpriteBatch.DrawRect(new Rect(ScreenUtils.ScreenWidth - 50, (float)(ScreenUtils.ScreenHeight - value * 25), ScreenUtils.ScreenWidth, ScreenUtils.ScreenHeight), System.Drawing.Color.White);
            SpriteBatch.Font2.DrawJustifiedText(Utils.RoundNumber(value), 40f, ScreenUtils.ScreenWidth - 100, 0, System.Drawing.Color.White);
            SpriteBatch.Font2.DrawJustifiedText(Utils.RoundNumber(nps), 40f, ScreenUtils.ScreenWidth - 100, 200, System.Drawing.Color.White);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            float now = (float)Game.Audio.Now();
            value *= 0.995f;
            while (i < lasti && scoreTracker.c.Notes.Points[i].Offset <= now)
            {
                for (byte k = 0; k < scoreTracker.c.Notes.Points[i].Count; k++)
                {
                    value *= func(Game.Gameplay.ChartDifficulty.PhysicalData[i] / value);
                }
                i++;
            }
            nps = 0;
            for (int index = scoreTracker.c.Notes.GetNextIndex(now - 100); index < scoreTracker.c.Notes.Count && scoreTracker.c.Notes.Points[index].Offset < now + 100; index++)
            {
                nps += scoreTracker.c.Notes.Points[index].Count * (1 - Math.Pow((scoreTracker.c.Notes.Points[index].Offset - now) / 100f, 2));
            }
        }

        double func(double ratio)
        {
            return (1 + 0.05f * ratio);
        }
    }
}
