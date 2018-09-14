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
            SpriteBatch.DrawRect(new Rect(ScreenUtils.ScreenWidth - 50, (float)(ScreenUtils.ScreenHeight - value * 50), ScreenUtils.ScreenWidth, ScreenUtils.ScreenHeight), System.Drawing.Color.White);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            float now = (float)Game.Audio.Now();
            value *= 0.995f;
            while (i < lasti && scoreTracker.c.Notes.Points[i].Offset <= now)
            {
                value *= func(Game.Gameplay.ChartDifficulty.PhysicalData[i] / value);
                i++;
            }
        }

        double func(double ratio)
        {
            return (1 + 0.05f * ratio);
        }
    }
}
