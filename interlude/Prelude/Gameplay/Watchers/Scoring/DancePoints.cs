using System;
using Prelude.Utilities;

namespace Prelude.Gameplay.Watchers.Scoring
{
    [DataTemplate("Judge", "Default", 4, "Min", 1, "Max", 10)]
    public class DancePoints : ScoreSystem
    {
        //DP is the old standard for scoring on Stepmania
        //It can be set to different difficulty settings called Judges with Judge 4 (J4) being the most common standard and J5 also fairly popular for competitive play etc
        public DancePoints(DataGroup Settings) : base("DP J"+Settings.GetValue("Judge", 4).ToString(), 6)
        {
            MaxPointsPerNote = 2;
            PointsPerJudgement = new int[] { 2, 2, 1, -4, -8, -8 };
            ComboBreakingJudgement = 3;
            int judge = Settings.GetValue("Judge", 4);
            float perfwindow = 45f / 6 * (10 - judge);
            JudgementWindows = new float[] {
                perfwindow * 0.5f,
                perfwindow,
                perfwindow * 2,
                perfwindow * 3,
                perfwindow * 4
            };
        }
    }
}
