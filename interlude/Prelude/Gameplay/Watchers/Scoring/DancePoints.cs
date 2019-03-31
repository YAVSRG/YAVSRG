using System;
using Prelude.Utilities;

namespace Prelude.Gameplay.Watchers.Scoring
{
    public class DancePoints : ScoreSystem
    {
        public DancePoints(DataGroup Settings) : base("DP J"+Settings.GetValue("Judge", 4).ToString(), 6)
        {
            MaxPointsPerNote = 2;
            PointsPerJudgement = new int[] { 2, 2, 1, -4, -8, -8 };
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
