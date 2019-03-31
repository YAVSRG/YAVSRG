using System;
using Prelude.Utilities;

namespace Prelude.Gameplay.Watchers.Scoring
{
    public class StandardScoring : ScoreSystem
    {
        public StandardScoring(DataGroup Settings) : base("SC", 6)
        {
            MaxPointsPerNote = 10;
            PointsPerJudgement = new int[] { 10, 9, 5, 1, -8, 0 };
            float perfwindow = 45f;
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
