using Prelude.Utilities;

namespace Prelude.Gameplay.ScoreMetrics.Accuracy
{
    public class StandardScoring : ScoreSystem
    {
        public StandardScoring(DataGroup Settings) : base("SC")
        {
            MaxPointsPerNote = 10;
            PointsPerJudgement = new int[] { 10, 10, 9, 5, 1, -8, 0, 0, 0 };
            ComboBreakingJudgement = HitType.GOOD;
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
