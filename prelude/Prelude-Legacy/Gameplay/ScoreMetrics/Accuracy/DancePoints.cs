using Prelude.Utilities;

namespace Prelude.Gameplay.ScoreMetrics.Accuracy
{
    [DataTemplate("Judge", "Default", 4, "Min", 1, "Max", 10)]
    [DataTemplate("Enable Ridiculous", "Default", false)]
    public class DancePoints : ScoreSystem
    {
        //DP is the old standard for scoring on Stepmania
        //It can be set to different difficulty settings called Judges with Judge 4 (J4) being the most common standard and J5 also fairly popular for competitive play etc
        public DancePoints(DataGroup Settings) : base("DP J" + Settings.GetValue("Judge", 4).ToString())
        {
            int judge = Settings.GetValue("Judge", 4);
            float perfwindow = 45f / 6 * (10 - judge);
            MaxPointsPerNote = 2;
            PointsPerJudgement = new int[] { 2, 2, 2, 1, -4, -8, -8 };
            ComboBreakingJudgement = HitType.GOOD;
            if (Settings.GetValue("Enable Ridiculous", false))
            {
                HitTypes = new[]
                {
                    HitType.RIDICULOUS,
                    HitType.MARVELLOUS, HitType.PERFECT, HitType.GREAT,
                    HitType.GOOD, HitType.BAD, HitType.MISS
                };
                JudgementWindows = new float[] {
                    perfwindow * 0.25f,
                    perfwindow * 0.5f,
                    perfwindow,
                    perfwindow * 2,
                    perfwindow * 3,
                    perfwindow * 4
                };
            }
            else
            {
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
}
