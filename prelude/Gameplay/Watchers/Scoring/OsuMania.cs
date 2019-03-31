using System;
using Prelude.Utilities;

namespace Prelude.Gameplay.Watchers.Scoring
{
    public class OsuMania : ScoreSystem
    {
        public OsuMania(DataGroup Settings) : base("o!m OD" + Math.Round(Settings.GetValue("OD", 9f), 1).ToString(), 6)
        {
            MaxPointsPerNote = 300;
            PointsPerJudgement = new int[] { 300, 300, 200, 100, 50, 0 };
            float od = Settings.GetValue("OD", 9f);
            JudgementWindows = new float[] {
                16.5f,
                64.5f - od * 3,
                97.5f - od * 3,
                127.5f - od * 3,
                151.5f - od * 3
            };
        }
    }
}
