using System;
using Prelude.Utilities;

namespace Prelude.Gameplay.ScoreMetrics.Accuracy
{
    public class StandardScoringPlus : DancePoints
    {
        float scale = 1f;

        public StandardScoringPlus(DataGroup Settings) : base(Settings)
        {
            int judge = Settings.GetValue("Judge", 4);
            float m = (10 - judge) / 6f;
            scale *= m;
            Name = Name.Replace("DP", "SC+");
        }

        public override float GetPointsForNote(float Delta)
        {
            if (Delta >= MissWindow) { return 0; };
            return (float)Math.Max(-1, (1 - Math.Pow(Delta / scale, 2.8) * 0.0000056d) * 2f);
        }
    }
}
