using System;
using Prelude.Utilities;

namespace Prelude.Gameplay.Watchers.Scoring
{
    public class Wife : DancePoints
    {
        float CurveEnd = 180f;
        float scale = 95*95;

        public Wife(DataGroup Settings) : base(Settings)
        {
            int judge = Settings.GetValue("Judge", 4);
            float m = (10 - judge) / 6f;
            scale *= m;
            CurveEnd *= m;
            Name = Name.Replace("DP", "Wife");
        }

        public override float GetPointsForNote(float Delta)
        {
            if (Delta >= CurveEnd) { return -8; }; //max penalty
            return (float)(2 - 10 * Math.Pow(1 - Math.Pow(2, -(Delta * Delta) / scale), 2));
        }
    }
}
