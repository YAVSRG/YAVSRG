using System;
using Prelude.Utilities;

namespace Prelude.Gameplay.ScoreMetrics.Accuracy
{
    public class Wife : DancePoints
    {
        //Wife is a score system implemented by Etterna with the aim to improve upon DP
        //It is pretty much exactly like DP except that all hits in milliseconds are fitted to a curve and a formula is used to determine points rather than judgement windows
        //This gives more feedback to players who get almost entirely marvellous judgements as every millisecond matters
        //The curve was initially modelled to be a close mimic of DP but has been adjusted over time - the curve used here is up to date with april 2019
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
