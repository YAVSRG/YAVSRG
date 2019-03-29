using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Gameplay.Watchers.Scoring
{
    public class MSScoring : DP
    {
        float CurveEnd = 180f;
        float scale = 95*95;

        public MSScoring(int judge) : base(judge)
        {
            float m = (10 - judge) / 6f;
            scale *= m;
            CurveEnd *= m;
        }

        protected override void AddJudgement(int i)
        {
            Judgements[i]++;
        }

        public override void HandleHit(int k, int index, HitData[] data)
        {
            int j = JudgeHit(data[index].delta[k]);
            AddJudgement(j);
            PossiblePoints += MaxPointsPerNote;
            PointsScored += CalculatePoints(Math.Abs(data[index].delta[k]));
            if (j >= 3)
            {
                ComboBreak();
            }
            else
            {
                Combo++;
            }
            OnHit(k, j, data[index].delta[k]);
        }

        private float CalculatePoints(float ms)
        {
            if (ms >= CurveEnd) { return -8; }; //max penalty
            return (float)(2 - 10 * Math.Pow(1 - Math.Pow(2, -(ms * ms) / scale), 2));
        }

        public override string FormatAcc()
        {
            return base.FormatAcc().Replace("DP", "Wife");
        }
    }
}
