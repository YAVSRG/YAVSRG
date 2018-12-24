using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Gameplay.Watchers.Scoring
{
    public class StandardScoringPlus : DP
    {
        float CurveEnd = 180f;
        float scale = 32f;

        public StandardScoringPlus(int judge) : base(judge)
        {
            float m = (10 - judge) / 6f;
            scale *= m;
        }

        protected override void AddJudgement(int i)
        {
            Judgements[i]++;
        }

        public override void HandleHit(int k, int index, ScoreTracker.HitData[] data)
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
            if (ms >= CurveEnd) { return 0; };
            return (float)(10 - Math.Pow(ms / scale, 2))/5f;
        }

        public override string FormatAcc()
        {
            return base.FormatAcc().Replace("DP", "SC+");
        }
    }
}
