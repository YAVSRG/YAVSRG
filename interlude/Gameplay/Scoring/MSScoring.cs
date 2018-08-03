using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Gameplay
{
    public class MSScoring : DP
    {
        float CurveBegin = 18f; //named exactly like etterna wiki
        float CurveEnd = 150f;
        float linFac = 9.5f;
        float expFac = 2f;

        public MSScoring(int judge) : base(judge)
        {
        }

        public override void AddJudgement(int i)
        {
            Judgements[i]++;
        }

        public override void HandleHit(int k, int index, ScoreTracker.HitData[] data)
        {
            int j = JudgeHit(data[index].delta[k]);
            AddJudgement(j);
            maxscore += maxweight;
            score += CalculatePoints(Math.Abs(data[index].delta[k]));
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
            if (ms <= CurveBegin) { return maxweight; }
            if (ms >= CurveEnd) { return maxweight-linFac; }; //max penalty
            return maxweight - (linFac * (float)Math.Pow((ms - CurveBegin) / (CurveEnd - CurveBegin), expFac));
        }

        public override string FormatAcc()
        {
            return Utils.RoundNumber(Accuracy()) + "% (Wife J5)";
        }
    }
}
