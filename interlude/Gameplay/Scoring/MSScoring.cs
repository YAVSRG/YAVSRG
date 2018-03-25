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

        /*
        public override void Update(float now, ScoreTracker.HitData[] data)
        {
            while (pos < data.Length && data[pos].Offset <= now) //copy paste code from DP except it calcs a bit differently
            {
                for (int i = 0; i < data[pos].hit.Length; i++)
                {
                    if (data[pos].hit[i] == 1)
                    {
                        //GET MAD
                        score += (maxweight - linFac); //miss penalty
                        ComboBreak();
                        //OnMiss(i);
                        maxscore += maxweight;
                    }
                    else if (data[pos].hit[i] == 2)
                    {
                        //GET GLAD
                        score+=CalculatePoints(Math.Abs(data[pos].delta[i]));
                        Combo++;
                        maxscore += maxweight;
                    }
                }
                pos++;
            }
        }*/

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
            return maxweight - (linFac * (float)Math.Pow((ms - CurveBegin) / (CurveEnd - CurveBegin), expFac));
        }

        public override string FormatAcc()
        {
            return base.FormatAcc().Replace("DP","Wife");
        }
    }
}
