using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Gameplay
{
    public class CCScoring : ScoreSystem
    {
        public CCScoring(float[] windows, int[] weights, int max)
        {
            maxweight = max;
            this.windows = windows;
            this.weights = weights;
            Judgements = new int[weights.Length];
        }

        private void AddCCJudgement(int i, int count)
        {
            Judgements[i] += 1;
            score += (float)weights[i] / count;
            maxscore += (float)maxweight / count;
        }

        public override void HandleHit(int k, int index, ScoreTracker.HitData[] data)
        {
            int j = JudgeHit(data[index].delta[k]);
            AddCCJudgement(j, data[index].Count);
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

        public override void ProcessScore(ScoreTracker.HitData[] data)
        {
            while (pos < data.Length)
            {
                for (int i = 0; i < data[pos].hit.Length; i++)
                {
                    if (data[pos].hit[i] == 1)
                    {
                        data[pos].delta[i] = MissWindow;
                        HandleHit(i, pos, data);
                    }
                    else if (data[pos].hit[i] == 2)
                    {
                        HandleHit(i, pos, data);
                    }
                }
                pos++;
            }
        }

        public override void Update(float now, ScoreTracker.HitData[] data)
        {
            while (pos < data.Length && data[pos].Offset <= now)
            {
                for (int i = 0; i < data[pos].hit.Length; i++)
                {
                    if (data[pos].hit[i] == 1)
                    {
                        data[pos].delta[i] = MissWindow;
                        HandleHit(i, pos, data);
                    }
                }
                pos++;
            }
        }
    }
}
