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

        public override void Update(float now, PlayingChart.HitData[] data)
        {
            while (pos < data.Length && data[pos].Offset <= now)
            {
                float t = 0;
                int n = 0;
                int judgement = 0;
                for (int i = 0; i < data[pos].hit.Length; i++)
                {
                    if (data[pos].hit[i] == 1)
                    {
                        judgement += 1;
                        ComboBreak();
                    }
                    else if (data[pos].hit[i] == 2)
                    {
                        t += Math.Abs(data[pos].delta[i]);
                        n += 1;
                    }
                }
                if (n > 0 && judgement < 3)
                {
                    float delta = t / n;
                    if (judgement > 0) judgement++;
                    judgement += JudgeHit(delta);
                    if (judgement > 4)
                    {
                        judgement = 4;
                    }
                    AddJudgement(judgement);
                    Combo++;
                }
                else
                {
                    AddJudgement(5);
                }
                pos++;
            }
        }
    }
}
