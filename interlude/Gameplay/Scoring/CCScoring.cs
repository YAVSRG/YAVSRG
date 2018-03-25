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
            score += weights[i] / count;
            maxscore += maxweight / count;
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
                        AddCCJudgement(5, data[pos].Count);
                        OnHit(i, 5, MissWindow);
                        ComboBreak();
                    }
                }
                    /*
                    float t = 0;
                    int n = 0;
                    int judgement = 0;
                    for (int i = 0; i < data[pos].hit.Length; i++)
                    {
                        if (data[pos].hit[i] == 1)
                        {
                            judgement += 1;
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
                    */
                    pos++;
            }
        }
    }
}
