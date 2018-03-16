using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Gameplay
{
    public class NoCCScoring : ScoreSystem
    {
        public NoCCScoring(float[] windows, int[] weights, int max)
        {
            maxweight = max;
            this.windows = windows;
            this.weights = weights;
            Judgements = new int[weights.Length];
        }

        /*
        public override void Update(float now, ScoreTracker.HitData[] data)
        {
            while (pos < data.Length && data[pos].Offset <= now)
            {
                for (int i = 0; i < data[pos].hit.Length; i++)
                {
                    if (data[pos].hit[i] == 1)
                    {
                        //GET MAD
                        AddJudgement(5);
                    }
                    else if (data[pos].hit[i] == 2)
                    {
                        //GET GLAD
                        AddJudgement(JudgeHit(Math.Abs(data[pos].delta[i])));
                        Combo++;
                    }
                }
                pos++;
            }
        }*/


        public override void HandleHit(int k, int index, ScoreTracker.HitData[] data)
        {
            int j = JudgeHit(data[index].delta[k]);
            AddJudgement(j);
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
                        AddJudgement(5);
                        //no onhit call here because it doesn't matter
                        ComboBreak();
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
                        AddJudgement(5);
                        OnHit(i, 5, MissWindow);
                        ComboBreak();
                    }
                }
                pos++;
            }
        }
    }
}
