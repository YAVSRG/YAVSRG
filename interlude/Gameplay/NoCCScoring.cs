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
        }
    }
}
