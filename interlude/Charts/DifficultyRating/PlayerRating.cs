using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;

namespace YAVSRG.Charts.DifficultyRating
{
    public class PlayerRating
    {
        public static float GetRating(RatingReport r, ScoreTracker.HitData[] hitdata)
        {
            ScoreSystem judge = ScoreSystem.GetScoreSystem(ScoreType.Default);
            float v = 0;
            int n = 0;
            for (int i = 0; i < r.PhysicalData.Length; i++)
            {
                if (r.PhysicalData[i] > 0)
                {
                    v += GetValue(hitdata[i], judge) * r.PhysicalData[i];
                    n += 1;
                }
            }
            if (n > 0)
            {
                return v / n;
            }
            return 0f;
        }

        static float GetValue(ScoreTracker.HitData h, ScoreSystem judge)
        {
            float v = 0;
            for (byte k = 0; k < h.hit.Length; k++)
            {
                if (h.hit[k] > 0)
                {
                    v += judge.weights[judge.JudgeHit(h.delta[k])];
                }
            }
            return v / (h.Count * judge.maxweight) / 0.95f;
        }
    }
}
