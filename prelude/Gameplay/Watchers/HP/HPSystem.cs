using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Gameplay.Watchers.HP
{
    public class HPSystem : ILifeMeter
    {
        protected ScoreSystem Scoring;

        public HPSystem(ScoreSystem Scoring)
        {
            this.Scoring = Scoring;
            PointsPerJudgement = new float[] { 0.5f, 0.25f, 0f, -5f, -20f, -10f };
        }

        public override void HandleHit(int k, int index, HitData[] data)
        {
            int judgement = Scoring.JudgeHit(data[index].delta[k]);
            CurrentHP += PointsPerJudgement[judgement];
            CurrentHP = Math.Max(0, Math.Min(MaximumHP, CurrentHP));
            if (CurrentHP == 0)
            {
                Failed = true;
            }
        }

        public override void ProcessScore(HitData[] data)
        {
            while (Counter < data.Length)
            {
                for (int i = 0; i < data[Counter].hit.Length; i++)
                {
                    if (data[Counter].hit[i] > 0)
                    {
                        HandleHit(i, Counter, data);
                    }
                }
                Counter++;
            }
        }

        //must happen after score system passes over it
        public override void Update(float now, HitData[] data)
        {
            while (Counter < data.Length && data[Counter].Offset <= now)
            {
                for (int i = 0; i < data[Counter].hit.Length; i++)
                {
                    if (data[Counter].hit[i] == 1)
                    {
                        HandleHit(i, Counter, data);
                    }
                }
                Counter++;
            }
        }
    }
}
