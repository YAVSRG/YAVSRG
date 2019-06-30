using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Gameplay.ScoreMetrics.HP
{
    public class HPSystem : ILifeMeter
    {
        protected ScoreSystem Scoring;

        public HPSystem(ScoreSystem Scoring)
        {
            this.Scoring = Scoring;
            PointsPerJudgement = new float[] { 0.5f, 0.25f, 0f, -5f, -20f, -10f };
            CurrentHP = 50;
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

        public override void ProcessScore(HitData[] HitData)
        {
            while (Counter < HitData.Length)
            {
                for (int i = 0; i < HitData[Counter].hit.Length; i++)
                {
                    if (HitData[Counter].hit[i] > 0)
                    {
                        HandleHit(i, Counter, HitData);
                    }
                }
                Counter++;
                UpdateTimeSeriesData(HitData.Length);
            }
        }

        //must happen after score system passes over it
        public override void Update(float Now, HitData[] HitData)
        {
            Now -= Scoring.MissWindow;
            while (Counter < HitData.Length && HitData[Counter].Offset <= Now)
            {
                for (int i = 0; i < HitData[Counter].hit.Length; i++)
                {
                    if (HitData[Counter].hit[i] == 1)
                    {
                        //data[Counter].delta[i] = Scoring.MissWindow; <-- this line is for if score system does not pass first (was here to test a bug that has now been fixed)
                        HandleHit(i, Counter, HitData);
                    }
                }
                Counter++;
            }
            UpdateTimeSeriesData(HitData.Length);
        }
    }
}
