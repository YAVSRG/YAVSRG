using System;

namespace Prelude.Gameplay.ScoreMetrics.HP
{
    public class HPSystem : LifeMeter
    {
        protected ScoreSystem Scoring;

        public HPSystem(ScoreSystem Scoring)
        {
            this.Scoring = Scoring;
            PointsPerJudgement = new float[] { 0.5f, 0.5f, 0.25f, 0f, -5f, -20f, -10f, 0f, -5f };
            CurrentHP = 50;
        }

        public override void HandleHit(byte Column, int Index, HitData[] Data)
        {
            int judgement = (int)Scoring.JudgeHit(Data[Index].delta[Column]);
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
                for (byte k = 0; k < HitData[Counter].hit.Length; k++)
                {
                    if (HitData[Counter].hit[k] > 0)
                    {
                        HandleHit(k, Counter, HitData);
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
                for (byte k = 0; k < HitData[Counter].hit.Length; k++)
                {
                    if (HitData[Counter].hit[k] == 1)
                    {
                        //data[Counter].delta[i] = Scoring.MissWindow; <-- this line is for if score system does not pass first (was here to test a bug that has now been fixed)
                        HandleHit(k, Counter, HitData);
                    }
                }
                Counter++;
            }
            UpdateTimeSeriesData(HitData.Length);
        }
    }
}
