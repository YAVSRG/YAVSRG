using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Gameplay.Watchers.HP
{
    public class HPSystem : ILifeMeter
    {
        protected IScoreSystem Scoring;

        public HPSystem(IScoreSystem Scoring)
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
            while (Cursor < data.Length)
            {
                for (int i = 0; i < data[Cursor].hit.Length; i++)
                {
                    if (data[Cursor].hit[i] > 0)
                    {
                        HandleHit(i, Cursor, data);
                    }
                }
                Cursor++;
            }
        }

        //must happen after score system passes over it
        public override void Update(float now, HitData[] data)
        {
            while (Cursor < data.Length && data[Cursor].Offset <= now)
            {
                for (int i = 0; i < data[Cursor].hit.Length; i++)
                {
                    if (data[Cursor].hit[i] == 1)
                    {
                        HandleHit(i, Cursor, data);
                    }
                }
                Cursor++;
            }
        }
    }
}
