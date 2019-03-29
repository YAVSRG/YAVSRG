using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Gameplay.Watchers.Scoring
{
    public class NoCCScoring : IScoreSystem
    {
        public NoCCScoring(float[] windows, int[] weights, int max)
        {
            MaxPointsPerNote = max;
            JudgementWindows = windows;
            PointsPerJudgement = weights;
            Judgements = new int[weights.Length];
        }

        public override void HandleHit(int k, int index, HitData[] data)
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

        public override void ProcessScore(HitData[] data)
        {
            while (Cursor < data.Length)
            {
                for (int i = 0; i < data[Cursor].hit.Length; i++)
                {
                    if (data[Cursor].hit[i] == 1)
                    {
                        data[Cursor].delta[i] = MissWindow;
                        HandleHit(i, Cursor, data);
                    }
                    else if (data[Cursor].hit[i] == 2)
                    {
                        HandleHit(i, Cursor, data);
                    }
                }
                Cursor++;
            }
            BestCombo = Math.Max(Combo, BestCombo);
        }

        public override void Update(float now, HitData[] data)
        {
            while (Cursor < data.Length && data[Cursor].Offset <= now)
            {
                for (int i = 0; i < data[Cursor].hit.Length; i++)
                {
                    if (data[Cursor].hit[i] == 1)
                    {
                        data[Cursor].delta[i] = MissWindow;
                        HandleHit(i, Cursor, data);
                    }
                }
                Cursor++;
            }
        }
    }
}
