using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Prelude.Gameplay.Watchers.Scoring;

namespace Prelude.Gameplay.Watchers
{
    public abstract class IScoreSystem : IGameplayWatcher
    {
        //todo: remove and replace with dynamic score system list
        public enum ScoreType
        {
            Default,
            Osu,
            DP,
            Wife,
            SCPlus
        }

        public float[] JudgementWindows;
        public int[] PointsPerJudgement;
        public int MaxPointsPerNote;
        public float MissWindow = 180f;

        protected float PossiblePoints = 0;
        protected float PointsScored = 0;
        public int Combo = 0;
        public int ComboBreaks = 0;
        public int BestCombo = 0;
        public int[] Judgements;

        public Action<int, int, float> OnHit = (a, b, c) => { };

        protected virtual void ComboBreak()
        {
            if (Combo > BestCombo)
            {
                BestCombo = Combo;
            }
            Combo = 0;
            ComboBreaks += 1;
        }

        protected virtual void AddJudgement(int i)
        {
            Judgements[i] += 1;
            PointsScored += PointsPerJudgement[i];
            PossiblePoints += MaxPointsPerNote;
        }

        public virtual float Accuracy()
        {
            return 100f * GetValue();
        }

        public override float GetValue()
        {
            if (PossiblePoints == 0) return 1;
            return PointsScored / PossiblePoints;
        }

        public virtual int JudgeHit(float delta)
        {
            delta = Math.Abs(delta);
            for (int i = 0; i < JudgementWindows.Length; i++)
            {
                if (delta < JudgementWindows[i]) { return i; }
            }
            return JudgementWindows.Length;
        }

        public virtual string FormatAcc()
        {
            return string.Format("{0:0.00}", Math.Round(Accuracy(), 2)) + "%";
        }
    }
}
