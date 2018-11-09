using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay.Watchers.Scoring;

namespace YAVSRG.Gameplay.Watchers
{
    public abstract class IScoreSystem : IGameplayWatcher
    {
        public enum ScoreType
        {
            Default,
            Osu,
            DP,
            Wife
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

        public static IScoreSystem GetScoreSystem(ScoreType s)
        {
            switch (s)
            {
                case ScoreType.DP:
                    return new DP(Game.Options.Profile.Judge);
                case ScoreType.Osu:
                    return new OD(Game.Options.Profile.OD);
                case ScoreType.Wife:
                    return new MSScoring(Game.Options.Profile.Judge);
                case ScoreType.Default:
                default:
                    return new StandardScoring();
            }
        }

        public virtual void ComboBreak()
        {
            if (Combo > BestCombo)
            {
                BestCombo = Combo;
            }
            Combo = 0;
            ComboBreaks += 1;
        }

        public virtual void AddJudgement(int i)
        {
            Judgements[i] += 1;
            PointsScored += PointsPerJudgement[i];
            PossiblePoints += MaxPointsPerNote;
        }

        public virtual float Accuracy()
        {
            if (PossiblePoints == 0) return 100;
            return PointsScored * 100f / PossiblePoints;
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
            return Utils.RoundNumber(Accuracy()) + "%";
        }

        public bool EndOfChart(int snaps)
        {
            return Cursor == snaps;
        }
    }
}
