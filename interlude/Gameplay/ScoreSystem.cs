using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Gameplay
{
    public abstract class ScoreSystem
    {
        protected float[] windows;
        protected int[] weights;
        protected int maxweight;
        protected int pos = 0;
        protected float score = 0;
        protected int maxscore = 0;
        public int Combo = 0;
        public int ComboBreaks = 0;
        public int[] Judgements;
        public int BestCombo = 0;
        public float MissWindow = 180f;
        public string name;
        public Action<int> OnMiss = (x) => { };
        
        public abstract void Update(float now, PlayingChart.HitData[] data);

        public static ScoreSystem GetScoreSystem(ScoreType s)
        {
            switch (s)
            {
                case ScoreType.DP:
                    return new DP(Game.Options.Profile.Judge);
                case ScoreType.Osu:
                    return new OD(Game.Options.Profile.OD);
                case ScoreType.Wife:
                    return new MSScoring();
                case ScoreType.Default:
                default:
                    return new StandardScoring();
            }
        }

        public virtual void ProcessScore(PlayingChart.HitData[] data)
        {
            Update(data[data.Length - 1].Offset, data);
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
            score += weights[i];
            maxscore += maxweight;
            if (i > 3)
            {
                ComboBreak();
                if (i == 5)
                {
                    OnMiss(i);
                }
            }
        }

        public virtual float Accuracy()
        {
            if (maxscore == 0) return 100;
            return score * 100f / maxscore;
        }

        public virtual int JudgeHit(float delta)
        {
            for (int i = 0; i < windows.Length; i++)
            {
                if (delta <= windows[i]) { return i; }
            }
            return windows.Length;
        }

        public virtual string FormatAcc()
        {
            return Utils.RoundNumber(Accuracy())+"%";
        }
    }
}
