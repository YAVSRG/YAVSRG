using System;
using Prelude.Utilities;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Prelude.Gameplay.ScoreMetrics.Accuracy;

namespace Prelude.Gameplay.ScoreMetrics
{
    //todo: option between points out of 100% rising, max accuracy possible and current accuracy for time series
    public class ScoreSystem : IScoreMetric
    {
        public class ScoreSystemData
        {
            public ScoreType Type;
            public DataGroup Data;

            public ScoreSystemData(ScoreType type, DataGroup data)
            {
                Data = data;
                Type = type;
            }

            public ScoreSystem Instantiate()
            {
                switch (Type)
                {
                    case ScoreType.DP:
                        return new DancePoints(Data);
                    case ScoreType.Osu:
                        return new OsuMania(Data);
                    case ScoreType.Wife:
                        return new Wife(Data);
                    case ScoreType.SCPlus:
                        return new StandardScoringPlus(Data);
                    case ScoreType.Default:
                    default:
                        return new StandardScoring(Data);
                }
            }
        }
        //todo: remove and replace with dynamic score system list
        public enum ScoreType
        {
            Default,
            Osu,
            DP,
            Wife,
            SCPlus,
            Custom
        }

        //this should be a constant but its chilling here just in case it needs changing
        public readonly float MissWindow = 180f;

        //array of windows in milliseconds to score a certain judgement
        //e.g. for the best judgement your absolute deviation must be less than JudgementWindows[0]
        protected float[] JudgementWindows;

        //points awarded for each judgement, should 1 more than length of JudgementWindows (extra for missing a note)
        protected int[] PointsPerJudgement;

        //maximum points awarded for each judgement (should be same as PointsPerJudgement[0] but allows for funny use cases)
        protected int MaxPointsPerNote;

        //this judgement index OR WORSE will cause a combo break e.g. 3 awards Good, so 3,4 or 5 (good, bad, miss) all combo break if this is set to 3
        protected int ComboBreakingJudgement;

        //display name of this accuracy system
        public string Name;

        //max points possible to have scored so far
        protected float PossiblePoints = 0;

        //points the user has scored so far
        protected float PointsScored = 0;

        //CURRENT combo the user is on
        public int Combo = 0;

        //amount of times the user has broken combo
        public int ComboBreaks = 0;

        //BEST combo the user has scored so far
        public int BestCombo = 0;

        //array storing number of each judgement the user has achieved so far
        public int[] Judgements;

        //hook used by things like UI on the gameplay screen to display judgements when you get them
        public Action<int, int, float> OnHit = (Column, Judgement, Delta) => { };

        public ScoreSystem(string Name, int JudgementCount)
        {
            this.Name = Name;
            Judgements = new int[JudgementCount];
        }

        public virtual float GetPointsForNote(float Delta)
        {
            return PointsPerJudgement[JudgeHit(Delta)];
        }

        //logic to handle a combo breaking judgement/miss
        protected virtual void ComboBreak()
        {
            Combo = 0;
            ComboBreaks += 1;
        }

        public override void HandleHit(int Column, int Index, HitData[] HitData)
        {
            float delta = Math.Abs(HitData[Index].delta[Column]);
            int Judgement = JudgeHit(delta);
            Judgements[Judgement] += 1;
            PointsScored += GetPointsForNote(delta);
            PossiblePoints += MaxPointsPerNote;
            if (Judgement >= ComboBreakingJudgement)
            {
                ComboBreak();
            }
            else
            {
                Combo++;
                if (Combo > BestCombo)
                {
                    BestCombo = Combo;
                }
            }
            OnHit(Column, Judgement, HitData[Index].delta[Column]);
        }

        //processes the entire hit data of an existing score to get the final accuracy
        public override void ProcessScore(HitData[] HitData)
        {
            while (Counter < HitData.Length)
            {
                for (int i = 0; i < HitData[Counter].hit.Length; i++)
                {
                    if (HitData[Counter].hit[i] == 1)
                    {
                        HitData[Counter].delta[i] = MissWindow;
                        HandleHit(i, Counter, HitData);
                    }
                    else if (HitData[Counter].hit[i] == 2)
                    {
                        HandleHit(i, Counter, HitData);
                    }
                }
                Counter++;
            }
        }

        //processes updates to accuracy live by identifying recently missed notes
        public override void Update(float Now, HitData[] HitData)
        {
            Now -= MissWindow;
            while (Counter < HitData.Length && HitData[Counter].Offset <= Now)
            {
                for (int i = 0; i < HitData[Counter].hit.Length; i++)
                {
                    if (HitData[Counter].hit[i] == 1)
                    {
                        HitData[Counter].delta[i] = MissWindow;
                        HandleHit(i, Counter, HitData);
                    }
                }
                Counter++;
            }
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

        //public because external code may want to know the judgement for a given ms deviation e.g. drawing the distribution graph on score screen
        public virtual int JudgeHit(float Delta)
        {
            for (int i = 0; i < JudgementWindows.Length; i++)
            {
                if (Delta < JudgementWindows[i]) { return i; }
            }
            return JudgementWindows.Length;
        }

        public virtual string FormatAcc()
        {
            return string.Format("{0:0.00}", Math.Round(Accuracy(), 2)) + "% ("+Name+")";
        }
    }
}
