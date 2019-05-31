using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Gameplay.ScoreMetrics
{
    public abstract class ILifeMeter : IScoreMetric
    {
        protected float MaximumHP = 100;
        protected float CurrentHP = 100;
        protected float[] PointsPerJudgement;
        protected bool Failed;

        public virtual bool HasFailed()
        {
            return Failed;
        }

        public override float GetValue()
        {
            return CurrentHP / MaximumHP;
        }
    }
}
