using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Gameplay.ScoreMetrics
{
    //interface for anything that looks at your hitdata either live or by processing a score
    //e.g. accuracy or HP system
    //todo: support for time series data to graph it overlayed on hit distribution graph
    public abstract class IScoreMetric
    {
        public delegate void HitHandler(int Column, int Judgement, float Offset);

        protected int Counter = 0;

        public abstract void Update(float Now, HitData[] HitData);

        public abstract void HandleHit(int Column, int Index, HitData[] HitData);

        public abstract void ProcessScore(HitData[] HitData);

        public abstract float GetValue();

        public bool ReachedEnd(int snaps)
        {
            return Counter == snaps;
        }
    }
}
