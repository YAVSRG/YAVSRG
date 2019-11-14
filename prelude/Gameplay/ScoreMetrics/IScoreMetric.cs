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
        public float[] Data = new float[100];

        protected int Counter = 0;

        public abstract void Update(float Now, HitData[] HitData);

        public abstract void HandleHit(byte Column, int Index, HitData[] HitData);

        public abstract void ProcessScore(HitData[] HitData);

        public abstract float GetValue();

        public void UpdateTimeSeriesData(int snaps)
        {
            int i = Data.Length * Counter / snaps;
            if (i == Data.Length) return;
            if (Data[i] == 0)
            {
                Data[i] = GetValue();
            }
        }

        public bool ReachedEnd(int snaps)
        {
            return Counter == snaps;
        }
    }
}
