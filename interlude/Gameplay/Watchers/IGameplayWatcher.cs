using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Gameplay.Watchers
{
    public abstract class IGameplayWatcher
    {
        public delegate void HitHandler(int Column, int Judgement, float Offset);

        protected int Cursor = 0;

        public abstract void Update(float now, ScoreTracker.HitData[] data);

        public abstract void HandleHit(int k, int index, ScoreTracker.HitData[] data);

        public abstract void ProcessScore(ScoreTracker.HitData[] data);
    }
}
