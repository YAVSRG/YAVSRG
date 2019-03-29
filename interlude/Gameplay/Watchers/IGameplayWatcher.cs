using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Gameplay.Watchers
{
    public abstract class IGameplayWatcher
    {
        public delegate void HitHandler(int Column, int Judgement, float Offset);

        protected int Cursor = 0;

        public abstract void Update(float Now, ScoreTracker.HitData[] HitData);

        public abstract void HandleHit(int Column, int Index, ScoreTracker.HitData[] HitData);

        public abstract void ProcessScore(ScoreTracker.HitData[] HitData);

        public abstract float GetValue();

        public bool ReachedEnd(int snaps)
        {
            return Cursor == snaps;
        }
    }
}
