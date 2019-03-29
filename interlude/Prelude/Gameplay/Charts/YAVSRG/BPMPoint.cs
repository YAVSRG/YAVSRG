using System;

namespace Prelude.Gameplay.Charts.YAVSRG
{
    public class BPMPoint : OffsetItem
    {
        public int Meter;
        public float MSPerBeat;
         
        public BPMPoint(float offset, int meter, float bpm) : base(offset)
        {
            Meter = meter;
            MSPerBeat = bpm;
        }
    }
}
