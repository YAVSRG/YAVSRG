using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Charts.YAVSRG
{
    public class BPMPoint : OffsetItem
    {
        public int Meter;
        public float MSPerBeat;
         
        public BPMPoint(float offset, int meter, float bpm)
        {
            Offset = offset;
            Meter = meter;
            MSPerBeat = bpm;
        }
    }
}
