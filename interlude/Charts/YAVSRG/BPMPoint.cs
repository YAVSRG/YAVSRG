using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Charts.YAVSRG
{
    public class BPMPoint : OffsetItem
    {
        public float ScrollSpeed;
        public int Meter;
        public float MSPerBeat;
        public float InheritsFrom;
         
        public BPMPoint(float offset, int meter, float bpm, float scroll, float inherit)
        {
            Offset = offset;
            Meter = meter;
            ScrollSpeed = scroll;
            MSPerBeat = bpm;
            InheritsFrom = inherit;
        }
    }
}
