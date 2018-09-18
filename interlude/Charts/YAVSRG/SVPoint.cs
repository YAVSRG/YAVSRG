using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Charts.YAVSRG
{
    public class SVPoint : OffsetItem
    {
        public float ScrollSpeed;

        public SVPoint(float offset, float sv)
        {
            Offset = offset;
            ScrollSpeed = sv;
        }

        public override OffsetItem Interpolate(float time)
        {
            return new SVPoint(time, ScrollSpeed);
        }
    }
}
