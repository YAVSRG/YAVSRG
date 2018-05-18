using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Charts.YAVSRG
{
    public class OffsetItem
    {
        public float Offset;

        public virtual OffsetItem Interpolate(float time) { return null; }
    }
}
