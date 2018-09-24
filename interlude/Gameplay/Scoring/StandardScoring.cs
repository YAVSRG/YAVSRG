using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Gameplay
{
    class StandardScoring : CCScoring
    {
        public StandardScoring() : base(null, new int[] { 10, 9, 5, 1, -25, 0 }, 10)
        {
            float hw = 45f;
            windows = new float[] {
                hw/2,
                hw,
                hw*2,
                hw*3,
                hw*4
            };
        }

        public override string FormatAcc()
        {
            return base.FormatAcc() + " (YAV)";
        }
    }
}
