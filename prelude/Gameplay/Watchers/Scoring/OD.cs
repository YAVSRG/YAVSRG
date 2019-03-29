using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Gameplay.Watchers.Scoring
{
    public class OD : NoCCScoring
    {
        private float od;
        public OD (float od) : base(null, new int[] { 300, 300, 200, 100, 50, 0 },300)
        {
            this.od = od;
            JudgementWindows = new float[] {
                16.5f,
                64.5f - od*3,
                97.5f - od*3,
                127.5f - od*3,
                151.5f - od*3
            };
        }

        public override string FormatAcc()
        {
            return base.FormatAcc()+" (Osu OD"+od.ToString()+")";
        }
    }
}
