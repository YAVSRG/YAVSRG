using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Gameplay.Watchers.Scoring
{
    public class DP : NoCCScoring
    {
        int judge;
        public DP(int judge) : base(null, new int[] { 2, 2, 1, -4, -8, -8 }, 2)
        {
            this.judge = judge;
            float hw = 45f / 6 * (10 - judge);
            JudgementWindows = new float[] {
                hw/2,
                hw,
                hw*2,
                hw*3,
                hw*4
            };
        }

        public override string FormatAcc()
        {
            return base.FormatAcc()+" (DP J" + judge.ToString() + ")";
        }
    }
}
