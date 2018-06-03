using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Charts.YAVSRG;

namespace YAVSRG.Gameplay.Mods
{
    public class NoSV : Mod
    {
        public override bool IsApplicable(ChartWithModifiers c, string data)
        {
            foreach (BPMPoint b in c.Timing.Points)
            {
                if (b.ScrollSpeed != 1) { return true; }
            }
            return false;
        }

        public override void Apply(ChartWithModifiers c, string data)
        {
            foreach (BPMPoint b in c.Timing.Points)
            {
                b.ScrollSpeed = 1;
            }
        }

        public override string GetName(string data)
        {
            return "NoSV";
        }
    }
}
