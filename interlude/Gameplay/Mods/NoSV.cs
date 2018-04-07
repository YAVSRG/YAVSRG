using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Beatmap;

namespace YAVSRG.Gameplay.Mods
{
    public class NoSV : Mod
    {
        public override void Apply(ChartWithModifiers c)
        {
            foreach (BPMPoint b in c.Timing.Points)
            {
                b.ScrollSpeed = 1;
            }
        }

        public override string GetName()
        {
            return "NoSV";
        }
    }
}
