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
            List <BPMPoint> newPoints = new List<BPMPoint>();
            foreach (BPMPoint b in c.Timing.Points)
            {
                newPoints.Add(new BPMPoint(b.Offset, b.Meter, b.MSPerBeat, 1, b.InheritsFrom));
            }
            c.Timing.Points = newPoints; //todo: this is stupid please clean it up and make it sensible
        }

        public override string GetName(string data)
        {
            return "NoSV";
        }

        public override string GetDescription(string data) { return "Removes all slider velocity (scroll speed) changes from a chart."; }
    }
}
