using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Gameplay.Mods
{
    public class Wave : Mod
    {
        public override void Apply(ChartWithModifiers c, string data)
        {
            base.Apply(c, data);
            if (c.Timing.BPM.Count == 0) return;
            float t = c.Timing.BPM.Points[0].Offset;
            float step = c.Timing.BPM.Points[0].MSPerBeat;
            double x = Math.PI * 2 / c.Timing.BPM.Points[0].Meter;
            double y = 0;
            while (t < c.Notes.Points[c.Notes.Points.Count - 1].Offset)
            {
                for (byte k = 0; k < c.Keys; k++)
                {
                    c.Timing.SV[k + 1].AppendPoint(new Charts.YAVSRG.SVPoint(t, 1 + 0.1f * (float)Math.Sin(y + x * k)));
                }
                t += step;
                y += x;
            }
        }

        public override string GetName(string data)
        {
            return "W A V E";
        }

        public override int GetStatus(string data)
        {
            return 2;
        }

        public override string GetDescription(string data) { return "Makes notes W A V Y"; }
    }
}
