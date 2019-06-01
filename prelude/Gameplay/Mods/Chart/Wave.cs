using System;
using Prelude.Utilities;

namespace Prelude.Gameplay.Mods
{
    public class Wave : Mod
    {
        //This is a temporary testing mod so it is undocumented
        public override void Apply(ChartWithModifiers Chart, DataGroup Data)
        {
            if (Chart.Timing.BPM.Count == 0) return;
            float t = Chart.Timing.BPM.Points[0].Offset;
            float step = Chart.Timing.BPM.Points[0].MSPerBeat;
            double x = Math.PI * 2 / Chart.Timing.BPM.Points[0].Meter;
            double y = 0;
            while (t < Chart.Notes.Points[Chart.Notes.Points.Count - 1].Offset)
            {
                for (byte k = 0; k < Chart.Keys; k++)
                {
                    Chart.Timing.SV[k + 1].AppendPoint(new Charts.YAVSRG.SVPoint(t, 1 + 0.1f * (float)Math.Sin(y + x * k)));
                }
                t += step;
                y += x;
            }
        }

        public override string GetName(DataGroup Data)
        {
            return "Seasick";
        }

        public override int GetStatus(DataGroup Dataa)
        {
            return 2;
        }

        public override string GetDescription(DataGroup Data) { return "Makes notes W A V Y"; }
    }
}
