using System.Collections.Generic;
using Prelude.Utilities;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay.Mods
{
    //This modifier is incomplete and undocumented
    public class Manipulate : Mod
    {
        public override bool IsApplicable(ChartWithModifiers Chart, DataGroup Data)
        {
            return (Chart.Keys == 4);
        }

        public override void Apply(ChartWithModifiers Chart, DataGroup Data)
        {
            List<GameplaySnap> newPoints = new List<GameplaySnap>();
            int count = Chart.Notes.Count;
            ushort left, right;
            ushort lmask = 3;
            ushort rmask = 12;
            List<float> ltimes = new List<float>();
            List<float> rtimes = new List<float>();

            int i = 0;
            float lim;
            while (i < count)
            {
                left = 0; right = 0;
                ltimes.Clear();
                var p = Chart.Timing.BPM.GetPointAt(Chart.Notes.Points[i].Offset, false);
                lim = Chart.Notes.Points[i].Offset - (Chart.Notes.Points[i].Offset - p.Offset) % (p.MSPerBeat) + (p.MSPerBeat);

                while (i < count && Chart.Notes.Points[i].Offset <= lim)
                {
                    if ((left & Chart.Notes.Points[i].taps.value & lmask) > 0)
                    {
                        newPoints.Add(new GameplaySnap(mean(ltimes), left, 0, 0, 0, 0));
                        left = 0;
                        ltimes.Clear();
                    }
                    if ((right & Chart.Notes.Points[i].taps.value & rmask) > 0)
                    {
                        //if (newPoints.Count > 0 && mean(rtimes) == newPoints[newPoints.Count - 1].Offset)
                        {
                        //    newPoints[newPoints.Count - 1].taps.value |= right;
                        }
                        //else
                        {
                            newPoints.Add(new GameplaySnap(mean(rtimes), right, 0, 0, 0, 0));
                        }
                        right = 0;
                        rtimes.Clear();
                    }
                    if ((Chart.Notes.Points[i].taps.value & lmask) > 0)
                    {
                        left |= (ushort)(Chart.Notes.Points[i].taps.value & lmask);
                        ltimes.Add(Chart.Notes.Points[i].Offset);
                    }
                    if ((Chart.Notes.Points[i].taps.value & rmask) > 0)
                    {
                        right |= (ushort)(Chart.Notes.Points[i].taps.value & rmask);
                        rtimes.Add(Chart.Notes.Points[i].Offset);
                    }
                    i++;
                }
                if (ltimes.Count > 0)
                {
                    newPoints.Add(new GameplaySnap(mean(ltimes), left, 0, 0, 0, 0));
                }
                if (rtimes.Count > 0)
                {
                    //if (newPoints.Count > 0 && mean(rtimes) == newPoints[newPoints.Count - 1].Offset)
                    {
                    //    newPoints[newPoints.Count - 1].taps.value |= right;
                    }
                    //else
                    {
                        newPoints.Add(new GameplaySnap(mean(rtimes), right, 0, 0, 0, 0));
                    }
                }
            }
            newPoints.Sort((a, b) => a.Offset.CompareTo(b.Offset));
            Chart.Notes = new PointManager<GameplaySnap>(newPoints);
        }

        private float mean(List<float> data)
        {
            float r = 0;
            foreach (float f in data)
            {
                r += f;
            }
            return r / data.Count;
        }

        public override int Status => 2;

        public override string Name => "Manipulate";
    }
}
