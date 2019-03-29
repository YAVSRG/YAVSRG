using System;
using System.Collections.Generic;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay.Mods
{
    public class Manipulate : Mod
    {
        public override void Apply(ChartWithModifiers c, string data)
        {
            if (c.Keys != 4) return;
            base.Apply(c, data);
            List<GameplaySnap> newPoints = new List<GameplaySnap>();
            int count = c.Notes.Count;
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
                var p = c.Timing.BPM.GetPointAt(c.Notes.Points[i].Offset, false);
                lim = c.Notes.Points[i].Offset - (c.Notes.Points[i].Offset - p.Offset) % (p.MSPerBeat) + (p.MSPerBeat);

                while (i < count && c.Notes.Points[i].Offset <= lim)
                {
                    if ((left & c.Notes.Points[i].taps.value & lmask) > 0)
                    {
                        newPoints.Add(new GameplaySnap(mean(ltimes), left, 0, 0, 0, 0));
                        left = 0;
                        ltimes.Clear();
                    }
                    if ((right & c.Notes.Points[i].taps.value & rmask) > 0)
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
                    if ((c.Notes.Points[i].taps.value & lmask) > 0)
                    {
                        left |= (ushort)(c.Notes.Points[i].taps.value & lmask);
                        ltimes.Add(c.Notes.Points[i].Offset);
                    }
                    if ((c.Notes.Points[i].taps.value & rmask) > 0)
                    {
                        right |= (ushort)(c.Notes.Points[i].taps.value & rmask);
                        rtimes.Add(c.Notes.Points[i].Offset);
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
            c.Notes = new PointManager<GameplaySnap>(newPoints);
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

        public override int GetStatus(string data)
        {
            return 2;
        }

        public override string GetName(string data)
        {
            return "Manipulate";
        }
    }
}
