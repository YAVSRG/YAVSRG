using System;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay.Mods
{
    public class Inverse : Mod
    {
        public override void Apply(ChartWithModifiers c, string data)
        {
            base.Apply(c, data);
            PointManager<GameplaySnap> newSnaps = new PointManager<GameplaySnap>();
            int count = c.Notes.Count;
            GameplaySnap s,n;

            for (int i = 0; i < count; i++)
            {
                s = c.Notes.Points[i];
                if (i > 0)
                {
                    n = newSnaps.GetPointAt(s.Offset, true);
                    n.holds.value = s.taps.value;
                    n.taps.value = s.holds.value;
                }
                else
                {
                    n = new GameplaySnap(s.Offset, 0, 0, 0, 0, 0);
                }
                if (i > 0)
                {
                    GameplaySnap temp2 = newSnaps.Points[newSnaps.Count - 1]; //last snap
                    GameplaySnap temp = (GameplaySnap)temp2.Interpolate(s.Offset - GetGapSize(c, s.Offset));//newSnaps.GetPointAt(s.Offset - GetGapSize(c, s.Offset), true); //find state to form gaps
                    foreach (byte k in s.taps.GetColumns()) //all taps
                    {
                        n.holds.SetColumn(k); //turn to start of lns
                        if (n.middles.GetColumn(k)) //if already holding make a gap
                        {
                            n.middles.RemoveColumn(k);
                            temp.middles.RemoveColumn(k);
                            temp.ends.SetColumn(k);
                        }
                    }

                    if (temp.Offset < temp2.Offset + 10) //replace with tap note when too close
                    {
                        foreach (byte k in temp.ends.GetColumns())
                        {
                            if (temp2.holds.GetColumn(k))
                            {
                                temp.ends.RemoveColumn(k);
                                temp2.holds.RemoveColumn(k);
                                temp2.taps.SetColumn(k);
                            }
                        }
                    }

                    if (temp.ends.value > 0) //store the gap if it was made
                    {
                        newSnaps.AppendPoint(temp);
                    }
                    //algo needed for notes closer than 1/8 snap together
                    foreach (byte k in s.holds.GetColumns()) //all taps
                    {
                        if (n.middles.GetColumn(k))
                        {
                            n.middles.RemoveColumn(k);
                            n.ends.SetColumn(k);
                        }
                        else
                        {
                            n.taps.SetColumn(k);
                        }
                    }
                    n.holds.value += s.ends.value; //things that break this should never happen
                }
                newSnaps.AppendPoint(n);
            }
            s = newSnaps.Points[newSnaps.Count - 1]; //finalise
            s.ends.value += s.middles.value;
            s.middles.value = 0;
            c.Notes = newSnaps;
        }

        float GetGapSize(ChartWithModifiers c, float time)
        {
            return c.Timing.BPM.GetPointAt(time, false).MSPerBeat / 4;
        }
        //replace tap with start AND place end IF middle <- if end is before start, put tap back
        //replace start with end IF middle
        //remove middles (implicitly)
        //replace ends with starts

        public override string GetName(string data)
        {
            return "Inverse (not complete!)";
        }
    }
}
