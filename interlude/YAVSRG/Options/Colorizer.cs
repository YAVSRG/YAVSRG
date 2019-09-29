using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using Prelude.Gameplay;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Interlude.Options
{
    //todo: refactor as interface/class for each colorizer and move to prelude
    public class Colorizer
    {
        public static readonly int[] DDRValues = { 1, 2, 3, 4, 6, 8, 12, 16 };

        public enum ColorStyle
        {
            DDR,
            Column,
            Chord,
            Jackhammer,
            //LongNote,
            //Manipulate
        }

        public static void Colorize(ChartWithModifiers c, ColorScheme s)
        {
            switch (s.Style)
            {
                case (ColorStyle.DDR):
                    DDR(c, s);
                    return;
                case (ColorStyle.Column):
                    Column(c, s);
                    return;
                case (ColorStyle.Chord):
                    Chord(c, s);
                    return;
                case (ColorStyle.Jackhammer):
                    Jackhammer(c, s);
                    return;
            }
        }

        private static void Jackhammer(ChartWithModifiers c, ColorScheme cs)
        {
            float[] last = new float[c.Keys];
            GameplaySnap s;
            float v;
            float x;
            BPMPoint p;
            int color;
            for (int i = 0; i < c.Notes.Count; i++)
            {
                s = c.Notes.Points[i];
                foreach (int k in new BinarySwitcher(s.taps.value + s.holds.value).GetColumns())
                {
                    p = c.Timing.BPM.GetPointAt(s.Offset, false);
                    v = p.MSPerBeat;
                    x = (s.Offset - last[k] - 2);

                    color = DDRValues.Length;
                    for (int j = DDRValues.Length - 1; j >= 0; j--)
                    {
                        if (x < v / DDRValues[j])
                        {
                            color = j;
                            break;
                        }
                    }
                    s.colors[k] = cs.GetColorIndex(color, c.Keys);
                    last[k] = s.Offset;
                }
            }
        }

        private static void DDR(ChartWithModifiers c, ColorScheme cs)
        {
            //i'll comment it soon, but the code is likely to change around a bit
            float v;
            float x;
            BPMPoint p;
            int color;
            int[] prev = null;
            foreach (GameplaySnap s in c.Notes.Points)
            {
                p = c.Timing.BPM.GetPointAt(s.Offset, false);
                v = p.MSPerBeat;
                x = (s.Offset - p.Offset);

                color = DDRValues.Length;
                for (int i = 0; i < DDRValues.Length; i++)
                {
                    if (RoughlyDivisibleBy(x, v / DDRValues[i]))
                    {
                        color = i;
                        break;
                    }
                }
                for (byte k = 0; k < c.Keys; k++)
                {
                    s.colors[k] = cs.GetColorIndex(color,c.Keys);
                }
                if (prev != null)
                {
                    foreach (byte k in new BinarySwitcher(s.middles.value | (cs.LNEndsMatchBody ? s.ends.value : 0)).GetColumns())
                    {
                        s.colors[k] = prev[k];
                    }
                }
                prev = s.colors;
            }
        }

        private static void Column(ChartWithModifiers c, ColorScheme cs)
        {
            int[] prev = null;
            foreach (GameplaySnap s in c.Notes.Points)
            {
                s.colors = new int[c.Keys];
                for (int i = 0; i < c.Keys; i++)
                {
                    s.colors[i] = cs.GetColorIndex(i,c.Keys); //color notes based on column.
                }
                if (prev != null)
                {
                    foreach (byte k in new BinarySwitcher(s.middles.value | (cs.LNEndsMatchBody ? s.ends.value : 0)).GetColumns())
                    {
                        s.colors[k] = prev[k];
                    }
                }
                prev = s.colors;
            }
        }

        private static void Chord(ChartWithModifiers c, ColorScheme cs)
        {
            int color;
            int[] prev = null;
            foreach (GameplaySnap s in c.Notes.Points)
            {
                s.colors = new int[c.Keys];
                color = cs.GetColorIndex(Math.Max(0,new BinarySwitcher((ushort)(s.taps.value | s.holds.value)).Count - 1), c.Keys); //count number of lns/notes
                for (int i = 0; i < c.Keys; i++)
                {
                    s.colors[i] = color; //color notes in row based on number of notes. chord coloring.
                }
                if (prev != null)
                {
                    foreach (byte k in new BinarySwitcher(s.middles.value | (cs.LNEndsMatchBody ? s.ends.value : 0)).GetColumns())
                    {
                        s.colors[k] = prev[k];
                    }
                }
                prev = s.colors;
            }
        }

        private static bool RoughlyDivisibleBy(float a, float b)
        {
            return Math.Abs(a - b * Math.Round(a / b)) < 3;
        }
    }
}
