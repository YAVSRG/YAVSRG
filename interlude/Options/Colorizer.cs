using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using YAVSRG.Beatmap;

namespace YAVSRG.Options
{
    class Colorizer
    {
        public enum ColorScheme
        {
            DDR,
            Column,
            Chord,
            LongNote,
            Jackhammer,
            Manipulate
        }

        public static void Colorize(Chart c, ColorScheme scheme)
        {
            DDR(c);
            //Chord(c);
        }

        private static void Jackhammer(Chart c)
        {
            int last = c.States.Points[0].taps.value;
            int current;
            for (int i = 1; i < c.States.Count; i++)
            {
                current = c.States.Points[i].taps.value;
                foreach (int k in new Snap.BinarySwitcher(current & last).GetColumns())
                {
                    c.States.Points[i - 1].colors[k] = 3;
                    c.States.Points[i].colors[k] += 1;
                }
                last = current;
            }
        }
        private static void DDR(Chart c)
        {
            int[] arr1 = new[] { 1, 2, 4, 8, 3, 6, 12 };
            float v;
            float x;
            BPMPoint p;
            int color;
            foreach (Snap s in c.States.Points)
            {
                p = c.Timing.GetPointAt(c.Timing.GetPointAt(s.Offset, false).InheritsFrom, false);
                v = p.MSPerBeat;
                x = (s.Offset - p.Offset) % v;

                color = 7;
                for (int i = 0; i < arr1.Length; i++)
                {
                    if (RoughlyDivisibleBy(x, v / arr1[i]))
                    {
                        color = i;
                        break;
                    }
                }
                for (int k = 0; k < c.Keys; k++)
                {
                    s.colors[k] = color;
                }
            }
        }

        private static void Column(Chart c)
        {
            foreach (Snap s in c.States.Points)
            {
                s.colors = new int[c.Keys];
                for (int i = 1; i < c.Keys; i++)
                {
                    s.colors[i] = i;
                }
            }
        }

        private static void Chord(Chart c)
        {
            int count;
            foreach (Snap s in c.States.Points)
            {
                s.colors = new int[c.Keys];
                count = new Snap.BinarySwitcher(s.taps.value | s.holds.value).Count - 1;
                for (int i = 0; i < c.Keys; i++)
                {
                    s.colors[i] = count;
                }
            }
        }

        private static bool RoughlyDivisibleBy(float a, float b)
        {
            var x = a % b;

            return Math.Min(Math.Abs(x),Math.Abs(b-x)) < 5;
        }
    }
}
