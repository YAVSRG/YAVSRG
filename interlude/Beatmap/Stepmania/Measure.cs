using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Beatmap.Stepmania
{
    public class Measure
    {
        private string[] data;

        public Measure(string[] rows)
        {
            data = rows;
        }

        public IEnumerable<Snap> ConvertBeat(double offset, double msPerBeat, Snap.BinarySwitcher lntracker, int keys, int beat, int meter)
        {
            int l = data.Length;
            double sep = msPerBeat * 4 / l;
            int end = (beat + 1) * l / meter;
            int start = (beat) * l / meter;
            for (int i = start; i < end; i++)
            {
                Snap s = new Snap((float)(offset+(i-start)*sep),0,0,lntracker.value,0);
                for (int c = 0; c < keys; c++)
                {
                    if (data[i][c] == '1') { s.taps.SetColumn(c); }
                    else if (data[i][c] == '2') { s.holds.SetColumn(c); lntracker.SetColumn(c); }
                    else if (data[i][c] == '3') { s.ends.SetColumn(c); lntracker.RemoveColumn(c); }
                }
                if (s.Count > 0) {
                    yield return s;
                }
            }
        }
    }
}
