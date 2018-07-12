using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Charts.YAVSRG;

namespace YAVSRG.Charts.Stepmania
{
    public class Measure
    {
        private string[] data;

        public Measure(string[] rows)
        {
            data = rows;
        }

        public void ConvertSection(double offset, double msPerBeat, BinarySwitcher lntracker, byte keys, double from, double to, byte meter, List<Snap> output)
        {
            int l = data.Length;
            double sep = msPerBeat * meter / l;
            int end = (int)Math.Floor((to) * l / meter);
            int start = (int)Math.Ceiling((from) * l / meter);
            offset += ((start * meter / l) - from) * msPerBeat;
            for (int i = start; i < end; i++)
            {
                if (data[i] == "0000") { continue; }
                Snap s = new Snap((float)(offset + (i - start) * sep), 0, 0, lntracker.value, 0, 0);
                for (byte c = 0; c < keys; c++)
                {
                    if (data[i][c] == '1') { s.taps.SetColumn(c); }
                    else if (data[i][c] == 'M') { s.mines.SetColumn(c); }
                    else if (data[i][c] == '2') { s.holds.SetColumn(c); lntracker.SetColumn(c); }
                    else if (data[i][c] == '3') { s.ends.SetColumn(c); lntracker.RemoveColumn(c); }
                }
                if (!s.IsEmpty())
                {
                    output.Add(s);
                }
            }
        }
    }
}
