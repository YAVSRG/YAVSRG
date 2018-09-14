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
            float l = data.Length;
            double sep = msPerBeat * meter / l;
            int start = (int)Math.Ceiling(from * l / meter);
            int end = (int)Math.Ceiling(to * l / meter);
            //Utilities.Logging.Log(start.ToString() + "|" + end.ToString());
            offset += (((double)start * meter / l) - from) * msPerBeat;
            for (int i = start; i < end; i++) //if start and end are the same no conversions occur
            {
                if (data[i] == "0000") { continue; } //optimisation won't work on non 4k but no idea how effective it is anyway
                Snap s = new Snap((float)(offset + (i - start) * sep), 0, 0, lntracker.value, 0, 0);
                for (byte c = 0; c < keys; c++)
                {
                    //no support for fakes (yet(?))
                    if (data[i][c] == '1') { s.taps.SetColumn(c); }
                    else if (data[i][c] == 'M') { s.mines.SetColumn(c); }
                    else if (data[i][c] == '2' || data[i][c] == '4') { s.holds.SetColumn(c); lntracker.SetColumn(c); }
                    else if (data[i][c] == '3') { s.ends.SetColumn(c); s.middles.RemoveColumn(c); lntracker.RemoveColumn(c); }
                }
                if (!s.IsEmpty())
                {
                    output.Add(s);
                    //Utilities.Logging.Log(s.Offset.ToString());
                }
            }
        }
    }
}
