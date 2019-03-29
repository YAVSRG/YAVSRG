using System;
using System.Collections.Generic;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay.Charts.Stepmania
{
    //contains the lines from a .sm file corresponding to a musical bar (only 4 beats per bar supported in .sm format)
    public class Measure
    {
        private string[] data;

        public Measure(string[] rows)
        {
            data = rows;
        }

        //used by conversion in StepFile - see there for clearer explanation of use
        //given a List<Snap> output, it appends new snaps appropriately between "from" and "to", fractional beats dividing the measure
        //the input is normally always 0 and 4 but the ability to split it apart allows for mid-measure, mid-beat BPM changes.
        public void ConvertSection(double offset, double msPerBeat, BinarySwitcher lntracker, byte keys, double from, double to, byte meter, List<Snap> output)
        {
            float l = data.Length;
            double sep = msPerBeat * meter / l;
            int start = (int)Math.Ceiling(from * l / meter);
            int end = (int)Math.Ceiling(to * l / meter);
            offset += (start - from * l / meter) * sep;
            //offset += (((double)start * meter / l) - from) * msPerBeat;
            //WTF ^ this was the bugged line of code but i can't see what is mathematically different between it and the one in use
            //probably some typing/rounding issue and im blind

            for (int i = start; i < end; i++) //if start and end are the same no conversions occur
            {
                if (data[i] == "0000") { continue; } //optimisation won't work on non 4k but no idea how effective it is anyway
                Snap s = new Snap((float)(offset + (i - start) * sep), 0, 0, lntracker.value);
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
                }
            }
        }
    }
}
