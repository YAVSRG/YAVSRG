using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Charts.Osu
{
    public class TimingPoint
    {
        public float offset;
        public float msPerBeat;
        public int meter, sampleType, sampleSet, volume;
        public bool inherited, kiai;

        public TimingPoint(float offset, float msPerBeat, int meter, int sampleType, int sampleSet, int volume, bool inherited, bool kiai)
        {
            this.offset = offset;
            this.msPerBeat = msPerBeat;
            this.meter = meter;
            this.sampleSet = sampleSet;
            this.sampleType = sampleType;
            this.volume = volume;
            this.inherited = inherited;
            this.kiai = kiai;
        }

        public TimingPoint(string parse)
        {
            string[] parts = parse.Split(',');

            offset = float.Parse(parts[0]);
            msPerBeat = float.Parse(parts[1]);
            meter = int.Parse(parts[2]);
            sampleSet = int.Parse(parts[3]);
            sampleType = int.Parse(parts[4]);
            volume = int.Parse(parts[5]);
            inherited = int.Parse(parts[6]) == 0;
            kiai = int.Parse(parts[7]) == 1;
        }

        public float ScrollSpeed()
        {
            if (inherited)
            {
                return -100 / msPerBeat;
            }
            return 1f;
        }

        public TimingPoint GetChildPoint(float offset)
        {
            return new TimingPoint(offset, -100, meter, sampleSet, sampleSet, volume, true, kiai);
        }

        public void Dump(System.IO.TextWriter tw)
        {

        }
    }
}
