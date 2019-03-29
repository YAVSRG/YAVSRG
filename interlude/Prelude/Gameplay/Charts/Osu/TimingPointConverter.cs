using System.Collections.Generic;
using System.Linq;
using System.IO;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay.Charts.Osu
{
    public class TimingPointConverter
        //see also: HitObjectConverter, which is commented
    {
        private List<TimingPoint> points;

        public TimingPointConverter(TextReader fs)
        {
            points = new List<TimingPoint>();
            string l;
            while (true)
            {
                l = fs.ReadLine();
                if (l == "")
                {
                    return;
                }
                points.Add(new TimingPoint(l));
            }
        }

        public float GetMostCommonBPM(float end)
        {
            float current = points[0].msPerBeat; //should always be a normal timing point
            float t = points[0].offset;
            Dictionary<float, float> data = new Dictionary<float, float>();
            foreach (TimingPoint p in points)
            {
                if (!data.ContainsKey(current))
                {
                    data.Add(current, 0);
                }
                if (!p.inherited)
                {
                    data[current] += (p.offset - t); current = p.msPerBeat; t = p.offset;
                }
            }
            if (!data.ContainsKey(current))
            {
                data.Add(current, 0);
            }
            data[current] += (end - t);
            return data.OrderBy(pair => pair.Value).Last().Key; //should select bpm value that has the most time during the map
        }

        public void Convert(float end, SVManager output) //time of last note is used for most common bpm
        {
            List<BPMPoint> bpms = new List<BPMPoint>();
            List<SVPoint> sv = new List<SVPoint>();
            float bpm = 500;
            float basebpm = GetMostCommonBPM(end);
            float inherit = points[0].offset;
            int meter = 4;
            float scroll = 1.0f;
            SVPoint prev = null;
            foreach (TimingPoint point in points)
            {
                if (!point.inherited)
                {
                    meter = point.meter;
                    scroll = basebpm / point.msPerBeat;
                    bpm = point.msPerBeat;
                    inherit = point.offset;
                    bpms.Add(new BPMPoint(point.offset, meter, bpm));
                    sv.Add(prev = new SVPoint(point.offset, scroll));
                }
                else if (point.offset == prev?.Offset)
                {
                    prev.ScrollSpeed = scroll * (-100 / point.msPerBeat);
                }
                else
                {
                    sv.Add(new SVPoint(point.offset, scroll * (-100 / point.msPerBeat)));
                }
            }
            output.SetTimingData(bpms);
            output.SetSVData(-1, sv);
        }

        public void ConvertFromBPMPoints(SVManager input) //reverse process for above algorithm NYI (because it's harder)
        {
            //nyi
        }

        public void Dump(TextWriter tw)
        {
            //nyi
        }
    }
}
