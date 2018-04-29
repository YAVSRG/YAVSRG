using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace YAVSRG.Beatmap
{
    public class TimingPointConverter
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
            float current = points[0].msPerBeat;
            float t = 0;
            Dictionary<float, float> data = new Dictionary<float, float>();
            foreach (TimingPoint p in points)
            {
                if (data.ContainsKey(current))
                {
                    data[current] += (p.offset - t);
                }
                else
                {
                    data.Add(current, p.offset - t);
                }
                if (!p.inherited) { current = p.msPerBeat; t = p.offset; }
            }
            if (data.ContainsKey(current))
            {
                data[current] += (end - t);
            }
            else
            {
                data.Add(current, end - t);
            }
            return data.OrderBy(pair => pair.Value).First().Key;
        }

        public List<BPMPoint> Convert(float end)
        {
            List<BPMPoint> tp = new List<BPMPoint>();
            float bpm = 500;
            float basebpm = GetMostCommonBPM(end);
            float inherit = points[0].offset;
            int meter = 4;
            float scroll = 1.0f;
            foreach (TimingPoint point in points)
            {
                if (!point.inherited)
                {
                    meter = point.meter;
                    scroll = basebpm / point.msPerBeat;
                    bpm = point.msPerBeat;
                    inherit = point.offset;
                    tp.Add(new BPMPoint(point.offset, meter, bpm, scroll, point.offset));
                }
                else
                {
                    tp.Add(new BPMPoint(point.offset, meter, bpm, scroll * (-100 / point.msPerBeat), inherit));
                }
            }
            return tp;
        }

        public void Dump(TextWriter tw)
        {

        }
    }
}
