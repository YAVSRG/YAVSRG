using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using YAVSRG.Charts.YAVSRG;

namespace YAVSRG.Charts.Osu
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

        public float GetMostCommonBPM(float end) //this doesn't work and needs to be fixeddd
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

        public List<BPMPoint> Convert(float end)
        {
            List<BPMPoint> tp = new List<BPMPoint>();
            float bpm = 500;
            float basebpm = GetMostCommonBPM(end);
            float inherit = points[0].offset;
            int meter = 4;
            float scroll = 1.0f;
            BPMPoint prev = null;
            foreach (TimingPoint point in points)
            {
                if (!point.inherited)
                {
                    meter = point.meter;
                    scroll = basebpm / point.msPerBeat;
                    bpm = point.msPerBeat;
                    inherit = point.offset;
                    tp.Add(prev = new BPMPoint(point.offset, meter, bpm, scroll, point.offset));
                }
                else if (point.offset == prev?.Offset) //fix for when red and green line are on top of each other
                {
                    prev.ScrollSpeed *= (-100 / point.msPerBeat);
                }
                else
                {
                    tp.Add(new BPMPoint(point.offset, meter, bpm, scroll * (-100 / point.msPerBeat), inherit));
                }
            }
            return tp;
        }

        public void ConvertFromBPMPoints() //reverse process for above algorithm NYI (because it's harder)
        {
            //nyi
        }

        public void Dump(TextWriter tw)
        {
            //nyi
        }
    }
}
