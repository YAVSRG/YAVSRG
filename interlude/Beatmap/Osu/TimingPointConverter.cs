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

        public List<BPMPoint> Convert()
        {
            List<BPMPoint> tp = new List<BPMPoint>();
            float bpm = 125;
            float basebpm = points[0].msPerBeat;
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
