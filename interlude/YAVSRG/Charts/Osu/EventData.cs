using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace YAVSRG.Charts.Osu
{
    public class EventData
    {
        private List<StoryboardEvent> points;

        public EventData(TextReader fs)
        {
            points = new List<StoryboardEvent>();
            string l;
            while (true)
            {
                l = fs.ReadLine();
                if (l == "") return;
                if (l.StartsWith("//")) continue;
                points.Add(new StoryboardEvent(l));
            }
        }

        public string GetBGPath()
        {
            foreach (StoryboardEvent s in points)
            {
                if (s.data[0] == "0")
                {
                    return s.data[2].Trim('"');
                }
            }
            return "";
        }

        public void Dump(TextWriter tw)
        {

        }
    }
}
